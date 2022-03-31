use super::log_wasm;
use std::path::PathBuf;

const WASM_MUTATE_FUEL: u64 = 1_000;

fn validate(wasm: &[u8]) -> Result<(), impl std::error::Error> {
    let mut validator = wasmparser::Validator::new();
    validator.wasm_features(wasmparser::WasmFeatures {
        mutable_global: true,
        saturating_float_to_int: true,
        sign_extension: true,
        reference_types: true,
        multi_value: true,
        bulk_memory: true,
        simd: true,
        multi_memory: true,

        // Not supported by `wasm-mutate` yet.
        exceptions: false,
        relaxed_simd: false,
        threads: false,
        tail_call: false,
        memory64: false,
        extended_const: false,

        // Will replace with component model soon.
        module_linking: false,

        // Will never enable this.
        deterministic_only: false,
    });
    validator.validate_all(wasm)
}

/// Use `wasm-mutate` to produce a sequence of semantically-equivalent mutated
/// variants of the given input Wasm and assert that they all evaluate the same
/// as the original.
pub fn mutate_differential(wasm: &[u8]) {
    log_wasm(wasm);

    if let Err(e) = validate(wasm) {
        log::warn!("input is not valid Wasm: {}", e);
        return;
    }

    // We want to use different seeds with the same Wasm input, which leads to
    // choosing different expressions or entities to mutate in the same Wasm
    // input. But at the same time, we don't want to use `Arbitrary` here
    // because we want the fuzzer's input to be exactly a Wasm file so that we
    // can easily seed the corpus with interesting Wasm files rather than try
    // and reverse a derived `Arbitrary` implementation. So we hash the Wasm's
    // custom section(s) to get the seed. The custom sections do not affect
    // semantics, so libfuzzer can mutate them, leading to a new hash of the
    // custom sections, and then we end up testing the "same" Wasm input with a
    // new seed. A bit of a hack, but it works out surprisingly well for our
    // needs here.
    let seed = hash_custom_sections(wasm).unwrap();

    // Mutate the Wasm with `wasm-mutate`. Assert that each mutation is still
    // valid Wasm.

    let mut wasm_mutate = wasm_mutate::WasmMutate::default();
    wasm_mutate.seed(seed);
    wasm_mutate.fuel(WASM_MUTATE_FUEL);
    wasm_mutate.preserve_semantics(true);

    let iterator = match wasm_mutate.run(&wasm) {
        Ok(iterator) => iterator,
        Err(e) => {
            log::warn!("Failed to mutate the Wasm: {}", e);
            return;
        }
    };

    for (i, mutated_wasm) in iterator.take(10).enumerate() {
        let mutated_wasm = match mutated_wasm {
            Ok(w) => w,
            Err(e) => match e.kind() {
                wasm_mutate::ErrorKind::NoMutationsApplicable => continue,
                _ => panic!("Unexpected mutation failure: {}", e),
            },
        };

        let validation_result = validate(&mutated_wasm);
        log::debug!("validation result = {:?}", validation_result);

        if log::log_enabled!(log::Level::Debug) {
            let wasm_path = PathBuf::from(format!("mutated{}.wasm", i));
            log::debug!("writing mutated Wasm to `{}`", wasm_path.display());
            drop(std::fs::write(&wasm_path, &mutated_wasm));
            if let Ok(mutated_wat) = wasmprinter::print_bytes(&mutated_wasm) {
                let wat_path = PathBuf::from(format!("mutated{}.wat", i));
                log::debug!("writing mutated WAT to `{}`", wat_path.display());
                drop(std::fs::write(&wat_path, &mutated_wat));
            }
        }
        assert!(
            validation_result.is_ok(),
            "`wasm-mutate` should always produce a valid mutated Wasm file when given a valid \
             input Wasm; got {:?}",
            validation_result
        );

        eval::assert_same_evaluation(&wasm, &mutated_wasm);
    }
}

fn hash_custom_sections(input_wasm: &[u8]) -> Result<u64, ()> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    use wasmparser::{Chunk, Parser, Payload};

    let mut hasher = DefaultHasher::new();
    let mut wasm = input_wasm;
    let mut parser = Parser::new(0);

    loop {
        let (payload, consumed) = match parser.parse(wasm, true) {
            Err(_) | Ok(Chunk::NeedMoreData(_)) => return Err(()),
            Ok(Chunk::Parsed { consumed, payload }) => (payload, consumed),
        };
        match payload {
            Payload::End => break,

            // Hash custom sections.
            Payload::CustomSection { name, data, .. } => {
                <str as Hash>::hash(name, &mut hasher);
                <[u8] as Hash>::hash(data, &mut hasher);
            }

            // Skip parsing the code section.
            Payload::CodeSectionStart { range, .. } => {
                parser.skip_section();
                wasm = &input_wasm[range.end..];
                continue;
            }

            // Module linking isn't supported.
            Payload::ModuleSectionStart { .. } => return Err(()),

            _ => {}
        }
        wasm = &wasm[consumed..];
    }

    Ok(hasher.finish())
}

mod eval {
    use super::super::dummy;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    /// Compile, instantiate, and evaluate both the original and mutated Wasm.
    ///
    /// We should get identical results because we told `wasm-mutate` to preserve
    /// semantics.
    pub fn assert_same_evaluation(wasm: &[u8], mutated_wasm: &[u8]) {
        let mut config = wasmtime::Config::default();
        config.cranelift_nan_canonicalization(true);
        config.consume_fuel(true);

        let engine = wasmtime::Engine::new(&config).unwrap();

        let (orig_module, mutated_module) = match (
            wasmtime::Module::new(&engine, &wasm),
            wasmtime::Module::new(&engine, &mutated_wasm),
        ) {
            (Ok(o), Ok(m)) => (o, m),
            // Ideally we would assert that they both errored if either one did, but
            // it is possible that a mutation bumped some count above/below an
            // implementation limit.
            (_, _) => return,
        };

        let mut store = wasmtime::Store::new(&engine, ());
        let (orig_imports, mutated_imports) = match dummy::dummy_imports(&mut store, &orig_module) {
            Ok(imps) => (imps.clone(), imps),
            Err(_) => return,
        };

        let (orig_instance, mutated_instance) = match (
            wasmtime::Instance::new(&mut store, &orig_module, &orig_imports),
            wasmtime::Instance::new(&mut store, &mutated_module, &mutated_imports),
        ) {
            (Ok(x), Ok(y)) => (x, y),
            (_, _) => return,
        };

        assert_same_state(&mut store, &orig_module, orig_instance, mutated_instance);
        assert_same_calls(&mut store, &orig_module, orig_instance, mutated_instance);
        assert_same_state(&mut store, &orig_module, orig_instance, mutated_instance);
    }

    fn assert_same_state(
        store: &mut wasmtime::Store<()>,
        orig_module: &wasmtime::Module,
        orig_instance: wasmtime::Instance,
        mutated_instance: wasmtime::Instance,
    ) {
        for export in orig_module.exports() {
            match export.ty() {
                wasmtime::ExternType::Global(_) => {
                    let orig = orig_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_global()
                        .unwrap()
                        .get(&mut *store);
                    let mutated = mutated_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_global()
                        .unwrap()
                        .get(&mut *store);
                    assert_val_eq(&orig, &mutated);
                }
                wasmtime::ExternType::Memory(_) => {
                    let orig = orig_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_memory()
                        .unwrap();
                    let mut h = DefaultHasher::default();
                    orig.data(&store).hash(&mut h);
                    let orig = h.finish();
                    let mutated = mutated_instance
                        .get_export(&mut *store, export.name())
                        .unwrap()
                        .into_memory()
                        .unwrap();
                    let mut h = DefaultHasher::default();
                    mutated.data(&store).hash(&mut h);
                    let mutated = h.finish();
                    assert_eq!(orig, mutated, "original and mutated Wasm memories diverged");
                }
                _ => continue,
            }
        }
    }

    fn assert_same_calls(
        store: &mut wasmtime::Store<()>,
        orig_module: &wasmtime::Module,
        orig_instance: wasmtime::Instance,
        mutated_instance: wasmtime::Instance,
    ) {
        for export in orig_module.exports() {
            match export.ty() {
                wasmtime::ExternType::Func(func_ty) => {
                    let orig_func = orig_instance.get_func(&mut *store, export.name()).unwrap();
                    let mutated_func = mutated_instance
                        .get_func(&mut *store, export.name())
                        .unwrap();
                    let args = dummy::dummy_values(func_ty.params());
                    let mut orig_rets = vec![wasmtime::Val::I32(0); func_ty.results().len()];
                    let mut mutated_rets = vec![wasmtime::Val::I32(0); func_ty.results().len()];
                    match (
                        {
                            store.add_fuel(1_000).unwrap();
                            orig_func.call(&mut *store, &args, &mut orig_rets)
                        },
                        {
                            let consumed = store.fuel_consumed().unwrap();
                            store.add_fuel(consumed).unwrap();
                            mutated_func.call(&mut *store, &args, &mut mutated_rets)
                        },
                    ) {
                        (Ok(()), Ok(())) => {
                            assert_eq!(orig_rets.len(), mutated_rets.len());
                            for (orig_val, mutated_val) in orig_rets.iter().zip(mutated_rets.iter())
                            {
                                assert_val_eq(orig_val, mutated_val);
                            }
                        }
                        (Err(_), Err(_)) => continue,
                        (orig, mutated) => panic!(
                            "mutated and original Wasm diverged: orig = {:?}; mutated = {:?}",
                            orig, mutated,
                        ),
                    }
                }
                _ => continue,
            }
        }
    }

    fn assert_val_eq(orig_val: &wasmtime::Val, mutated_val: &wasmtime::Val) {
        match (orig_val, mutated_val) {
            (wasmtime::Val::I32(o), wasmtime::Val::I32(m)) => assert_eq!(o, m),
            (wasmtime::Val::I64(o), wasmtime::Val::I64(m)) => assert_eq!(o, m),

            (wasmtime::Val::F32(o), wasmtime::Val::F32(m)) => {
                let o = f32::from_bits(*o);
                let m = f32::from_bits(*m);
                if o.is_nan() && m.is_nan() {
                    return;
                }
                assert_eq!(o, m);
            }
            (wasmtime::Val::F64(o), wasmtime::Val::F64(m)) => {
                let o = f64::from_bits(*o);
                let m = f64::from_bits(*m);
                if o.is_nan() && m.is_nan() {
                    return;
                }
                assert_eq!(o, m);
            }

            (o, m) => panic!(
                "mutated and original Wasm diverged: orig = {:?}; mutated = {:?}",
                o, m,
            ),
        }
    }
}
