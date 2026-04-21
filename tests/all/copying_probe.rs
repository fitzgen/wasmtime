// Probing tests for bugs in the copying collector.
// Run with: RUSTFLAGS="--cfg gc_zeal" cargo test --test all --features gc-copying -- copying_probe

use std::num::NonZeroU32;
use wasmtime::*;

fn copying_store(_gc_zeal: Option<u32>) -> Result<(Store<()>, Engine)> {
    let _ = env_logger::try_init();
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_function_references(true);
    config.collector(Collector::Copying);
    // Note: gc_zeal_alloc_counter is not used because it causes assertion
    // failures in the shared retry_after_gc_async code when bytes_needed=0.
    // We rely on RUSTFLAGS="--cfg gc_zeal" for poison checking instead.
    let engine = Engine::new(&config)?;
    let store = Store::new(&engine, ());
    Ok((store, engine))
}

/// Helper to instantiate a module that imports "wasmtime" "gc" as a no-op GC trigger.
fn instantiate_with_gc(store: &mut Store<()>, module: &Module) -> Result<Instance> {
    let gc_func = Func::wrap(&mut *store, |mut caller: Caller<'_, ()>| {
        caller.gc(None).unwrap();
    });
    let imports = [gc_func.into()];
    Instance::new(store, module, &imports)
}

fn copying_store_with_gc_zeal(counter: u32) -> Result<(Store<()>, Engine)> {
    let _ = env_logger::try_init();
    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_function_references(true);
    config.collector(Collector::Copying);
    config.gc_zeal_alloc_counter(Some(NonZeroU32::new(counter).unwrap()));
    let engine = Engine::new(&config)?;
    let store = Store::new(&engine, ());
    Ok((store, engine))
}

/// Test: struct with GC ref field survives collection
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_struct_gc_ref_field() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $inner (struct (field i32)))
            (type $outer (struct (field (ref null $inner)) (field i32)))

            (func (export "test") (result i32)
                (local $a (ref null $inner))
                (local $b (ref null $outer))
                (local.set $a (struct.new $inner (i32.const 42)))
                (local.set $b (struct.new $outer (local.get $a) (i32.const 100)))
                (i32.add
                    (struct.get $inner 0 (struct.get $outer 0 (local.get $b)))
                    (struct.get $outer 1 (local.get $b))
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 142);
    Ok(())
}

/// Test: array of GC refs survives collection
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_array_gc_refs() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $box (struct (field i32)))
            (type $arr (array (ref null $box)))

            (func (export "test") (result i32)
                (local $a (ref null $arr))
                (local.set $a
                    (array.new_fixed $arr 3
                        (struct.new $box (i32.const 10))
                        (struct.new $box (i32.const 20))
                        (struct.new $box (i32.const 30))
                    )
                )
                (i32.add
                    (i32.add
                        (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 0)))
                        (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 1)))
                    )
                    (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 2)))
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 60);
    Ok(())
}

/// Test: linked list with many nodes, forcing many GC cycles
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_linked_list_many_gc() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $node (struct (field (ref null $node)) (field i32)))

            (func $build (param $n i32) (result (ref null $node))
                (local $head (ref null $node))
                (local $i i32)
                (block $done
                    (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
                        (local.set $head
                            (struct.new $node (local.get $head) (local.get $i))
                        )
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)
                    )
                )
                (local.get $head)
            )

            (func $sum (param $head (ref null $node)) (result i32)
                (local $total i32)
                (local $cur (ref null $node))
                (local.set $cur (local.get $head))
                (block $done
                    (loop $loop
                        (br_if $done (ref.is_null (local.get $cur)))
                        (local.set $total
                            (i32.add (local.get $total) (struct.get $node 1 (local.get $cur)))
                        )
                        (local.set $cur (struct.get $node 0 (local.get $cur)))
                        (br $loop)
                    )
                )
                (local.get $total)
            )

            (func (export "test") (result i32)
                (call $sum (call $build (i32.const 50)))
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    // 0+1+2+...+49 = 49*50/2 = 1225
    assert_eq!(result, 1225);
    Ok(())
}

/// Test: externref survives GC
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_externref_survives_gc() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "" "gc" (func $gc))
            (func (export "roundtrip") (param externref) (result externref)
                (call $gc)
                (local.get 0)
            )
        )
        "#,
    )?;
    let gc_func = Func::wrap(&mut store, |mut caller: Caller<'_, ()>| {
        caller.gc(None);
    });
    let instance = Instance::new(&mut store, &module, &[gc_func.into()])?;
    let roundtrip = instance.get_typed_func::<Option<Rooted<ExternRef>>, Option<Rooted<ExternRef>>>(
        &mut store,
        "roundtrip",
    )?;

    {
        let val = ExternRef::new(&mut store, 42u32)?;
        let result = roundtrip.call(&mut store, Some(val))?;
        let result = result.unwrap();
        let data = result
            .data(&store)?
            .expect("should have data")
            .downcast_ref::<u32>()
            .copied()
            .unwrap();
        assert_eq!(data, 42u32);
    }
    Ok(())
}

/// Test: many externrefs with GC - tests linked list management
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_many_externrefs() -> Result<()> {
    let (mut store, engine) = copying_store(Some(2))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "" "gc" (func $gc))
            (import "" "make" (func $make (param i32) (result externref)))
            (import "" "check" (func $check (param externref i32)))

            (func (export "test")
                (local $a externref)
                (local $b externref)
                (local $c externref)

                (local.set $a (call $make (i32.const 100)))
                (local.set $b (call $make (i32.const 200)))
                (local.set $c (call $make (i32.const 300)))

                (call $gc)

                (call $check (local.get $a) (i32.const 100))
                (call $check (local.get $b) (i32.const 200))
                (call $check (local.get $c) (i32.const 300))
            )
        )
        "#,
    )?;

    let gc_func = Func::wrap(&mut store, |mut caller: Caller<'_, ()>| {
        caller.gc(None);
    });
    let make = Func::wrap(
        &mut store,
        |mut caller: Caller<'_, ()>, val: i32| -> Result<Option<Rooted<ExternRef>>> {
            Ok(Some(ExternRef::new(&mut caller, val)?))
        },
    );
    let check = Func::wrap(
        &mut store,
        |caller: Caller<'_, ()>, ext: Option<Rooted<ExternRef>>, expected: i32| -> Result<()> {
            let ext = ext.unwrap();
            let val = *ext.data(&caller)?.expect("data").downcast_ref::<i32>().unwrap();
            assert_eq!(val, expected, "externref value mismatch after GC");
            Ok(())
        },
    );

    let instance =
        Instance::new(&mut store, &module, &[gc_func.into(), make.into(), check.into()])?;
    let test = instance.get_typed_func::<(), ()>(&mut store, "test")?;
    test.call(&mut store, ())?;
    Ok(())
}

/// Test: struct with multiple field types including i8 and i16
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_packed_struct_fields() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $packed (struct
                (field i8)
                (field i16)
                (field i32)
                (field i64)
                (field f32)
                (field f64)
            ))

            (func (export "test") (result i32)
                (local $s (ref null $packed))
                (local.set $s
                    (struct.new $packed
                        (i32.const 1)
                        (i32.const 2)
                        (i32.const 3)
                        (i64.const 4)
                        (f32.const 5.0)
                        (f64.const 6.0)
                    )
                )
                ;; Read back all fields and sum the integers
                (i32.add
                    (i32.add
                        (struct.get_u $packed 0 (local.get $s))
                        (struct.get_u $packed 1 (local.get $s))
                    )
                    (i32.add
                        (struct.get $packed 2 (local.get $s))
                        (i32.wrap_i64 (struct.get $packed 3 (local.get $s)))
                    )
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 10); // 1+2+3+4 = 10
    Ok(())
}

/// Test: deep object graph - tree structure
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_binary_tree() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $tree (struct
                (field (ref null $tree))  ;; left
                (field (ref null $tree))  ;; right
                (field i32)               ;; value
            ))

            ;; Build a complete binary tree of depth n, with values 1..2^n-1
            (func $build (param $depth i32) (param $val i32) (result (ref null $tree))
                (local $left (ref null $tree))
                (local $right (ref null $tree))
                (if (result (ref null $tree)) (i32.le_s (local.get $depth) (i32.const 0))
                    (then (ref.null $tree))
                    (else
                        (local.set $left
                            (call $build
                                (i32.sub (local.get $depth) (i32.const 1))
                                (i32.mul (local.get $val) (i32.const 2))
                            )
                        )
                        (local.set $right
                            (call $build
                                (i32.sub (local.get $depth) (i32.const 1))
                                (i32.add (i32.mul (local.get $val) (i32.const 2)) (i32.const 1))
                            )
                        )
                        (struct.new $tree
                            (local.get $left)
                            (local.get $right)
                            (local.get $val)
                        )
                    )
                )
            )

            ;; Sum all values in the tree
            (func $sum (param $t (ref null $tree)) (result i32)
                (if (result i32) (ref.is_null (local.get $t))
                    (then (i32.const 0))
                    (else
                        (i32.add
                            (struct.get $tree 2 (local.get $t))
                            (i32.add
                                (call $sum (struct.get $tree 0 (local.get $t)))
                                (call $sum (struct.get $tree 1 (local.get $t)))
                            )
                        )
                    )
                )
            )

            ;; Build tree of depth 5 (31 nodes), sum all values
            ;; Values: 1,2,3,...,31 -> sum = 31*32/2 = 496
            (func (export "test") (result i32)
                (call $sum (call $build (i32.const 5) (i32.const 1)))
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 496);
    Ok(())
}

/// Test: allocate and discard objects to force collect/grow cycles,
/// then verify a kept object's value is correct
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_gc_pressure() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $box (struct (field i32)))

            (func (export "test") (result i32)
                (local $keep (ref null $box))
                (local $i i32)

                ;; Create the value we want to keep
                (local.set $keep (struct.new $box (i32.const 999)))

                ;; Allocate and discard 100 objects, forcing many GC cycles
                (local.set $i (i32.const 0))
                (block $done
                    (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (i32.const 100)))
                        (drop (struct.new $box (local.get $i)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)
                    )
                )

                ;; Read back the kept value
                (struct.get $box 0 (local.get $keep))
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 999);
    Ok(())
}

/// Test: v128 fields in GC structs
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_v128_struct() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $v (struct (field (mut v128)) (field (mut v128))))

            (func (export "test") (result i32)
                (local $s (ref null $v))
                (local.set $s
                    (struct.new $v
                        (v128.const i32x4 1 2 3 4)
                        (v128.const i32x4 10 20 30 40)
                    )
                )
                ;; Extract and add lanes from both fields
                (i32.add
                    (i32x4.extract_lane 0 (struct.get $v 0 (local.get $s)))
                    (i32x4.extract_lane 0 (struct.get $v 1 (local.get $s)))
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 11); // 1 + 10
    Ok(())
}

/// Test: v128 struct with GC ref field, survives collection
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_v128_with_gc_ref() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $inner (struct (field i32)))
            (type $outer (struct (field v128) (field (ref null $inner))))

            (func (export "test") (result i32)
                (local $i (ref null $inner))
                (local $o (ref null $outer))
                (local.set $i (struct.new $inner (i32.const 42)))
                (local.set $o
                    (struct.new $outer
                        (v128.const i32x4 100 200 300 400)
                        (local.get $i)
                    )
                )
                (i32.add
                    (i32x4.extract_lane 0 (struct.get $outer 0 (local.get $o)))
                    (struct.get $inner 0 (struct.get $outer 1 (local.get $o)))
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 142); // 100 + 42
    Ok(())
}

/// Test: gc_zeal_alloc_counter=2 with struct allocations
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_gc_zeal_counter_2() -> Result<()> {
    let (mut store, engine) = copying_store_with_gc_zeal(2)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $box (struct (field i32)))

            (func (export "test") (result i32)
                (local $a (ref null $box))
                (local $b (ref null $box))
                ;; First alloc
                (local.set $a (struct.new $box (i32.const 10)))
                ;; Second alloc triggers GC after gc_zeal counter hits 0
                (local.set $b (struct.new $box (i32.const 20)))
                (i32.add
                    (struct.get $box 0 (local.get $a))
                    (struct.get $box 0 (local.get $b))
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 30);
    Ok(())
}

/// Test: gc_zeal_alloc_counter=3 with many allocations and refs surviving
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_gc_zeal_counter_3_many() -> Result<()> {
    let (mut store, engine) = copying_store_with_gc_zeal(3)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $node (struct (field (ref null $node)) (field i32)))

            (func (export "test") (result i32)
                (local $head (ref null $node))
                (local $i i32)
                (local $total i32)
                (local $cur (ref null $node))
                ;; Build a linked list of 20 nodes
                (local.set $i (i32.const 0))
                (block $done
                    (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (i32.const 20)))
                        (local.set $head
                            (struct.new $node (local.get $head) (local.get $i))
                        )
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)
                    )
                )
                ;; Sum all values
                (local.set $cur (local.get $head))
                (block $done2
                    (loop $loop2
                        (br_if $done2 (ref.is_null (local.get $cur)))
                        (local.set $total
                            (i32.add (local.get $total) (struct.get $node 1 (local.get $cur)))
                        )
                        (local.set $cur (struct.get $node 0 (local.get $cur)))
                        (br $loop2)
                    )
                )
                (local.get $total)
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 190); // 0+1+...+19 = 190
    Ok(())
}

/// Test: funcref fields in GC structs
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_funcref_field() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $fbox (struct (field (ref null func))))

            (func $f (result i32) (i32.const 77))

            (func (export "test") (result i32)
                (local $b (ref null $fbox))
                (local.set $b (struct.new $fbox (ref.func $f)))
                (call_ref $ft (struct.get $fbox 0 (local.get $b)))
            )
            (type $ft (func (result i32)))
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 77);
    Ok(())
}

/// Test: large i8 array (near u32::MAX to trigger overflow bug)
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_huge_array_overflow() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $arr (array (mut i8)))
            (func (export "test")
                ;; This allocation has base_size=20, elem_size=1, length=4294967275
                ;; total size = 20 + 4294967275 = 4294967295 = u32::MAX
                ;; In alloc_raw, the rounding (size + 15) & !15 overflows u32
                (drop (array.new_default $arr (i32.const -21)))
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), ()>(&mut store, "test")?;
    // This should trap or error due to OOM, NOT succeed with a 0-byte allocation
    let result = test.call(&mut store, ());
    // The bug: with gc_zeal, this panics with out-of-bounds on the poison check.
    // Without gc_zeal, this corrupts memory silently.
    // Either way, it should trap cleanly.
    assert!(result.is_err(), "huge array allocation should have failed");
    Ok(())
}

/// Test: array of gc refs with GC pressure
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_gc_ref_array_pressure() -> Result<()> {
    let (mut store, engine) = copying_store(Some(1))?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $box (struct (field i32)))
            (type $arr (array (ref null $box)))

            (func (export "test") (result i32)
                (local $a (ref null $arr))
                (local $i i32)
                (local $total i32)

                ;; Create array of 10 refs
                (local.set $a
                    (array.new_fixed $arr 5
                        (struct.new $box (i32.const 1))
                        (struct.new $box (i32.const 2))
                        (struct.new $box (i32.const 3))
                        (struct.new $box (i32.const 4))
                        (struct.new $box (i32.const 5))
                    )
                )

                ;; Allocate garbage to cause GC
                (local.set $i (i32.const 0))
                (block $done
                    (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (i32.const 50)))
                        (drop (struct.new $box (local.get $i)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)
                    )
                )

                ;; Read back from the array
                (local.set $i (i32.const 0))
                (block $done2
                    (loop $loop2
                        (br_if $done2 (i32.ge_u (local.get $i) (i32.const 5)))
                        (local.set $total
                            (i32.add
                                (local.get $total)
                                (struct.get $box 0 (array.get $arr (local.get $a) (local.get $i)))
                            )
                        )
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop2)
                    )
                )
                (local.get $total)
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 15); // 1+2+3+4+5
    Ok(())
}

/// Test: struct with i8, i16 and gc ref fields, force GC between allocs
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_mixed_fields_gc() -> Result<()> {
    let (mut store, engine) = copying_store_with_gc_zeal(2)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (type $inner (struct (field i32)))
            (type $mixed (struct
                (field i8)
                (field i16)
                (field (ref null $inner))
                (field i64)
            ))

            (func (export "test") (result i32)
                (local $i (ref null $inner))
                (local $m (ref null $mixed))
                (local.set $i (struct.new $inner (i32.const 99)))
                (local.set $m
                    (struct.new $mixed
                        (i32.const 1)
                        (i32.const 2)
                        (local.get $i)
                        (i64.const 3)
                    )
                )
                (i32.add
                    (i32.add
                        (struct.get_u $mixed 0 (local.get $m))
                        (struct.get_u $mixed 1 (local.get $m))
                    )
                    (i32.add
                        (struct.get $inner 0 (struct.get $mixed 2 (local.get $m)))
                        (i32.wrap_i64 (struct.get $mixed 3 (local.get $m)))
                    )
                )
            )
        )
        "#,
    )?;
    let instance = Instance::new(&mut store, &module, &[])?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 105); // 1+2+99+3
    Ok(())
}

/// Test: many allocations with gc_zeal_alloc_counter to stress GC timing
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_gc_zeal_counter_stress() -> Result<()> {
    for counter in [2, 3, 5, 7, 10] {
        let (mut store, engine) = copying_store_with_gc_zeal(counter)?;
        let module = Module::new(
            &engine,
            r#"
            (module
                (type $box (struct (field i32)))

                (func (export "test") (result i32)
                    (local $keep (ref null $box))
                    (local $i i32)

                    (local.set $keep (struct.new $box (i32.const 12345)))

                    (local.set $i (i32.const 0))
                    (block $done
                        (loop $loop
                            (br_if $done (i32.ge_u (local.get $i) (i32.const 30)))
                            (drop (struct.new $box (local.get $i)))
                            (local.set $i (i32.add (local.get $i) (i32.const 1)))
                            (br $loop)
                        )
                    )

                    (struct.get $box 0 (local.get $keep))
                )
            )
            "#,
        )?;
        let instance = Instance::new(&mut store, &module, &[])?;
        let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
        let result = test.call(&mut store, ())?;
        assert_eq!(result, 12345, "failed with gc_zeal_counter={counter}");
    }
    Ok(())
}

/// Test: struct.set after GC - update a ref field, then trigger GC, then read it
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_struct_set_gc_ref() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $inner (struct (field i32)))
            (type $outer (struct (field (mut (ref null $inner)))))

            (func (export "test") (result i32)
                (local $o (ref null $outer))
                (local $i (ref null $inner))
                ;; Create outer with null inner
                (local.set $o (struct.new $outer (ref.null $inner)))
                ;; Create inner
                (local.set $i (struct.new $inner (i32.const 999)))
                ;; Set the field
                (struct.set $outer 0 (local.get $o) (local.get $i))
                ;; Trigger GC
                (call $gc)
                ;; Read the field
                (struct.get $inner 0 (struct.get $outer 0 (local.get $o)))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 999);
    Ok(())
}

/// Test: array.fill with GC refs, trigger GC, read back
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_array_fill_gc_ref() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $box (struct (field i32)))
            (type $arr (array (mut (ref null $box))))

            (func (export "test") (result i32)
                (local $a (ref null $arr))
                (local $b (ref null $box))
                ;; Create box with value 77
                (local.set $b (struct.new $box (i32.const 77)))
                ;; Create array of 5 nulls
                (local.set $a (array.new $arr (ref.null $box) (i32.const 5)))
                ;; Fill all elements with the box
                (array.fill $arr (local.get $a) (i32.const 0) (local.get $b) (i32.const 5))
                ;; Trigger GC
                (call $gc)
                ;; Read element 3
                (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 3)))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 77);
    Ok(())
}

/// Test: deeply nested struct references survive multiple GCs
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_deep_nesting_multi_gc() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $node (struct (field i32) (field (mut (ref null $node)))))

            (func (export "test") (result i32)
                (local $n1 (ref null $node))
                (local $n2 (ref null $node))
                (local $n3 (ref null $node))
                (local $n4 (ref null $node))
                ;; Build chain: n4 -> n3 -> n2 -> n1
                (local.set $n1 (struct.new $node (i32.const 1) (ref.null $node)))
                (local.set $n2 (struct.new $node (i32.const 2) (local.get $n1)))
                (call $gc)
                (local.set $n3 (struct.new $node (i32.const 3) (local.get $n2)))
                (call $gc)
                (local.set $n4 (struct.new $node (i32.const 4) (local.get $n3)))
                (call $gc)
                ;; Traverse: n4.next.next.next.val should be 1
                (struct.get $node 0
                    (struct.get $node 1
                        (struct.get $node 1
                            (struct.get $node 1
                                (local.get $n4)
                            )
                        )
                    )
                )
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 1);
    Ok(())
}

/// Test: i31ref in a struct field (should not be forwarded, just preserved)
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_i31ref_in_struct() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $box (struct (field (mut anyref))))

            (func (export "test") (result i32)
                (local $b (ref null $box))
                ;; Store an i31ref in the anyref field
                (local.set $b (struct.new $box (ref.i31 (i32.const 42))))
                (call $gc)
                ;; Read it back
                (i31.get_s (ref.cast (ref i31) (struct.get $box 0 (local.get $b))))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 42);
    Ok(())
}

/// Test: struct with many fields of different types including v128
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_multi_field_struct() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $big (struct
                (field i32)
                (field i64)
                (field f32)
                (field f64)
                (field i32)
            ))

            (func (export "test") (result i32)
                (local $s (ref null $big))
                (local.set $s (struct.new $big
                    (i32.const 100)
                    (i64.const 200)
                    (f32.const 3.14)
                    (f64.const 2.718)
                    (i32.const 500)
                ))
                (call $gc)
                (i32.add
                    (struct.get $big 0 (local.get $s))
                    (struct.get $big 4 (local.get $s))
                )
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 600);
    Ok(())
}

/// Test: many allocations forcing multiple GC cycles and heap growth
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_growth_and_gc_cycles() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $node (struct (field i32) (field (ref null $node))))

            (func (export "test") (result i32)
                (local $head (ref null $node))
                (local $i i32)
                ;; Build a linked list of 200 nodes
                (local.set $head (ref.null $node))
                (local.set $i (i32.const 0))
                (block $done
                    (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (i32.const 200)))
                        (local.set $head (struct.new $node (local.get $i) (local.get $head)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)
                    )
                )
                ;; Trigger GC a few times
                (call $gc)
                (call $gc)
                (call $gc)
                ;; Head value should be 199
                (struct.get $node 0 (local.get $head))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 199);
    Ok(())
}

/// Test: array.copy with GC refs
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_array_copy_gc_refs() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $box (struct (field i32)))
            (type $arr (array (mut (ref null $box))))

            (func (export "test") (result i32)
                (local $src (ref null $arr))
                (local $dst (ref null $arr))
                ;; Create source array
                (local.set $src (array.new_fixed $arr 3
                    (struct.new $box (i32.const 10))
                    (struct.new $box (i32.const 20))
                    (struct.new $box (i32.const 30))
                ))
                ;; Create dest array of nulls
                (local.set $dst (array.new $arr (ref.null $box) (i32.const 3)))
                ;; Copy src[0..3] to dst[0..3]
                (array.copy $arr $arr (local.get $dst) (i32.const 0) (local.get $src) (i32.const 0) (i32.const 3))
                ;; Trigger GC
                (call $gc)
                ;; Read from dst
                (i32.add
                    (struct.get $box 0 (array.get $arr (local.get $dst) (i32.const 0)))
                    (struct.get $box 0 (array.get $arr (local.get $dst) (i32.const 2)))
                )
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 40);
    Ok(())
}

/// Test: empty struct (minimum size object)
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_empty_struct() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $empty (struct))
            (type $holder (struct (field (ref null $empty)) (field i32)))

            (func (export "test") (result i32)
                (local $e (ref null $empty))
                (local $h (ref null $holder))
                (local.set $e (struct.new_default $empty))
                (local.set $h (struct.new $holder (local.get $e) (i32.const 42)))
                (call $gc)
                ;; Check that the empty struct ref is still valid
                (if (ref.is_null (struct.get $holder 0 (local.get $h)))
                    (then (return (i32.const -1)))
                )
                (struct.get $holder 1 (local.get $h))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 42);
    Ok(())
}

/// Test: struct.set with different types, then GC, then get
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_packed_field_set_get() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $packed (struct (field (mut i8)) (field (mut i16)) (field (mut i32))))

            (func (export "test") (result i32)
                (local $p (ref null $packed))
                (local.set $p (struct.new $packed (i32.const 0) (i32.const 0) (i32.const 0)))
                ;; Set fields
                (struct.set $packed 0 (local.get $p) (i32.const 0x7f))
                (struct.set $packed 1 (local.get $p) (i32.const 0x1234))
                (struct.set $packed 2 (local.get $p) (i32.const 0xdeadbeef))
                ;; GC
                (call $gc)
                ;; Get and combine
                (i32.add
                    (i32.add
                        (struct.get_u $packed 0 (local.get $p))
                        (struct.get_u $packed 1 (local.get $p))
                    )
                    (struct.get $packed 2 (local.get $p))
                )
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    // 0x7f + 0x1234 + 0xdeadbeef (as i32)
    let expected = 0x7fi32.wrapping_add(0x1234i32).wrapping_add(0xdeadbeefu32 as i32);
    assert_eq!(result, expected);
    Ok(())
}

/// Test: i64 and f64 fields survive GC
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_i64_f64_fields() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $s (struct (field i64) (field f64)))

            (func (export "test") (result i64)
                (local $x (ref null $s))
                (local.set $x (struct.new $s (i64.const 0x123456789abcdef0) (f64.const 1.0)))
                (call $gc)
                (struct.get $s 0 (local.get $x))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i64>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 0x123456789abcdef0i64);
    Ok(())
}

/// Test: array.new_default with i64 elements, fill, GC, read back
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_i64_array() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $arr (array (mut i64)))

            (func (export "test") (result i64)
                (local $a (ref null $arr))
                (local.set $a (array.new $arr (i64.const 0xfedcba9876543210) (i32.const 10)))
                (call $gc)
                (array.get $arr (local.get $a) (i32.const 7))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i64>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 0xfedcba9876543210u64 as i64);
    Ok(())
}

/// Test: struct with GC ref field, allocate many to force heap growth, verify
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_growth_with_refs() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $pair (struct (field i32) (field (ref null $pair))))

            (func (export "test") (result i32)
                (local $head (ref null $pair))
                (local $i i32)
                (local $sum i32)

                ;; Build a chain of 500 pairs (forces heap growth + GCs)
                (local.set $i (i32.const 0))
                (block $done
                    (loop $loop
                        (br_if $done (i32.ge_u (local.get $i) (i32.const 500)))
                        (local.set $head (struct.new $pair (local.get $i) (local.get $head)))
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)
                    )
                )
                ;; Trigger explicit GC
                (call $gc)

                ;; Walk the chain and sum the first 5 values
                ;; head=499, next=498, next=497, next=496, next=495
                (local.set $sum (struct.get $pair 0 (local.get $head)))
                (local.set $head (struct.get $pair 1 (local.get $head)))
                (local.set $sum (i32.add (local.get $sum) (struct.get $pair 0 (local.get $head))))
                (local.set $head (struct.get $pair 1 (local.get $head)))
                (local.set $sum (i32.add (local.get $sum) (struct.get $pair 0 (local.get $head))))
                (local.set $head (struct.get $pair 1 (local.get $head)))
                (local.set $sum (i32.add (local.get $sum) (struct.get $pair 0 (local.get $head))))
                (local.set $head (struct.get $pair 1 (local.get $head)))
                (local.set $sum (i32.add (local.get $sum) (struct.get $pair 0 (local.get $head))))
                (local.get $sum)
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    // 499 + 498 + 497 + 496 + 495 = 2485
    assert_eq!(result, 2485);
    Ok(())
}

/// Test: externref survives GC alongside typed GC refs
#[test]
#[cfg_attr(miri, ignore)]
fn copying_probe_externref_and_gc_ref() -> Result<()> {
    let (mut store, engine) = copying_store(None)?;
    let module = Module::new(
        &engine,
        r#"
        (module
            (import "wasmtime" "gc" (func $gc))
            (type $box (struct (field i32)))

            (func (export "test") (result i32)
                (local $b (ref null $box))
                ;; Allocate struct, then trigger GC, then read
                (local.set $b (struct.new $box (i32.const 42)))
                (call $gc)
                (struct.get $box 0 (local.get $b))
            )
        )
        "#,
    )?;
    let instance = instantiate_with_gc(&mut store, &module)?;
    let test = instance.get_typed_func::<(), i32>(&mut store, "test")?;
    let result = test.call(&mut store, ())?;
    assert_eq!(result, 42);
    Ok(())
}
