//! Tests related to compile-time builtins.

use super::*;
use std::path::Path;
use wasmtime::{CodeBuilder, Engine, Instance, Store};

#[test]
#[cfg_attr(miri, ignore)]
fn basic() -> Result<()> {
    let _ = env_logger::try_init();

    let engine = Engine::default();

    let mut builder = CodeBuilder::new(&engine);
    builder.wasm_binary_or_text(
        r#"
            (module
                (import "math" "add" (func $add (param i32 i32) (result i32)))
                (func (export "f") (result i32)
                    (call $add (i32.const 1) (i32.const 2))
                )
            )
        "#
        .as_bytes(),
        Some(Path::new("basic.wat")),
    )?;

    let mut builtin = builder.define_compile_time_builtin("math", "add");
    let x = builtin.i32_arg(0);
    let y = builtin.i32_arg(1);
    let z = builtin.i32_add(x, y);
    builtin.finish([z])?;

    let module = builder.compile_module()?;

    assert_eq!(
        module.imports().count(),
        0,
        "import was satisfied at compile time"
    );

    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])?;

    let f = instance.get_typed_func::<(), i32>(&mut store, "f")?;
    assert_eq!(f.call(&mut store, ())?, 3);

    Ok(())
}

// TODO FITZGEN: test re-export of imported compile-time builtin

// TODO FITZGEN: test `ref.func` of imported compile-time builtin

// TODO FITZGEN: test type error in builtin def

// TODO FITZGEN: test using invalid builtin value from different builtin
