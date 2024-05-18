//! Interpreter tests.

use std::ptr::NonNull;

use cranelift_pulley::{interp::Vm, *};

fn encoded(ops: &[Op]) -> Vec<u8> {
    let mut encoded = vec![];
    for op in ops {
        op.encode(&mut encoded);
    }
    log::trace!("encoded: {encoded:?}");
    encoded
}

unsafe fn run(vm: &mut Vm, ops: &[Op]) -> Result<(), *mut u8> {
    let _ = env_logger::try_init();
    let ops = encoded(ops);
    let _ = vm.call(NonNull::from(&ops[0]), &[], [])?;
    Ok(())
}

#[test]
fn xadd32() {
    let mut vm = Vm::new();

    let dst = XReg::new(0).unwrap();
    let src1 = XReg::new(1).unwrap();
    let src2 = XReg::new(2).unwrap();

    unsafe {
        run(
            &mut vm,
            &[
                Op::Xconst16(Xconst16 { dst: src1, imm: 10 }),
                Op::Xconst16(Xconst16 { dst: src2, imm: 32 }),
                Op::Xadd32(Xadd32 { dst, src1, src2 }),
                Op::Ret(Ret {}),
            ],
        )
        .unwrap();
    }

    assert_eq!(vm.state().x(dst).get_u32(), 42);
}

#[test]
fn trap() {
    let mut vm = Vm::new();
    let dst = XReg::new(0).unwrap();

    unsafe {
        run(
            &mut vm,
            &[
                Op::Xconst16(Xconst16 { dst, imm: 1 }),
                Op::ExtendedOp(ExtendedOp::Trap(Trap {})),
                Op::Xconst16(Xconst16 { dst, imm: 2 }),
                Op::Ret(Ret {}),
            ],
        )
        .unwrap_err();
    }

    // `dst` should not have been written to the second time.
    assert_eq!(vm.state().x(dst).get_u32(), 1);
}
