#![no_main]

use cranelift_pulley::{
    decode::Decoder,
    op::{MaterializeOpsVisitor, Op},
};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|ops: Vec<Op>| {
    let _ = env_logger::try_init();

    log::trace!("input: {ops:#?}");

    let mut encoded = vec![];
    for op in &ops {
        op.encode(&mut encoded);
    }
    log::trace!("encoded: {encoded:?}");

    let visitor = Decoder::decode_all(MaterializeOpsVisitor::default(), &encoded)
        .expect("should decode okay");
    let decoded = visitor.finish();
    log::trace!("decoded: {decoded:#?}");

    assert_eq!(
        decoded, ops,
        "`decode(encode(ops))` should be equal to the original `ops`"
    );
});
