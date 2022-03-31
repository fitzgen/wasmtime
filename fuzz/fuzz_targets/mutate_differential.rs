#![no_main]

use libfuzzer_sys::{fuzz_mutator, fuzz_target};

fuzz_target!(|wasm: &[u8]| {
    wasmtime_fuzzing::oracles::mutate_differential(wasm);
});

fuzz_mutator!(|data: &mut [u8], size: usize, max_size: usize, seed: u32| {
    // Roughly half the time, use libfuzzer's default mutator and the other half
    // of the time, use our `wasm-mutate` based mutator.
    if seed.count_ones() % 2 == 0 {
        return libfuzzer_sys::fuzzer_mutate(data, size, max_size);
    }

    wasmtime_fuzzing::mutators::wasm_mutate(
        data,
        size,
        max_size,
        seed,
        libfuzzer_sys::fuzzer_mutate,
    )
});
