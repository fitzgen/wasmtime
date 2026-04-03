#!/usr/bin/env bash

set -ex

dir=$(dirname $0)

# Build Wasmtime
cargo build --manifest-path "$dir/Cargo.toml" \
      --profile profiling \
      -p wasmtime-cli

wasmtime="$dir/target/profiling/wasmtime"

# Compile the test program
"$wasmtime" \
    compile -C cache=n -W gc ~/Downloads/binary-trees-wasi-opt.wasm \
    -o "$dir/binary-trees-wasi-opt.cwasm"

# Run the test program under `perf`.
set +e
timeout 10 \
        perf record \
        -e instructions \
        --call-graph=dwarf,65000 \
        -F 99 \
        -- \
        "$wasmtime" \
        run --allow-precompiled -Wgc \
        "$dir/binary-trees-wasi-opt.cwasm"
set -e

# Report the perf profiling results.
perf report -g --stdio --addr2line="$HOME/.cargo/bin/addr2line"
