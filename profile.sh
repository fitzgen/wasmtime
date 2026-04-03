#!/usr/bin/env bash

set -ex

cd "$(dirname $0)"

# Build Wasmtime
cargo build \
      --profile profiling \
      -p wasmtime-cli

wasmtime="./target/profiling/wasmtime"

# Compile the test program
"$wasmtime" \
    compile -C cache=n -W gc ~/Downloads/binary-trees-wasi-opt.wasm \
    -o binary-trees-wasi-opt.cwasm

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
        binary-trees-wasi-opt.cwasm
set -e

# Report the perf profiling results.
perf report -g --stdio --addr2line="$HOME/.cargo/bin/addr2line"

# Delete the perf data.
rm perf.data*
