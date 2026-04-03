#!/usr/bin/env bash

set -e
set -o pipefail

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

# Run the test program under `perf` and count how many instructions it takes.
perf stat -e instructions -- \
     "$wasmtime" \
     run --allow-precompiled -Wgc \
     "$dir/binary-trees-wasi-opt.cwasm" \
     2>&1
