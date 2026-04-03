#!/usr/bin/env bash

set -ex

dir=$(dirname $0)

# Set `cfg(gc_zeal)` to enable additional GC-related checks and assertions.
RUSTFLAGS="--cfg gc_zeal"
export RUSTFLAGS

# Log at the info level by default.
RUST_LOG="${RUST_LOG:-info}"
export RUST_LOG

# Trigger GC every 32 allocations by default.
WASMTIME_GC_ZEAL_ALLOC_COUNTER="${WASMTIME_GC_ZEAL_ALLOC_COUNTER:-32}"
export WASMTIME_GC_ZEAL_ALLOC_COUNTER

# Disable parallel compilation so that logs are in order.
export RAYON_NUM_THREADS=1

cargo test --manifest-path "$dir/Cargo.toml" \
      --features trace-log \
      -p wasmtime --lib -- \
      gc drc free_list --no-capture --test-threads 1

cargo test --manifest-path "$dir/Cargo.toml" \
      --features trace-log \
      -p wasmtime-cli --test all -- \
      gc extern any struct array ref \
      --nocapture --test-threads 1

cargo test --manifest-path "$dir/Cargo.toml" \
      --features trace-log \
      -p wasmtime-cli --test wast \
      gc -- \
      -- --nocapture --test-threads 1 --format=pretty
