Improve the performance of the DRC collector and its free list.

To measure benchmark performance, run this script:

```
$ ./perf.sh
```

It will output a number (instructions retired). Your goal is to make that number
as small as you can, while tests continue to pass. Note that running the
`perf.sh` script repeatedly will result in numbers with a small amount of
variation; consider very small deltas insignificant. You can run the script a
few times initially to get an idea of the sisze of the variation.

To run tests, run this script:

```
$ ./test.sh
```

You may customize the `RUST_LOG` and `WASMTIME_GC_ZEAL_ALLOC_COUNTER` env vars
when running the test script if it helps you debug failing tests.

To profile the benchmark and see what percentage of instructions are spent in
which functions, run this script:

```
$ ./profile.sh
```

You MUST NOT modify any of the above scripts.

You MUST NOT modify the Wasm program being run.

You MUST NOT modify tests (except to add `let _ = env_logger::try_init();`, if
needed for debugging, or the expected output of `tests/disas/*` if you make a
codegen improvement).

You MUST NOT add any of the above scripts, wasm files, or cwasm files to a
commit.

Make a commit after each successful performance optimization. Describe the
optimization and the percent of instructions retired that it shaves off of the
benchmark in the commit message. DO NOT add any co-authored-by annotation to the
commit message. The tests MUST pass for each commit.
