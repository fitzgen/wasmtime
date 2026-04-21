;;! gc = true

;; Bug 1: The copying collector's flip() wastes ALIGN (16) bytes when the
;; active semi-space starts at index 0 (to avoid NonZeroU32 index 0), but not
;; when it starts at halfway. This asymmetry means that after a collection that
;; flips from the second half to the first half, surviving objects may not fit
;; because the first half has 16 fewer usable bytes. The .expect() in copy()
;; panics.
;;
;; Run with: RUSTFLAGS="--cfg gc_zeal" cargo test --test wast --features gc-copying
;;           -- copying-bug1-semispace-capacity
;;
;; The gc import triggers collection. By keeping a large object alive and
;; triggering collections, we can force the flip from second-half to first-half
;; active space, exposing the capacity asymmetry.

(module
  (type $big (struct
    (field i64) (field i64) (field i64) (field i64)
    (field i64) (field i64) (field i64) (field i64)
  ))

  (import "wasmtime" "gc" (func $gc))

  ;; Keep a reference alive across GC so objects must survive collection.
  (global $keep (mut (ref null $big)) (ref.null $big))

  (func (export "trigger-bug")
    ;; Allocate a struct, keep it alive, and force GC repeatedly.
    ;; With gc_zeal_alloc_counter=1, each struct.new triggers a GC before/after.
    ;; Eventually the flip asymmetry causes the expect() to panic.
    (global.set $keep (struct.new $big
      (i64.const 1) (i64.const 2) (i64.const 3) (i64.const 4)
      (i64.const 5) (i64.const 6) (i64.const 7) (i64.const 8)
    ))
    call $gc
    (global.set $keep (struct.new $big
      (i64.const 1) (i64.const 2) (i64.const 3) (i64.const 4)
      (i64.const 5) (i64.const 6) (i64.const 7) (i64.const 8)
    ))
    call $gc
    (global.set $keep (struct.new $big
      (i64.const 1) (i64.const 2) (i64.const 3) (i64.const 4)
      (i64.const 5) (i64.const 6) (i64.const 7) (i64.const 8)
    ))
    call $gc
    ;; Verify the last struct is still correct.
    (if (i64.ne (struct.get $big 0 (global.get $keep)) (i64.const 1))
      (then unreachable)
    )
  )
)

(assert_return (invoke "trigger-bug"))
