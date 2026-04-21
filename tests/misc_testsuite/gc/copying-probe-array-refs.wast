;;! gc = true

;; Test: Arrays of GC references survive GC correctly.

(module
  (type $box (struct (field i32)))
  (type $arr (array (ref null $box)))

  (import "wasmtime" "gc" (func $gc))

  (func (export "test") (result i32)
    (local $a (ref null $arr))

    ;; Create array of 3 boxed values
    (local.set $a
      (array.new_fixed $arr 3
        (struct.new $box (i32.const 10))
        (struct.new $box (i32.const 20))
        (struct.new $box (i32.const 30))
      )
    )

    ;; Force GC - array and all elements should survive
    (call $gc)

    ;; Sum the elements
    (i32.add
      (i32.add
        (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 0)))
        (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 1)))
      )
      (struct.get $box 0 (array.get $arr (local.get $a) (i32.const 2)))
    )
  )
)

(assert_return (invoke "test") (i32.const 60))
