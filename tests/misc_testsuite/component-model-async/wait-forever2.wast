;;! component_model_async = true
;;! reference_types = true
;;! gc_types = true
;;! multi_memory = true

(component
  (component $C
    (core module $Memory (memory (export "mem") 1))
    (core instance $memory (instantiate $Memory))
    (core module $CM
      (import "" "mem" (memory 1))
      (import "" "waitable-set.new" (func $waitable-set.new (result i32)))

      (func (export "f") (result i32)
        (local $ws i32)
        ;; return WAIT on an empty waitable set
        (local.set $ws (call $waitable-set.new))
        (i32.or (i32.const 2 (; WAIT ;)) (i32.shl (local.get $ws) (i32.const 4)))
      )
      (func (export "cb") (param $event_code i32) (param $index i32) (param $payload i32) (result i32)
        unreachable
      )
    )
    (canon waitable-set.new (core func $waitable-set.new))
    (core instance $cm (instantiate $CM (with "" (instance
      (export "mem" (memory $memory "mem"))
      (export "waitable-set.new" (func $waitable-set.new))
    ))))
    (func (export "f") (result u32) (canon lift
      (core func $cm "f")
      async (memory $memory "mem") (callback (func $cm "cb"))
    ))
  )

  (component $D
    (import "f" (func $f (result u32)))

    (core module $Memory (memory (export "mem") 1))
    (core instance $memory (instantiate $Memory))
    (core module $DM
      (import "" "mem" (memory 1))
      (import "" "waitable.join" (func $waitable.join (param i32 i32)))
      (import "" "waitable-set.new" (func $waitable-set.new (result i32)))
      (import "" "waitable-set.wait" (func $waitable-set.wait (param i32 i32) (result i32)))
      (import "" "f" (func $f (param i32) (result i32)))

      (func (export "g") (result i32)
        (local $ws i32) (local $ret i32) (local $subtaski i32)
        (local.set $ws (call $waitable-set.new))
        (local.set $ret (call $f (i32.const 0)))
        (local.set $subtaski (i32.shr_u (local.get $ret) (i32.const 4)))
        (call $waitable.join (local.get $subtaski) (local.get $ws))
        (call $waitable-set.wait (local.get $ws) (i32.const 0))
        unreachable
      )
    )
    (canon waitable.join (core func $waitable.join))
    (canon waitable-set.new (core func $waitable-set.new))
    (canon waitable-set.wait (memory $memory "mem") (core func $waitable-set.wait))
    (canon lower (func $f) async (memory $memory "mem") (core func $f'))
    (core instance $dm (instantiate $DM (with "" (instance
      (export "mem" (memory $memory "mem"))
      (export "waitable.join" (func $waitable.join))
      (export "waitable-set.new" (func $waitable-set.new))
      (export "waitable-set.wait" (func $waitable-set.wait))
      (export "f" (func $f'))
    ))))
    (func (export "f") (result u32) (canon lift (core func $dm "g")))
  )

  (instance $c (instantiate $C))
  (instance $d (instantiate $D (with "f" (func $c "f"))))
  (func (export "f") (alias export $d "f"))
)

(assert_trap (invoke "f") "deadlock detected")
