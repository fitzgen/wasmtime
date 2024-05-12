(module
  (func (export "i32.add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
)
(assert_return (invoke "i32.add" (i32.const 1) (i32.const 2)) (i32.const 3))
