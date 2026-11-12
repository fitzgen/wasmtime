;;! target = "x86_64"
;;! test = "compile"
;;! filter = "wasm[1]::function"
;;! flags = "-Cinlining=y -Wconcurrency-support=y"

;; Like `sync-adapter-lazy-lend.wat`, but does its work in a loop so we can
;; check what gets LICM'd.

(component
  (component $A
    (type $t' (resource (rep i32)))
    (export $t "t" (type $t'))

    (core func $ctor (canon resource.new $t))

    (core module $M
      (func (export "get'") (param i32) (result i32)
        (local.get 0)
      )
    )
    (core instance $m (instantiate $M))

    (func (export "[constructor]t") (param "x" u32) (result (own $t))
      (canon lift (core func $ctor))
    )
    (func (export "get") (param "self" (borrow $t)) (result u32)
      (canon lift (core func $m "get'"))
    )
  )
  (instance $a (instantiate $A))

  (component $B
    (import "a" (instance $i
      (export "t" (type $t (sub resource)))
      (export "get" (func (param "self" (borrow $t)) (result u32)))
    ))
    (alias export $i "t" (type $t))
    (alias export $i "get" (func $get))

    (core func $get' (canon lower (func $get)))

    (core module $M
      (import "" "get" (func $get (param i32) (result i32)))
      (func (export "run") (param i32 i32) (result i32)
        loop
          (call $get (local.get 0))

          local.get 1
          i32.const 0
          i32.ne
          if
            local.get 1
            i32.const 1
            i32.sub
            local.set 1
            br 1
          end

          return
        end
        unreachable
      )
    )
    (core instance $m (instantiate $M
      (with "" (instance (export "get" (func $get'))))))

    (func (export "run") (param "x" u32) (param "n" u32) (result u32)
      (canon lift (core func $m "run"))
    )
  )
  (instance $b (instantiate $B (with "a" (instance $a))))
  (export "run" (func $b "run"))
)

;; wasm[1]::function[1]:
;;       pushq   %rbp
;;       movq    %rsp, %rbp
;;       movq    8(%rdi), %r10
;;       movq    0x18(%r10), %r10
;;       addq    $0xb0, %r10
;;       cmpq    %rsp, %r10
;;       ja      0x26e
;;   3c: subq    $0xa0, %rsp
;;       movq    %rbx, 0x70(%rsp)
;;       movq    %r12, 0x78(%rsp)
;;       movq    %r13, 0x80(%rsp)
;;       movq    %r14, 0x88(%rsp)
;;       movq    %r15, 0x90(%rsp)
;;       movq    %rcx, 0x30(%rsp)
;;       movq    0x48(%rdi), %rax
;;       movq    0x1e8(%rax), %r14
;;       movq    %r14, 0x58(%rsp)
;;       movq    8(%rax), %r13
;;       movq    %rax, 0x60(%rsp)
;;       leaq    (%rsp), %r12
;;       movq    %r12, 0x50(%rsp)
;;       movq    0x68(%rax), %rbx
;;       leal    -1(%rdx), %r12d
;;       movq    %rdx, 0x20(%rsp)
;;       imulq   $0x18, %r12, %r15
;;       movq    %r12, 0x48(%rsp)
;;       movq    0x90(%r13), %r14
;;       movq    0x60(%rsp), %rsi
;;       movq    %r14, 0x40(%rsp)
;;       movq    0x98(%rsi), %r12
;;       movq    %r12, %r14
;;       movq    0x178(%rsi), %r12
;;       movq    %r12, 0x38(%rsp)
;;       movq    0x58(%rsp), %rax
;;       movl    (%rax), %ecx
;;       testl   %ecx, %ecx
;;       je      0x270
;;   d8: movq    0x88(%r13), %rdi
;;       movq    %rdi, (%rsp)
;;       movl    $0, 8(%rsp)
;;       movl    $1, 0xc(%rsp)
;;       movl    0x80(%r13), %ecx
;;       movl    %ecx, 0x10(%rsp)
;;       movl    $0, 0x80(%r13)
;;       movl    0x84(%r13), %edx
;;       movl    %edx, 0x14(%rsp)
;;       movl    $0, 0x84(%r13)
;;       movq    0x98(%r13), %rsi
;;       movq    %rsi, 0x18(%rsp)
;;       movq    0x50(%rsp), %r8
;;       movq    %r8, 0x88(%r13)
;;       cmpq    $8, %rsi
;;       jae     0x21e
;;  141: movq    0x78(%rbx), %r8
;;       movq    0x48(%rsp), %rax
;;       cmpq    %r8, %rax
;;       jae     0x228
;;  153: movq    0x70(%rbx), %rax
;;       movl    $0xffffffff, %r8d
;;       addl    (%rax, %r15), %r8d
;;       cmpl    $1, %r8d
;;       ja      0x232
;;  16b: movl    4(%rax, %r15), %r8d
;;       cmpl    $2, %r8d
;;       jne     0x23c
;;  17a: movl    8(%rax, %r15), %eax
;;       movq    0x40(%rsp), %r8
;;       movl    $2, (%r8, %rsi, 8)
;;       movq    0x20(%rsp), %r12
;;       movl    %r12d, 4(%r8, %rsi, 8)
;;       leaq    1(%rsi), %r8
;;       movq    %r8, 0x98(%r13)
;;       movq    %rdi, 0x88(%r13)
;;       movq    %rsi, 0x98(%r13)
;;       movl    %ecx, 0x80(%r13)
;;       movl    %edx, 0x84(%r13)
;;       movq    %rax, 0x28(%rsp)
;;       movq    %r12, 0x20(%rsp)
;;       movq    0x38(%rsp), %r12
;;       movq    0x30(%rsp), %rcx
;;       testl   %ecx, %ecx
;;       je      0x1eb
;;  1d9: subl    $1, %ecx
;;       movq    %rcx, 0x30(%rsp)
;;       movq    %r12, 0x38(%rsp)
;;       jmp     0xc9
;;  1eb: movq    0x28(%rsp), %rax
;;       movq    0x70(%rsp), %rbx
;;       movq    0x78(%rsp), %r12
;;       movq    0x80(%rsp), %r13
;;       movq    0x88(%rsp), %r14
;;       movq    0x90(%rsp), %r15
;;       addq    $0xa0, %rsp
;;       movq    %rbp, %rsp
;;       popq    %rbp
;;       retq
;;  21e: movq    0x20(%rsp), %r12
;;  223: jmp     0x241
;;  228: movq    0x20(%rsp), %r12
;;  22d: jmp     0x241
;;  232: movq    0x20(%rsp), %r12
;;  237: jmp     0x241
;;  23c: movq    0x20(%rsp), %r12
;;  241: movq    %r12, %rdx
;;  244: movq    0x60(%rsp), %rsi
;;  249: movq    %rbx, %rdi
;;  24c: callq   *%r14
;;  24f: movq    %r12, 0x20(%rsp)
;;  254: movq    %rax, 0x28(%rsp)
;;  259: movq    0x38(%rsp), %r12
;;  25e: movq    0x60(%rsp), %rsi
;;  263: movq    %rbx, %rdi
;;  266: callq   *%r12
;;  269: jmp     0x1cc
;;  26e: ud2
;;  270: ud2
