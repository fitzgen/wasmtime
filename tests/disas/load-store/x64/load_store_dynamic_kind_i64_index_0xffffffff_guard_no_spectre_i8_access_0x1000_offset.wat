;;! target = "x86_64"
;;! test = "compile"
;;! flags = " -C cranelift-enable-heap-access-spectre-mitigation=false -W memory64 -O static-memory-maximum-size=0 -O static-memory-guard-size=4294967295 -O dynamic-memory-guard-size=4294967295"

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!! GENERATED BY 'make-load-store-tests.sh' DO NOT EDIT !!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(module
  (memory i64 1)

  (func (export "do_store") (param i64 i32)
    local.get 0
    local.get 1
    i32.store8 offset=0x1000)

  (func (export "do_load") (param i64) (result i32)
    local.get 0
    i32.load8_u offset=0x1000))

;; wasm[0]::function[0]:
;;       pushq   %rbp
;;       movq    %rsp, %rbp
;;       cmpq    0x40(%rdi), %rdx
;;       ja      0x1f
;;    e: movq    0x38(%rdi), %r9
;;       movb    %cl, 0x1000(%r9, %rdx)
;;       movq    %rbp, %rsp
;;       popq    %rbp
;;       retq
;;   1f: ud2
;;
;; wasm[0]::function[1]:
;;       pushq   %rbp
;;       movq    %rsp, %rbp
;;       cmpq    0x40(%rdi), %rdx
;;       ja      0x60
;;   4e: movq    0x38(%rdi), %r9
;;       movzbq  0x1000(%r9, %rdx), %rax
;;       movq    %rbp, %rsp
;;       popq    %rbp
;;       retq
;;   60: ud2
