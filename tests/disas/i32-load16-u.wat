;;! target = "x86_64"

;; Test basic code generation for i32 memory WebAssembly instructions.

(module
  (memory 1)
  (func (export "i32.load16_u") (param i32) (result i32)
    local.get 0
    i32.load16_u))

;; function u0:0(i64 vmctx, i64, i32) -> i32 tail {
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly gv0+8
;;     gv2 = load.i64 notrap aligned gv1+16
;;     gv3 = vmctx
;;     gv4 = load.i64 notrap aligned gv3+64
;;     gv5 = load.i64 notrap aligned readonly can_move checked gv3+56
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32):
;; @0032                               v4 = uextend.i64 v2
;; @0032                               v5 = load.i64 notrap aligned readonly can_move checked v0+56
;; @0032                               v6 = iadd v5, v4
;; @0032                               v7 = uload16.i32 little heap v6
;; @0035                               jump block1
;;
;;                                 block1:
;; @0035                               return v7
;; }
