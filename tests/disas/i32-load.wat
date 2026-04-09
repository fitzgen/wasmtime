;;! target = "x86_64"

;; Test basic code generation for i32 memory WebAssembly instructions.

(module
  (memory 1)
  (func (export "i32.load") (param i32) (result i32)
    local.get 0
    i32.load))

;; function u0:0(i64 vmctx, i64, i32) -> i32 tail {
;;     region0 = 64 "VMContext+0x40"
;;     region1 = 56 "VMContext+0x38"
;;     region2 = 536870912 "DefinedMemory(StaticModuleIndex(0), DefinedMemoryIndex(0))"
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly gv0+8
;;     gv2 = load.i64 notrap aligned gv1+24
;;     gv3 = vmctx
;;     gv4 = load.i64 notrap aligned region0 gv3+64
;;     gv5 = load.i64 notrap aligned readonly can_move region1 gv3+56
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32):
;; @002e                               v4 = uextend.i64 v2
;; @002e                               v5 = load.i64 notrap aligned readonly can_move region1 v0+56
;; @002e                               v6 = iadd v5, v4
;; @002e                               v7 = load.i32 little region2 v6
;; @0031                               jump block1
;;
;;                                 block1:
;; @0031                               return v7
;; }
