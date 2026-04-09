;;! target = "x86_64"
;;! flags = "-W function-references,gc -C collector=null"
;;! test = "optimize"

(module
  (global $x (mut externref) (ref.null extern))
  (func (export "get") (result externref)
    (global.get $x)
  )
  (func (export "set") (param externref)
    (global.set $x (local.get 0))
  )
)

;; function u0:0(i64 vmctx, i64) -> i32 tail {
;;     region0 = 1610612736 "DefinedGlobal(StaticModuleIndex(0), DefinedGlobalIndex(0))"
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly gv0+8
;;     gv2 = load.i64 notrap aligned gv1+24
;;     gv3 = vmctx
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64):
;;                                     v6 = iconst.i64 48
;; @0034                               v4 = iadd v0, v6  ; v6 = 48
;; @0034                               v5 = load.i32 notrap aligned region0 v4
;; @0036                               jump block1
;;
;;                                 block1:
;; @0036                               return v5
;; }
;;
;; function u0:1(i64 vmctx, i64, i32) tail {
;;     region0 = 1610612736 "DefinedGlobal(StaticModuleIndex(0), DefinedGlobalIndex(0))"
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly gv0+8
;;     gv2 = load.i64 notrap aligned gv1+24
;;     gv3 = vmctx
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32):
;;                                     v5 = iconst.i64 48
;; @003b                               v4 = iadd v0, v5  ; v5 = 48
;; @003b                               store notrap aligned region0 v2, v4
;; @003d                               jump block1
;;
;;                                 block1:
;; @003d                               return
;; }
