;;! target = "x86_64"
;;! test = "optimize"
;;! filter = "wasm[2]--function"
;;! flags = "-Wconcurrency-support=y"

;; An adapter that passes a `borrow` to the instance owning the resource, with
;; both halves of the lazy-lend fast path inline and no libcall on either.

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
      (func (export "run") (param i32) (result i32)
        (call $get (local.get 0))
      )
    )
    (core instance $m (instantiate $M
      (with "" (instance (export "get" (func $get'))))))

    (func (export "run") (param "x" u32) (result u32)
      (canon lift (core func $m "run"))
    )
  )
  (instance $b (instantiate $B (with "a" (instance $a))))
  (export "run" (func $b "run"))
)
;; function u2:0(i64 vmctx, i64, i32) -> i32 tail {
;;     ss0 = explicit_slot 32, align = 8
;;     region0 = 123 ""
;;     region1 = 160 ""
;;     region2 = 57 ""
;;     region3 = 209 ""
;;     region4 = 42 ""
;;     region5 = 68 ""
;;     region6 = 237 ""
;;     region7 = 126 ""
;;     region8 = 221 ""
;;     region9 = 112 ""
;;     region10 = 71 ""
;;     region11 = 149 ""
;;     region12 = 93 ""
;;     region13 = 30 ""
;;     region14 = 184 ""
;;     region15 = 132 ""
;;     region16 = 108 ""
;;     region17 = 8 ""
;;     region18 = 218 ""
;;     region19 = 252 ""
;;     region20 = 13 ""
;;     region21 = 34 ""
;;     region22 = 195 ""
;;     region23 = 158 ""
;;     region24 = 174 ""
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly can_move region0 gv0+8
;;     gv2 = load.i64 notrap aligned region1 gv1+24
;;     sig0 = (i64 vmctx, i64) tail
;;     sig1 = (i64 vmctx, i64, i32, i32) tail
;;     sig2 = (i64 vmctx, i64, i32) -> i32 tail
;;     fn0 = colocated u0:0 sig2
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32):
;; @00e7                               jump block4
;;
;;                                 block6(v4: i64):
;; @00e7                               jump block3
;;
;;                                 block4:
;; @00ee                               v6 = load.i64 notrap aligned readonly can_move region2 v0+264
;; @00ee                               v7 = load.i32 notrap aligned region3 v6
;; @00f2                               trapz v7, user26
;; @00f2                               jump block7
;;
;;                                 block7:
;; @00fc                               v13 = load.i64 notrap aligned readonly can_move region0 v0+8
;; @00fc                               v14 = load.i64 notrap aligned region5 v13+136
;; @00fc                               v12 = stack_addr.i64 ss0
;; @00fc                               store notrap aligned region6 v14, v12
;; @00e1                               v3 = iconst.i32 0
;; @00fc                               store notrap aligned region7 v3, v12+8  ; v3 = 0
;; @00fa                               v10 = iconst.i32 1
;; @00fc                               store notrap aligned region8 v10, v12+12  ; v10 = 1
;; @00fc                               v15 = load.i32 notrap aligned region9 v13+128
;; @00fc                               store notrap aligned region10 v15, v12+16
;; @00fc                               store notrap aligned region9 v3, v13+128  ; v3 = 0
;; @00fc                               v17 = load.i32 notrap aligned region11 v13+132
;; @00fc                               store notrap aligned region12 v17, v12+20
;; @00fc                               store notrap aligned region11 v3, v13+132  ; v3 = 0
;; @00fc                               v19 = load.i64 notrap aligned region13 v13+152
;; @00fc                               store notrap aligned region14 v19, v12+24
;; @00fc                               store notrap aligned region5 v12, v13+136
;; @00fe                               v20 = load.i64 notrap aligned readonly can_move region2 v0+240
;; @00fe                               v21 = load.i32 notrap aligned region15 v20
;; @0104                               store notrap aligned region15 v3, v20  ; v3 = 0
;; @00f4                               v8 = load.i64 notrap aligned readonly can_move region4 v0+104
;; @0108                               v36 = load.i64 notrap aligned region16 v8+120
;; @0108                               v30 = iconst.i64 8
;; @0108                               v31 = icmp ult v19, v30  ; v30 = 8
;; @0108                               v33 = iconst.i32 -1
;; @0108                               v34 = iadd.i32 v2, v33  ; v33 = -1
;; @0108                               v35 = uextend.i64 v34
;; @0108                               v37 = icmp ult v35, v36
;; @0108                               v38 = band v31, v37
;; @0108                               brif v38, block9, block11
;;
;;                                 block9:
;; @0108                               v40 = load.i64 notrap aligned region17 v8+112
;; @0108                               v41 = iconst.i64 24
;; @0108                               v42 = imul.i64 v35, v41  ; v41 = 24
;; @0108                               v43 = iadd v40, v42
;; @0108                               v44 = load.i32 notrap aligned region18 v43
;; @0108                               v49 = load.i32 notrap aligned region19 v43+4
;;                                     v93 = iconst.i32 -1
;;                                     v94 = iadd v44, v93  ; v93 = -1
;;                                     v95 = iconst.i32 1
;;                                     v96 = icmp ule v94, v95  ; v95 = 1
;; @0108                               v50 = iconst.i32 2
;; @0108                               v51 = icmp eq v49, v50  ; v50 = 2
;; @0108                               v52 = band v96, v51
;; @0108                               brif v52, block10, block11
;;
;;                                 block10:
;; @0108                               v53 = load.i32 notrap aligned region20 v43+8
;;                                     v97 = iconst.i32 2
;; @0108                               v54 = load.i64 notrap aligned readonly can_move region21 v13+144
;;                                     v90 = iconst.i64 3
;;                                     v91 = ishl.i64 v19, v90  ; v90 = 3
;; @0108                               v57 = iadd v54, v91
;; @0108                               store notrap aligned region22 v97, v57  ; v97 = 2
;; @0108                               store.i32 notrap aligned region23 v2, v57+4
;; @0108                               v59 = iconst.i64 1
;; @0108                               v60 = iadd.i64 v19, v59  ; v59 = 1
;; @0108                               store notrap aligned region13 v60, v13+152
;; @0108                               jump block12(v53)
;;
;;                                 block11:
;; @0108                               v61 = load.i64 notrap aligned readonly can_move region24 v0+152
;; @0108                               try_call_indirect v61(v8, v0, v2), sig2, block13(ret0), [ context v0, default: block6(exn0) ]
;;
;;                                 block13(v62: i32):
;; @0108                               jump block12(v62)
;;
;;                                 block12(v39: i32):
;; @010c                               store.i32 notrap aligned region15 v21, v20
;; @010e                               v66 = load.i64 notrap aligned readonly can_move region4 v0+72
;; @010e                               try_call fn0(v66, v0, v39), sig2, block14(ret0), [ context v0, default: block6(exn0) ]
;;
;;                                 block14(v67: i32):
;; @0112                               v71 = load.i64 notrap aligned region5 v13+136
;; @0112                               v72 = icmp eq v71, v12
;; @0112                               brif v72, block15, block16
;;
;;                                 block15:
;; @0112                               v73 = load.i64 notrap aligned region6 v12
;; @0112                               store notrap aligned region5 v73, v13+136
;; @0112                               v74 = load.i64 notrap aligned region14 v12+24
;; @0112                               store notrap aligned region13 v74, v13+152
;; @0112                               v75 = load.i32 notrap aligned region10 v12+16
;; @0112                               store notrap aligned region9 v75, v13+128
;; @0112                               v76 = load.i32 notrap aligned region12 v12+20
;; @0112                               store notrap aligned region11 v76, v13+132
;; @0112                               jump block17
;;
;;                                 block16:
;; @0112                               v77 = load.i64 notrap aligned readonly can_move region24 v0+184
;; @0112                               try_call_indirect v77(v8, v0), sig0, block18, [ context v0, default: block6(exn0) ]
;;
;;                                 block18:
;; @0112                               jump block17
;;
;;                                 block17:
;; @011c                               store.i32 notrap aligned region3 v7, v6
;; @011e                               jump block5
;;
;;                                 block5:
;; @011f                               jump block2
;;
;;                                 block3:
;; @0122                               trap user52
;;
;;                                 block2:
;; @0126                               jump block1
;;
;;                                 block1:
;; @0126                               return v67
;; }
