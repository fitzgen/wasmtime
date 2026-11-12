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
;;     region15 = 108 ""
;;     region16 = 8 ""
;;     region17 = 218 ""
;;     region18 = 252 ""
;;     region19 = 13 ""
;;     region20 = 34 ""
;;     region21 = 195 ""
;;     region22 = 158 ""
;;     region23 = 174 ""
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly can_move region0 gv0+8
;;     gv2 = load.i64 notrap aligned region1 gv1+24
;;     sig0 = (i64 vmctx, i64) tail
;;     sig1 = (i64 vmctx, i64, i32, i32) tail
;;     sig2 = (i64 vmctx, i64) -> i32 tail
;;     sig3 = (i64 vmctx, i64, i32) -> i32 tail
;;     fn0 = colocated u0:0 sig3
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32):
;; @021e                               jump block4
;;
;;                                 block6(v4: i64):
;; @021e                               jump block3
;;
;;                                 block4:
;; @0225                               v6 = load.i64 notrap aligned readonly can_move region2 v0+488
;; @0225                               v7 = load.i32 notrap aligned region3 v6
;; @0229                               trapz v7, user26
;; @0229                               jump block7
;;
;;                                 block7:
;; @0233                               v13 = load.i64 notrap aligned readonly can_move region0 v0+8
;; @0233                               v14 = load.i64 notrap aligned region5 v13+136
;; @0233                               v12 = stack_addr.i64 ss0
;; @0233                               store notrap aligned region6 v14, v12
;; @0218                               v3 = iconst.i32 0
;; @0233                               store notrap aligned region7 v3, v12+8  ; v3 = 0
;; @0231                               v10 = iconst.i32 1
;; @0233                               store notrap aligned region8 v10, v12+12  ; v10 = 1
;; @0233                               v15 = load.i32 notrap aligned region9 v13+128
;; @0233                               store notrap aligned region10 v15, v12+16
;; @0233                               store notrap aligned region9 v3, v13+128  ; v3 = 0
;; @0233                               v17 = load.i32 notrap aligned region11 v13+132
;; @0233                               store notrap aligned region12 v17, v12+20
;; @0233                               store notrap aligned region11 v3, v13+132  ; v3 = 0
;; @0233                               v19 = load.i64 notrap aligned region13 v13+152
;; @0233                               store notrap aligned region14 v19, v12+24
;; @0233                               store notrap aligned region5 v12, v13+136
;; @0242                               jump block12
;;
;;                                 block12:
;; @0244                               v32 = iconst.i64 8
;;                                     v135 = icmp.i64 uge v19, v32  ; v32 = 8
;; @0247                               brif v135, block11, block13
;;
;;                                 block13:
;; @022b                               v8 = load.i64 notrap aligned readonly can_move region4 v0+104
;; @024b                               v42 = load.i64 notrap aligned region15 v8+120
;; @024b                               v39 = iconst.i32 -1
;; @024b                               v40 = iadd.i32 v2, v39  ; v39 = -1
;; @024b                               v41 = uextend.i64 v40
;;                                     v137 = icmp uge v41, v42
;; @024e                               brif v137, block11, block14
;;
;;                                 block14:
;; @0252                               v52 = load.i64 notrap aligned region16 v8+112
;; @0252                               v53 = iconst.i64 24
;; @0252                               v54 = imul.i64 v41, v53  ; v53 = 24
;; @0252                               v55 = iadd v52, v54
;; @0252                               v56 = load.i32 notrap aligned region17 v55
;; @024b                               v147 = iconst.i32 -1
;; @0252                               v148 = iadd v56, v147  ; v147 = -1
;; @0231                               v149 = iconst.i32 1
;;                                     v150 = icmp ugt v148, v149  ; v149 = 1
;; @0255                               brif v150, block11, block15
;;
;;                                 block15:
;; @0259                               v73 = load.i32 notrap aligned region18 v55+4
;; @0259                               v74 = iconst.i32 2
;;                                     v141 = icmp ne v73, v74  ; v74 = 2
;; @025c                               brif v141, block11, block16
;;
;;                                 block16:
;; @0260                               v89 = load.i32 notrap aligned region19 v55+8
;; @0259                               v151 = iconst.i32 2
;; @0260                               v91 = load.i64 notrap aligned readonly can_move region20 v13+144
;;                                     v142 = iconst.i64 3
;;                                     v143 = ishl.i64 v19, v142  ; v142 = 3
;; @0260                               v94 = iadd v91, v143
;; @0260                               store notrap aligned region21 v151, v94  ; v151 = 2
;; @0260                               store.i32 notrap aligned region22 v2, v94+4
;; @0260                               v96 = iconst.i64 1
;; @0260                               v97 = iadd.i64 v19, v96  ; v96 = 1
;; @0260                               store notrap aligned region13 v97, v13+152
;; @026a                               v98 = load.i64 notrap aligned readonly can_move region4 v0+72
;; @026a                               try_call fn0(v98, v0, v89), sig3, block17(ret0), [ context v0, default: block6(exn0) ]
;;
;;                                 block17(v99: i32):
;; @026e                               v103 = load.i64 notrap aligned region5 v13+136
;;                                     v146 = icmp ne v103, v12
;; @0271                               brif v146, block10(v99), block18
;;
;;                                 block18:
;; @0273                               v112 = load.i64 notrap aligned region6 v12
;; @0273                               store notrap aligned region5 v112, v13+136
;; @0273                               v113 = load.i64 notrap aligned region14 v12+24
;; @0273                               store notrap aligned region13 v113, v13+152
;; @0273                               v114 = load.i32 notrap aligned region10 v12+16
;; @0273                               store notrap aligned region9 v114, v13+128
;; @0273                               v115 = load.i32 notrap aligned region12 v12+20
;; @0273                               store notrap aligned region11 v115, v13+132
;; @0277                               jump block9(v99)
;;
;;                                 block11 cold:
;; @027c                               v118 = load.i64 notrap aligned readonly can_move region23 v0+152
;; @022b                               v152 = load.i64 notrap aligned readonly can_move region4 v0+104
;; @027c                               v119 = call_indirect sig3, v118(v152, v0, v2)
;; @026a                               v153 = load.i64 notrap aligned readonly can_move region4 v0+72
;; @0286                               try_call fn0(v153, v0, v119), sig3, block19(ret0), [ context v0, default: block6(exn0) ]
;;
;;                                 block19(v121: i32):
;; @028a                               jump block10(v121)
;;
;;                                 block10(v124: i32) cold:
;; @028b                               v123 = load.i64 notrap aligned readonly can_move region23 v0+376
;; @022b                               v154 = load.i64 notrap aligned readonly can_move region4 v0+104
;; @028b                               call_indirect sig0, v123(v154, v0)
;; @028f                               jump block9(v124)
;;
;;                                 block9(v125: i32):
;; @0290                               jump block5
;;
;;                                 block5:
;; @0291                               jump block2
;;
;;                                 block3:
;; @0294                               trap user52
;;
;;                                 block2:
;; @0298                               jump block1
;;
;;                                 block1:
;; @0298                               return v125
;; }
