;;! target = "x86_64"
;;! test = "optimize"
;;! filter = "wasm[1]--function"
;;! flags = "-Cinlining=y -Wconcurrency-support=y"

;; An adapter passing several `borrow`s, mixed with other parameters, to the
;; instance owning their resource, with the callee inlined. FACT versions each
;; transfer and the `exit-sync-call`, and lays the adapter out so that its fast
;; path never rejoins its slow path. So on the fast path, each transfer after
;; the first and the `exit-sync-call` all know that the deferred thread is
;; still current, and `exit-sync-call` knows the values that `enter-sync-call`
;; stored into it.

(component
  (component $A
    (type $t' (resource (rep i32)))
    (export $t "t" (type $t'))

    (core module $M
      (func (export "f") (param i32 i32 i32 i32) (result i32)
        (i32.add (local.get 0)
          (i32.add (local.get 1)
            (i32.add (local.get 2) (local.get 3))))
      )
    )
    (core instance $m (instantiate $M))

    (func (export "f")
        (param "a" (borrow $t)) (param "x" u32) (param "b" (borrow $t))
        (param "c" (borrow $t))
        (result u32)
      (canon lift (core func $m "f"))
    )
  )
  (instance $a (instantiate $A))

  (component $B
    (import "a" (instance $i
      (export "t" (type $t (sub resource)))
      (export "f" (func
        (param "a" (borrow $t)) (param "x" u32) (param "b" (borrow $t))
        (param "c" (borrow $t))
        (result u32)))
    ))
    (alias export $i "t" (type $t))
    (alias export $i "f" (func $f))

    (core func $f' (canon lower (func $f)))

    (core module $M
      (import "" "f" (func $f (param i32 i32 i32 i32) (result i32)))
      (func (export "run") (param i32 i32 i32) (result i32)
        (call $f (local.get 0) (i32.const 42) (local.get 1) (local.get 2))
      )
    )
    (core instance $m (instantiate $M
      (with "" (instance (export "f" (func $f'))))))

    (func (export "run") (param "x" u32) (param "y" u32) (param "z" u32) (result u32)
      (canon lift (core func $m "run"))
    )
  )
  (instance $b (instantiate $B (with "a" (instance $a))))
  (export "run" (func $b "run"))
)
;; function u1:0(i64 vmctx, i64, i32, i32, i32) -> i32 tail {
;;     ss0 = explicit_slot 32, align = 8
;;     region0 = 123 ""
;;     region1 = 160 ""
;;     region2 = 42 ""
;;     region3 = 57 ""
;;     region4 = 209 ""
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
;;     region15 = 174 ""
;;     region16 = 108 ""
;;     region17 = 8 ""
;;     region18 = 218 ""
;;     region19 = 252 ""
;;     region20 = 13 ""
;;     region21 = 34 ""
;;     region22 = 195 ""
;;     region23 = 158 ""
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly can_move region0 gv0+8
;;     gv2 = load.i64 notrap aligned region1 gv1+24
;;     gv3 = vmctx
;;     gv4 = load.i64 notrap aligned readonly can_move region0 gv3+8
;;     gv5 = load.i64 notrap aligned region1 gv4+24
;;     gv6 = vmctx
;;     gv7 = load.i64 notrap aligned readonly can_move region0 gv6+8
;;     gv8 = load.i64 notrap aligned region1 gv7+24
;;     gv9 = vmctx
;;     gv10 = load.i64 notrap aligned readonly can_move region0 gv9+8
;;     gv11 = load.i64 notrap aligned region1 gv10+24
;;     sig0 = (i64 vmctx, i64, i32, i32, i32, i32) -> i32 tail
;;     sig1 = (i64 vmctx, i64) tail
;;     sig2 = (i64 vmctx, i64, i32, i32) tail
;;     sig3 = (i64 vmctx, i64) -> i32 tail
;;     sig4 = (i64 vmctx, i64, i32) -> i32 tail
;;     sig5 = (i64 vmctx, i64, i32, i32, i32, i32) -> i32 tail
;;     fn0 = colocated u2:0 sig0
;;     fn1 = colocated u0:0 sig5
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32, v3: i32, v4: i32):
;; @015d                               jump block2
;;
;;                                 block2:
;;                                     jump block6
;;
;;                                 block6:
;; @015d                               v6 = load.i64 notrap aligned readonly can_move region2 v0+72
;;                                     v18 = load.i64 notrap aligned readonly can_move region3 v6+488
;;                                     v19 = load.i32 notrap aligned region4 v18
;;                                     trapz v19, user26
;;                                     jump block9
;;
;;                                 block9:
;;                                     v24 = load.i64 notrap aligned readonly can_move region0 v6+8
;;                                     v25 = load.i64 notrap aligned region5 v24+136
;;                                     v23 = stack_addr.i64 ss0
;;                                     store notrap aligned region6 v25, v23
;;                                     v17 = iconst.i32 0
;;                                     store notrap aligned region7 v17, v23+8  ; v17 = 0
;;                                     v21 = iconst.i32 1
;;                                     store notrap aligned region8 v21, v23+12  ; v21 = 1
;;                                     v26 = load.i32 notrap aligned region9 v24+128
;;                                     store notrap aligned region10 v26, v23+16
;;                                     store notrap aligned region9 v17, v24+128  ; v17 = 0
;;                                     v28 = load.i32 notrap aligned region11 v24+132
;;                                     store notrap aligned region12 v28, v23+20
;;                                     store notrap aligned region11 v17, v24+132  ; v17 = 0
;;                                     v30 = load.i64 notrap aligned region13 v24+152
;;                                     store notrap aligned region14 v30, v23+24
;;                                     store notrap aligned region5 v23, v24+136
;;                                     jump block16
;;
;;                                 block16:
;;                                     v58 = iconst.i64 8
;;                                     v309 = icmp.i64 uge v30, v58  ; v58 = 8
;;                                     brif v309, block15, block17
;;
;;                                 block17:
;;                                     v300 = load.i64 notrap aligned readonly can_move region2 v6+104
;;                                     v68 = load.i64 notrap aligned region16 v300+120
;;                                     v65 = iconst.i32 -1
;;                                     v66 = iadd.i32 v2, v65  ; v65 = -1
;;                                     v67 = uextend.i64 v66
;;                                     v311 = icmp uge v67, v68
;;                                     brif v311, block15, block18
;;
;;                                 block18:
;;                                     v78 = load.i64 notrap aligned region17 v300+112
;;                                     v79 = iconst.i64 24
;;                                     v80 = imul.i64 v67, v79  ; v79 = 24
;;                                     v81 = iadd v78, v80
;;                                     v82 = load.i32 notrap aligned region18 v81
;;                                     v361 = iconst.i32 -1
;;                                     v362 = iadd v82, v361  ; v361 = -1
;;                                     v363 = iconst.i32 1
;;                                     v364 = icmp ugt v362, v363  ; v363 = 1
;;                                     brif v364, block15, block19
;;
;;                                 block19:
;;                                     v99 = load.i32 notrap aligned region19 v81+4
;;                                     v100 = iconst.i32 2
;;                                     v315 = icmp ne v99, v100  ; v100 = 2
;;                                     brif v315, block15, block20
;;
;;                                 block20:
;;                                     v115 = load.i32 notrap aligned region20 v81+8
;;                                     v365 = iconst.i32 2
;;                                     v117 = load.i64 notrap aligned readonly can_move region21 v24+144
;;                                     v316 = iconst.i64 3
;;                                     v317 = ishl.i64 v30, v316  ; v316 = 3
;;                                     v120 = iadd v117, v317
;;                                     store notrap aligned region22 v365, v120  ; v365 = 2
;;                                     store.i32 notrap aligned region23 v2, v120+4
;;                                     v122 = iconst.i64 1
;;                                     v123 = iadd.i64 v30, v122  ; v122 = 1
;;                                     store notrap aligned region13 v123, v24+152
;;                                     jump block21
;;
;;                                 block21:
;;                                     v366 = iadd.i64 v30, v122  ; v122 = 1
;;                                     v367 = iconst.i64 8
;;                                     v368 = icmp uge v366, v367  ; v367 = 8
;;                                     brif v368, block14(v115), block22
;;
;;                                 block22:
;;                                     v369 = iconst.i32 -1
;;                                     v370 = iadd.i32 v3, v369  ; v369 = -1
;;                                     v145 = uextend.i64 v370
;;                                     v322 = icmp uge v145, v68
;;                                     brif v322, block14(v115), block23
;;
;;                                 block23:
;;                                     v371 = iconst.i64 24
;;                                     v372 = imul.i64 v145, v371  ; v371 = 24
;;                                     v159 = iadd.i64 v78, v372
;;                                     v160 = load.i32 notrap aligned region18 v159
;;                                     v373 = iconst.i32 -1
;;                                     v374 = iadd v160, v373  ; v373 = -1
;;                                     v375 = iconst.i32 1
;;                                     v376 = icmp ugt v374, v375  ; v375 = 1
;;                                     brif v376, block14(v115), block24
;;
;;                                 block24:
;;                                     v177 = load.i32 notrap aligned region19 v159+4
;;                                     v377 = iconst.i32 2
;;                                     v378 = icmp ne v177, v377  ; v377 = 2
;;                                     brif v378, block14(v115), block25
;;
;;                                 block25:
;;                                     v193 = load.i32 notrap aligned region20 v159+8
;;                                     v379 = iconst.i32 2
;;                                     v380 = iadd.i64 v30, v122  ; v122 = 1
;;                                     v381 = iconst.i64 3
;;                                     v382 = ishl v380, v381  ; v381 = 3
;;                                     v198 = iadd.i64 v117, v382
;;                                     store notrap aligned region22 v379, v198  ; v379 = 2
;;                                     store.i32 notrap aligned region23 v3, v198+4
;;                                     v330 = iconst.i64 2
;;                                     v335 = iadd.i64 v30, v330  ; v330 = 2
;;                                     store notrap aligned region13 v335, v24+152
;;                                     jump block26
;;
;;                                 block26:
;;                                     v383 = iadd.i64 v30, v330  ; v330 = 2
;;                                     v384 = iconst.i64 8
;;                                     v385 = icmp uge v383, v384  ; v384 = 8
;;                                     brif v385, block13(v115, v193), block27
;;
;;                                 block27:
;;                                     v386 = iconst.i32 -1
;;                                     v387 = iadd.i32 v4, v386  ; v386 = -1
;;                                     v223 = uextend.i64 v387
;;                                     v340 = icmp uge v223, v68
;;                                     brif v340, block13(v115, v193), block28
;;
;;                                 block28:
;;                                     v388 = iconst.i64 24
;;                                     v389 = imul.i64 v223, v388  ; v388 = 24
;;                                     v237 = iadd.i64 v78, v389
;;                                     v238 = load.i32 notrap aligned region18 v237
;;                                     v390 = iconst.i32 -1
;;                                     v391 = iadd v238, v390  ; v390 = -1
;;                                     v392 = iconst.i32 1
;;                                     v393 = icmp ugt v391, v392  ; v392 = 1
;;                                     brif v393, block13(v115, v193), block29
;;
;;                                 block29:
;;                                     v255 = load.i32 notrap aligned region19 v237+4
;;                                     v394 = iconst.i32 2
;;                                     v395 = icmp ne v255, v394  ; v394 = 2
;;                                     brif v395, block13(v115, v193), block30
;;
;;                                 block30:
;;                                     v271 = load.i32 notrap aligned region20 v237+8
;;                                     v396 = iconst.i32 2
;;                                     v397 = iadd.i64 v30, v330  ; v330 = 2
;;                                     v398 = iconst.i64 3
;;                                     v399 = ishl v397, v398  ; v398 = 3
;;                                     v276 = iadd.i64 v117, v399
;;                                     store notrap aligned region22 v396, v276  ; v396 = 2
;;                                     store.i32 notrap aligned region23 v4, v276+4
;;                                     v400 = iadd.i64 v30, v398  ; v398 = 3
;;                                     store notrap aligned region13 v400, v24+152
;;                                     jump block34
;;
;;                                 block34:
;;                                     jump block35
;;
;;                                 block35:
;;                                     jump block31
;;
;;                                 block31:
;;                                     jump block32
;;
;;                                 block32:
;;                                     store.i64 notrap aligned region5 v25, v24+136
;;                                     store.i64 notrap aligned region13 v30, v24+152
;;                                     store.i32 notrap aligned region9 v26, v24+128
;;                                     store.i32 notrap aligned region11 v28, v24+132
;;                                     v281 = iadd.i32 v193, v271
;; @0157                               v5 = iconst.i32 42
;;                                     v357 = iadd v281, v5  ; v5 = 42
;;                                     v283 = iadd.i32 v115, v357
;;                                     jump block11(v283)
;;
;;                                 block15 cold:
;;                                     v41 = load.i64 notrap aligned readonly can_move region15 v6+152
;;                                     v401 = load.i64 notrap aligned readonly can_move region2 v6+104
;;                                     v42 = call_indirect sig4, v41(v401, v6, v2)
;;                                     jump block14(v42)
;;
;;                                 block14(v13: i32) cold:
;;                                     v402 = load.i64 notrap aligned readonly can_move region15 v6+152
;;                                     v403 = load.i64 notrap aligned readonly can_move region2 v6+104
;;                                     v45 = call_indirect sig4, v402(v403, v6, v3)
;;                                     jump block13(v13, v45)
;;
;;                                 block13(v11: i32, v12: i32) cold:
;;                                     v404 = load.i64 notrap aligned readonly can_move region15 v6+152
;;                                     v405 = load.i64 notrap aligned readonly can_move region2 v6+104
;;                                     v48 = call_indirect sig4, v404(v405, v6, v4)
;;                                     jump block36
;;
;;                                 block36:
;;                                     jump block37
;;
;;                                 block37:
;;                                     jump block33
;;
;;                                 block33:
;;                                     v50 = iadd.i32 v12, v48
;; @0157                               v406 = iconst.i32 42
;;                                     v359 = iadd v50, v406  ; v406 = 42
;;                                     v52 = iadd.i32 v11, v359
;;                                     jump block12(v52)
;;
;;                                 block12(v10: i32) cold:
;;                                     v54 = load.i64 notrap aligned readonly can_move region15 v6+376
;;                                     v407 = load.i64 notrap aligned readonly can_move region2 v6+104
;;                                     call_indirect sig1, v54(v407, v6)
;;                                     jump block11(v10)
;;
;;                                 block11(v9: i32):
;;                                     jump block7
;;
;;                                 block7:
;;                                     jump block4
;;
;;                                 block4:
;;                                     jump block3
;;
;;                                 block3:
;;                                     jump block38(v9)
;;
;;                                 block38(v16: i32):
;; @015f                               jump block1
;;
;;                                 block1:
;; @015f                               return v16
;; }
