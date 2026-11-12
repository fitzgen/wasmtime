;;! target = "x86_64"
;;! test = "optimize"
;;! filter = "wasm[3]--function"
;;! flags = "-Wconcurrency-support=y"

;; An adapter passing a `list<borrow>` to the instance owning the resource.
;; Each element's transfer happens inside the adapter's loop over the list, so
;; FACT can't lay its fast path out to never rejoin its slow path, and instead
;; versions it as a local diamond. The `exit-sync-call` is still versioned at
;; the top level of the adapter.

(component
  (component $A
    (type $t' (resource (rep i32)))
    (export $t "t" (type $t'))

    (core module $M
      (memory (export "mem") 1)
      (func (export "realloc") (param i32 i32 i32 i32) (result i32)
        i32.const 1024)
      (func (export "f") (param i32 i32) (result i32)
        local.get 1)
    )
    (core instance $m (instantiate $M))

    (func (export "f") (param "xs" (list (borrow $t))) (result u32)
      (canon lift (core func $m "f")
        (memory (core memory $m "mem"))
        (realloc (core func $m "realloc"))))
  )
  (instance $a (instantiate $A))

  (component $B
    (import "a" (instance $i
      (export "t" (type $t (sub resource)))
      (export "f" (func (param "xs" (list (borrow $t))) (result u32)))
    ))
    (alias export $i "t" (type $t))
    (alias export $i "f" (func $f))

    (core module $Mem
      (memory (export "mem") 1)
    )
    (core instance $mem (instantiate $Mem))

    (core func $f' (canon lower (func $f) (memory (core memory $mem "mem"))))

    (core module $M
      (import "" "f" (func $f (param i32 i32) (result i32)))
      (func (export "run") (param i32 i32) (result i32)
        (call $f (local.get 0) (local.get 1))
      )
    )
    (core instance $m (instantiate $M
      (with "" (instance (export "f" (func $f'))))))

    (func (export "run") (param "p" u32) (param "n" u32) (result u32)
      (canon lift (core func $m "run"))
    )
  )
  (instance $b (instantiate $B (with "a" (instance $a))))
  (export "run" (func $b "run"))
)
;; function u3:0(i64 vmctx, i64, i32, i32) -> i32 tail {
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
;;     region16 = 75 ""
;;     region17 = 215 ""
;;     region18 = 105 ""
;;     region19 = 108 ""
;;     region20 = 8 ""
;;     region21 = 218 ""
;;     region22 = 252 ""
;;     region23 = 13 ""
;;     region24 = 34 ""
;;     region25 = 195 ""
;;     region26 = 158 ""
;;     region27 = 174 ""
;;     region28 = 171 ""
;;     gv0 = vmctx
;;     gv1 = load.i64 notrap aligned readonly can_move region0 gv0+8
;;     gv2 = load.i64 notrap aligned region1 gv1+24
;;     sig0 = (i64 vmctx, i64) tail
;;     sig1 = (i64 vmctx, i64, i32, i32) tail
;;     sig2 = (i64 vmctx, i64) -> i32 tail
;;     sig3 = (i64 vmctx, i64, i32) tail
;;     sig4 = (i64 vmctx, i64, i32, i32, i32, i32) -> i32 tail
;;     sig5 = (i64 vmctx, i64, i32) -> i32 tail
;;     sig6 = (i64 vmctx, i64, i32, i32) -> i32 tail
;;     fn0 = colocated u2147483648:17 sig2
;;     fn1 = colocated u2147483648:19 sig2
;;     fn2 = colocated u2147483648:18 sig3
;;     fn3 = colocated u2147483648:20 sig3
;;     fn4 = colocated u0:0 sig4
;;     fn5 = colocated u0:1 sig6
;;     stack_limit = gv2
;;
;;                                 block0(v0: i64, v1: i64, v2: i32, v3: i32):
;; @02b9                               jump block4
;;
;;                                 block6(v7: i64):
;; @02b9                               jump block3
;;
;;                                 block4:
;; @02c0                               v9 = load.i64 notrap aligned readonly can_move region2 v0+760
;; @02c0                               v10 = load.i32 notrap aligned region3 v9
;; @02c4                               trapz v10, user26
;; @02c4                               jump block7
;;
;;                                 block7:
;; @02ce                               v16 = load.i64 notrap aligned readonly can_move region0 v0+8
;; @02ce                               v17 = load.i64 notrap aligned region5 v16+136
;; @02ce                               v15 = stack_addr.i64 ss0
;; @02ce                               store notrap aligned region6 v17, v15
;; @02af                               v4 = iconst.i32 0
;; @02ce                               store notrap aligned region7 v4, v15+8  ; v4 = 0
;; @02cc                               v13 = iconst.i32 1
;; @02ce                               store notrap aligned region8 v13, v15+12  ; v13 = 1
;; @02ce                               v18 = load.i32 notrap aligned region9 v16+128
;; @02ce                               store notrap aligned region10 v18, v15+16
;; @02ce                               store notrap aligned region9 v4, v16+128  ; v4 = 0
;; @02ce                               v20 = load.i32 notrap aligned region11 v16+132
;; @02ce                               store notrap aligned region12 v20, v15+20
;; @02ce                               store notrap aligned region11 v4, v16+132  ; v4 = 0
;; @02ce                               v22 = load.i64 notrap aligned region13 v16+152
;; @02ce                               store notrap aligned region14 v22, v15+24
;; @02ce                               store notrap aligned region5 v15, v16+136
;; @02d0                               v23 = load.i64 notrap aligned readonly can_move region2 v0+736
;; @02d0                               v24 = load.i32 notrap aligned region15 v23
;; @02d6                               store notrap aligned region15 v4, v23  ; v4 = 0
;; @02e6                               v27 = uextend.i64 v3
;;                                     v237 = iconst.i64 2
;;                                     v238 = ishl v27, v237  ; v237 = 2
;; @02ec                               v30 = iconst.i64 32
;; @02ee                               v31 = ushr v238, v30  ; v30 = 32
;; @02b1                               v5 = iconst.i64 0
;; @02ef                               v33 = icmp eq v31, v5  ; v5 = 0
;; @02f0                               brif v33, block9, block11
;;
;;                                 block11:
;; @02f2                               jump block10
;;
;;                                 block10:
;; @02f3                               trap user34
;;
;;                                 block9:
;; @0302                               v37 = iconst.i32 3
;; @0304                               v38 = band.i32 v2, v37  ; v37 = 3
;; @0305                               trapnz v38, user36
;; @0305                               jump block13
;;
;;                                 block13:
;; @030f                               v40 = load.i64 notrap aligned readonly can_move region16 v0+72
;; @030f                               v41 = load.i64 notrap aligned region18 v40+8
;; @030f                               v42 = iconst.i64 16
;; @030f                               v43 = ushr v41, v42  ; v42 = 16
;; @030f                               v44 = ireduce.i32 v43
;; @0311                               v45 = uextend.i64 v44
;; @0314                               v47 = ishl v45, v42  ; v42 = 16
;; @0317                               v48 = uextend.i64 v2
;;                                     v244 = iconst.i32 2
;;                                     v245 = ishl.i32 v3, v244  ; v244 = 2
;; @031a                               v49 = uextend.i64 v245
;; @031b                               v50 = iadd v48, v49
;; @031c                               v51 = icmp uge v47, v50
;; @031d                               brif v51, block14, block16
;;
;;                                 block16:
;; @031f                               jump block15
;;
;;                                 block15:
;; @0320                               trap user34
;;
;;                                 block14:
;; @033c                               v69 = load.i64 notrap aligned readonly can_move region4 v0+120
;; @02af                               v264 = iconst.i32 0
;;                                     v242 = iconst.i32 4
;; @033c                               try_call fn4(v69, v0, v264, v264, v242, v245), sig4, block17(ret0), [ context v0, default: block6(exn0) ]  ; v264 = 0, v264 = 0, v242 = 4
;;
;;                                 block17(v70: i32):
;; @02af                               v265 = iconst.i32 0
;; @0340                               store notrap aligned region9 v265, v16+128  ; v265 = 0
;; @0344                               store notrap aligned region11 v265, v16+132  ; v265 = 0
;; @0302                               v266 = iconst.i32 3
;; @034c                               v267 = band v70, v266  ; v266 = 3
;; @034d                               trapnz v267, user36
;; @034d                               jump block19
;;
;;                                 block19:
;; @0357                               v78 = load.i64 notrap aligned readonly can_move region16 v0+48
;; @0357                               v79 = load.i64 notrap aligned region18 v78+8
;; @030f                               v268 = iconst.i64 16
;; @0357                               v269 = ushr v79, v268  ; v268 = 16
;; @0357                               v82 = ireduce.i32 v269
;; @0359                               v83 = uextend.i64 v82
;; @035c                               v270 = ishl v83, v268  ; v268 = 16
;; @035f                               v86 = uextend.i64 v70
;; @0363                               v88 = iadd v86, v49
;; @0364                               v89 = icmp uge v270, v88
;; @0365                               brif v89, block20, block22
;;
;;                                 block22:
;; @0367                               jump block21
;;
;;                                 block21:
;; @0368                               trap user34
;;
;;                                 block20:
;; @02af                               v271 = iconst.i32 0
;; @0372                               v272 = icmp.i32 eq v3, v271  ; v271 = 0
;; @0373                               brif v272, block23(v70), block24
;;
;;                                 block24:
;; @0381                               v98 = load.i64 notrap aligned readonly can_move region17 v40
;; @0390                               v113 = iconst.i64 8
;; @02c6                               v11 = load.i64 notrap aligned readonly can_move region4 v0+184
;; @0397                               v120 = iconst.i32 -1
;; @039e                               v134 = iconst.i64 24
;; @03ac                               v172 = load.i64 notrap aligned readonly can_move region24 v16+144
;;                                     v259 = iconst.i64 3
;; @03ac                               v177 = iconst.i64 1
;; @03b5                               v181 = load.i64 notrap aligned readonly can_move region27 v0+424
;; @03be                               v189 = load.i64 notrap aligned readonly can_move region17 v78
;; @037d                               jump block25(v2, v70, v3, v70)
;;
;;                                 block25(v95: i32, v184: i32, v198: i32, v204: i32):
;; @0381                               v96 = uextend.i64 v95
;; @0381                               v99 = iadd.i64 v98, v96
;; @0381                               v100 = load.i32 little region14 v99
;; @038b                               v104 = load.i64 notrap aligned region5 v16+136
;;                                     v250 = icmp ne v104, v15
;; @038e                               brif v250, block28, block29
;;
;;                                 block29:
;; @0390                               v112 = load.i64 notrap aligned region13 v16+152
;; @0390                               v273 = iconst.i64 8
;;                                     v274 = icmp uge v112, v273  ; v273 = 8
;; @0393                               brif v274, block28, block30
;;
;;                                 block30:
;; @0397                               v123 = load.i64 notrap aligned region19 v11+120
;; @0397                               v275 = iconst.i32 -1
;; @0397                               v276 = iadd.i32 v100, v275  ; v275 = -1
;; @0397                               v122 = uextend.i64 v276
;;                                     v254 = icmp uge v122, v123
;; @039a                               brif v254, block28, block31
;;
;;                                 block31:
;; @039e                               v133 = load.i64 notrap aligned region20 v11+112
;; @039e                               v277 = iconst.i64 24
;; @039e                               v278 = imul.i64 v122, v277  ; v277 = 24
;; @039e                               v136 = iadd v133, v278
;; @039e                               v137 = load.i32 notrap aligned region21 v136
;; @0397                               v279 = iconst.i32 -1
;; @039e                               v280 = iadd v137, v279  ; v279 = -1
;; @02cc                               v281 = iconst.i32 1
;;                                     v282 = icmp ugt v280, v281  ; v281 = 1
;; @03a1                               brif v282, block28, block32
;;
;;                                 block32:
;; @03a5                               v154 = load.i32 notrap aligned region22 v136+4
;;                                     v283 = iconst.i32 2
;;                                     v284 = icmp ne v154, v283  ; v283 = 2
;; @03a8                               brif v284, block28, block33
;;
;;                                 block33:
;; @03ac                               v170 = load.i32 notrap aligned region23 v136+8
;;                                     v285 = iconst.i32 2
;;                                     v286 = iconst.i64 3
;;                                     v287 = ishl.i64 v112, v286  ; v286 = 3
;; @03ac                               v175 = iadd.i64 v172, v287
;; @03ac                               store notrap aligned region25 v285, v175  ; v285 = 2
;; @03ac                               store.i32 notrap aligned region26 v100, v175+4
;; @03ac                               v288 = iconst.i64 1
;; @03ac                               v289 = iadd.i64 v112, v288  ; v288 = 1
;; @03ac                               store notrap aligned region13 v289, v16+152
;; @03b0                               jump block27(v170)
;;
;;                                 block28 cold:
;; @03b5                               v182 = call_indirect.i64 sig5, v181(v11, v0, v100)
;; @03b9                               jump block27(v182)
;;
;;                                 block27(v186: i32):
;; @03be                               v187 = uextend.i64 v184
;; @03be                               v190 = iadd.i64 v189, v187
;; @03be                               store little region28 v186, v190
;; @0397                               v290 = iconst.i32 -1
;; @03d3                               v291 = iadd.i32 v198, v290  ; v290 = -1
;;                                     v292 = iconst.i32 4
;; @03c5                               v293 = iadd.i32 v95, v292  ; v292 = 4
;; @03cc                               v294 = iadd.i32 v184, v292  ; v292 = 4
;; @03d6                               brif v291, block25(v293, v294, v291, v204), block34
;;
;;                                 block34:
;; @03d8                               jump block26
;;
;;                                 block26:
;; @03d9                               jump block23(v204)
;;
;;                                 block23(v202: i32):
;; @03e8                               store.i32 notrap aligned region15 v24, v23
;; @03ea                               try_call fn5(v69, v0, v202, v3), sig6, block35(ret0), [ context v0, default: block6(exn0) ]
;;
;;                                 block35(v216: i32):
;; @03f2                               v220 = load.i64 notrap aligned region5 v16+136
;;                                     v263 = icmp ne v220, v15
;; @03f5                               brif v263, block37, block38
;;
;;                                 block38:
;; @03f7                               v229 = load.i64 notrap aligned region6 v15
;; @03f7                               store notrap aligned region5 v229, v16+136
;; @03f7                               v230 = load.i64 notrap aligned region14 v15+24
;; @03f7                               store notrap aligned region13 v230, v16+152
;; @03f7                               v231 = load.i32 notrap aligned region10 v15+16
;; @03f7                               store notrap aligned region9 v231, v16+128
;; @03f7                               v232 = load.i32 notrap aligned region12 v15+20
;; @03f7                               store notrap aligned region11 v232, v16+132
;; @03fb                               jump block36
;;
;;                                 block37 cold:
;; @03fe                               v234 = load.i64 notrap aligned readonly can_move region27 v0+648
;; @02c6                               v295 = load.i64 notrap aligned readonly can_move region4 v0+184
;; @03fe                               call_indirect sig0, v234(v295, v0)
;; @0402                               jump block36
;;
;;                                 block36:
;; @0403                               jump block5
;;
;;                                 block5:
;; @0404                               jump block2
;;
;;                                 block3:
;; @0407                               trap user52
;;
;;                                 block2:
;; @040b                               jump block1
;;
;;                                 block1:
;; @040b                               return v216
;; }
