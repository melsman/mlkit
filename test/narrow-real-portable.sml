(* Small cross-backend cases; print raw bits to compare native and JS results. *)
fun check name b = if b then () else raise Fail name
fun bits16 (x:Real16.real) : word = prim ("id", x)
fun bits32 (x:Real32.real) : word = prim ("id", x)
fun show (a,b) = print (Word.toString a ^ " " ^ Word.toString b ^ "\n")
val _ : real = (1.0/2.0)
val _ : Real.real = (0.0:Real64.real)
val _ : LargeReal.real = (0.0:Real.real)
structure HalfSignature : REAL = Real16
structure SingleSignature : REAL = Real32
val () = check "unsigned word conversion"
  (Word32.fromLargeInt 4294967295 = 0wxffffffff andalso
   Word32.fromLargeInt ~1 = 0wxffffffff andalso
   Word32.fromLargeInt 4294967297 = 0w1)
val a16 : Real16.real = 1.00048828125000000001
val a32 : Real32.real = 1.00000005960464477539062500001
val () = show(bits16 a16,bits32 a32)
val () = show(bits16 (~0.0:Real16.real),bits32 (~0.0:Real32.real))
val () = show(bits16 (1.0/3.0:Real16.real),bits32 (1.0/3.0:Real32.real))
val () = show(bits16 (Math16.sqrt 2.0),bits32 (Math32.sqrt 2.0))
val () = show(bits16 (Real16.minNormalPos*0.5),bits32 (Real32.minNormalPos*0.5))
val () = show(bits16 (Real16.rem(~7.0,2.0)),bits32 (Real32.rem(~7.0,2.0)))
val () = show(bits16 (Real16.fromManExp {man=1.0,exp = ~24}),
               bits32 (Real32.fromManExp {man=1.0,exp = ~149}))
val big = IntInf.+ (IntInf.<< (1,0w80),IntInf.+ (IntInf.<< (1,0w56),1))
val () = show(bits16 (Real16.fromLargeInt 2051),bits32 (Real32.fromLargeInt big))
val nan16 = PackReal16Big.fromBytes (Word8Vector.fromList [0wxfe,0wx23])
val nan32 = PackReal32Big.fromBytes (Word8Vector.fromList [0wxff,0wxc0,0wx01,0wx23])
val () = check "unordered" (Real16.isNan nan16 andalso Real32.isNan nan32 andalso
                             not (Real16.<(nan16,0.0)) andalso not (Real32.==(nan32,nan32)))
val () = check "integer NaN domain"
  (((Real16.floor nan16; false) handle Domain => true) andalso
   ((Real32.floor nan32; false) handle Domain => true))
val () = check "large integer conversion"
  (Real16.toLargeInt IEEEReal.TO_NEGINF (~1.5) = ~2 andalso
   Real32.toLargeInt IEEEReal.TO_ZERO 1.5 = 1)
val () = check "shared exception" ((Real32.compare(nan32,nan32); false)
                                    handle IEEEReal.Unordered => true)
fun rounding mode =
  (IEEEReal.setRoundingMode mode;
   show(bits16 (Real16.fromLarge mode 1.00048828125),
        bits32 (Real32.fromLarge mode 1.000000059604644775390625));
   show(bits16 (Real16.fromLarge mode (~1.00048828125)),
        bits32 (Real32.fromLarge mode (~1.000000059604644775390625)));
   show(bits16 (1.0/3.0:Real16.real),bits32 (1.0/3.0:Real32.real));
   show(bits16 (Math16.sqrt 2.0),bits32 (Math32.sqrt 2.0));
   show(bits16 (a16*a16),bits32 (a32*a32));
   show(bits16 (1.0+Real16.minPos),bits32 (1.0+Real32.minPos));
   show(bits16 (~1.0-Real16.minPos),bits32 (~1.0-Real32.minPos));
   show(bits16 (Real16.fromLarge mode 1e100),bits32 (Real32.fromLarge mode 1e100));
   show(bits16 (valOf(Real16.fromString "1e~1000")),
        bits32 (valOf(Real32.fromString "1e~1000"))))
val () = List.app rounding
  [IEEEReal.TO_NEAREST,IEEEReal.TO_NEGINF,IEEEReal.TO_POSINF,IEEEReal.TO_ZERO]
val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST
val () = show(bits16 (valOf(Real16.fromDecimal(Real16.toDecimal a16))),
               bits32 (valOf(Real32.fromDecimal(Real32.toDecimal a32))))
val () = check "Math sharing" (Real16.==(Real16.Math.pi,Math16.pi) andalso
                                Real32.==(Real32.Math.e,Math32.e))
val a = Real16Array.fromList [1.0,nan16,~0.0]
val v = Real16Array.vector a
val () = Real16Array.update(a,0,2.0)
val () = check "vector independence" (Real16.==(Real16Vector.sub(v,0),1.0))
val s = Real16ArraySlice.slice(a,1,NONE)
val () = show(bits16(Real16ArraySlice.sub(s,0)),bits32 nan32)
val x = Real32Vector.fromList [1.0,nan32,~0.0]
val t = Real32VectorSlice.slice(x,1,NONE)
val () = check "vector slice payload" (bits32(Real32VectorSlice.sub(t,0)) = bits32 nan32)
val m = Real32Array2.array(2,2,0.0)
val () = Real32Array2.update(m,1,1,nan32)
val () = show(bits16(PackReal16Little.fromBytes(PackReal16Little.toBytes nan16)),
               bits32(Real32Array2.sub(m,1,1)))
val () = check "array bounds" ((Real16Array.sub(a,3); false) handle Subscript => true)
val () = check "pack bounds" ((PackReal32Big.subVec(PackReal32Big.toBytes 1.0,~1);false)
                               handle Subscript => true)
val () = print "OK\n"
