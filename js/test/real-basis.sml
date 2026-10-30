fun check name b = if b then () else raise Fail name
structure D : REAL = Real
structure D64 : REAL = Real64
structure L : REAL = LargeReal
structure H : REAL = Real16
structure S : REAL = Real32
val () = List.app (fn fmt =>
  (check "positive infinity" (fmt Real.posInf = "inf");
   check "negative infinity" (fmt Real.negInf = "~inf");
   check "NaN format" (fmt (0.0/0.0) = "nan")))
  [Real.toString,Real.fmt (StringCvt.FIX NONE),Real.fmt (StringCvt.SCI NONE),Real.fmt StringCvt.EXACT]
val () = check "signed zero" (Real.signBit ~0.0 andalso String.isPrefix "~0" (Real.toString ~0.0))
val nan = 0.0/0.0
val () = check "unordered" (Real.unordered(nan,1.0) andalso not (Real.==(nan,nan)) andalso Real.?=(nan,1.0))
val () = check "compare exception" ((Real.compare(nan,1.0);false) handle IEEEReal.Unordered => true)
val () = check "NaN sign exception" ((Real.sign nan;false) handle Domain => true)
val () = check "integer NaN exception" ((Real.toInt IEEEReal.TO_ZERO nan;false) handle Domain => true)
val () = check "small integer conversion"
  (Real.toLargeInt IEEEReal.TO_ZERO 1.5 = 1 andalso
   Real.toLargeInt IEEEReal.TO_NEGINF (~1.5) = ~2)
val () = check "round ties" (Real.round 2.5 = 2 andalso Real.round ~1.5 = ~2)
val () = check "integer conversion"
  (Real.toLargeInt IEEEReal.TO_NEAREST 1e30 = valOf(IntInf.fromString "1000000000000000019884624838656") andalso
   Real.==(Real.fromLargeInt 9007199254740993,9007199254740992.0))
val () = List.app (fn x =>
  let val d = Real.toDecimal x
      val {man,exp} = Real.toManExp x
  in check "decimal roundtrip" (Real.==(valOf(Real.fromDecimal d),x));
     check "binary roundtrip" (Real.==(Real.fromManExp {man=man,exp=exp},x))
  end) [Real.minPos,Real.minNormalPos,Real.maxFinite,~1.25]
val () = check "adjacent values"
  (Real.==(Real.nextAfter(0.0,1.0),Real.minPos) andalso
   Real.==(Real.nextAfter(Real.posInf,0.0),Real.maxFinite))
val () = check "scan special" (Real.==(valOf(Real.fromString "~inf"),Real.negInf))
val a = RealArray.fromList [1.5,~0.0]
val v : Real64Vector.vector = RealArray.vector a
val () = RealArray.update(a,0,2.5)
val () = check "vector snapshot" (Real.==(LargeRealVector.sub(v,0),1.5))
val slice : LargeRealArraySlice.slice = RealArraySlice.full a
val () = Real64ArraySlice.update(slice,0,4.5)
val () = check "slice sharing" (Real.==(LargeRealArray.sub(a,0),4.5) andalso Real.signBit(RealVector.sub(v,1)))
val m : Real64Array2.array = RealArray2.array(2,2,0.0)
val () = LargeRealArray2.update(m,1,1,Real.minPos)
val () = check "Array2" (Real.==(RealArray2.sub(m,1,1),Real.minPos))
val () = check "bounds" ((RealVector.sub(v,2);false) handle Subscript => true)
val () = print "JavaScript REAL checks passed\n"
