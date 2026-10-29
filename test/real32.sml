(* The existing real families retain their type identities. *)
val _ : Real.real = (0.0:Real64.real)
val _ : LargeReal.real = (0.0:Real.real)
fun check name b = if b then () else raise Fail name
fun bits (x:Real32.real) : word = prim ("id", x)
fun eq (x,y) = Real32.== (x,y)
fun read s = valOf (Real32.fromString s)
val _ = check "precision" (Real32.precision = 24 andalso Real32.radix = 2)
val _ = check "Math sharing" (eq(Real32.Math.pi,Math32.pi))
val _ = check "constants" (Real32.isNormal Real32.minNormalPos andalso
                           Real32.class Real32.minPos = IEEEReal.SUBNORMAL andalso
                           not (Real32.isFinite Real32.posInf))
val _ = check "gradual underflow" (bits(Real32.minNormalPos * 0.5) = 0w4194304 andalso
                                  bits(Real32.minPos + Real32.minPos) = 0w2)
val _ = check "direct int rounding" (eq(Real32.fromInt 2305843146652647425,2305843284091600896.0))
val big = IntInf.+ (IntInf.<< (1,0w80),IntInf.+ (IntInf.<< (1,0w56),1))
val _ = check "direct IntInf rounding" (bits (Real32.fromLargeInt big) = 0w1736441857)
val _ = check "decimal midpoint" (bits (read "1.00000005960464477539062500001") = 0w1065353217)
val _ = check "negative zero" (Real32.signBit (read "~0.0"))
val _ = check "min/max signed zero" (Real32.signBit (Real32.min(0.0,~0.0)) andalso
                                     Real32.signBit (Real32.min(~0.0,0.0)) andalso
                                     not (Real32.signBit (Real32.max(0.0,~0.0))) andalso
                                     not (Real32.signBit (Real32.max(~0.0,0.0))))
val _ = check "Math32 float ABI" (eq(Math32.sin 0.0,0.0) andalso eq(Math32.cos 0.0,1.0) andalso
                                  eq(Math32.pow(2.0,3.0),8.0) andalso eq(Math32.atan2(0.0,1.0),0.0))
val _ = check "remainder" (eq(Real32.rem(7.0,2.0),1.0))
val _ = check "split zero sign" (Real32.signBit (#frac (Real32.split ~2.0)))
val _ = check "large integer rounding" (Real32.toLargeInt IEEEReal.TO_NEGINF (~1.25) = ~2)
val _ = check "man/exp" (eq(Real32.fromManExp (Real32.toManExp Real32.minPos),Real32.minPos))
val _ = check "nextAfter" (bits (Real32.nextAfter(1.0,2.0)) = 0w1065353217)
val halfway = 1.000000059604644775390625 : real
val _ = check "convert up" (bits (Real32.fromLarge IEEEReal.TO_POSINF halfway) = 0w1065353217)
val _ = check "convert nearest" (bits (Real32.fromLarge IEEEReal.TO_NEAREST halfway) = 0w1065353216)
val _ = check "convert down" (bits (Real32.fromLarge IEEEReal.TO_NEGINF halfway) = 0w1065353216)
val _ = check "convert toward zero negative" (Real32.signBit (Real32.fromLarge IEEEReal.TO_ZERO (~0.0)))
val _ = check "convert toward zero tie" (bits (Real32.fromLarge IEEEReal.TO_ZERO (~halfway)) = 0w3212836864)
val _ = check "rounding restored" (IEEEReal.getRoundingMode() = IEEEReal.TO_NEAREST)
val () = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val midpointDecimal = valOf (IEEEReal.fromString "1.000000059604644775390625")
val _ = check "fromDecimal nearest" (bits(valOf(Real32.fromDecimal midpointDecimal)) = 0w1065353216)
val _ = check "scan current rounding" (bits(read "1.000000059604644775390625") = 0w1065353217)
val _ = check "decimal directed underflow" (bits (read "1.0e~1000") = 0w1)
fun add (x:Real32.real,y:Real32.real) = x+y
val _ = check "arithmetic directed rounding" (bits(add(1.0,Real32.minPos)) = 0w1065353217)
val () = IEEEReal.setRoundingMode IEEEReal.TO_ZERO
val _ = check "decimal directed overflow" (eq(read "1.0e1000",Real32.maxFinite))
val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST
val nan = PackReal32Big.fromBytes (Word8Vector.fromList [0wxff,0wxc0,0wx01,0wx23])
val _ = check "shared unordered exception" ((Real32.compare(nan,nan); false)
                                            handle IEEEReal.Unordered => true)
val values : Real32.real list = [1.5,~2.5,~0.0,Real32.minPos,Real32.maxFinite,nan]
val a = Real32Array.fromList values
val v = Real32Array.vector a
val _ = List.app (fn i => check "packed values" (bits(Real32Array.sub(a,i)) = bits(List.nth(values,i))))
                 [0,1,2,3,4,5]
val () = Real32Array.update(a,0,~4.0)
val _ = check "array copy/vector independence" (eq(Real32Array.sub(a,0),~4.0) andalso eq(Real32Vector.sub(v,0),1.5))
val s = Real32ArraySlice.slice(a,1,SOME 3)
val _ = check "slice" (Real32ArraySlice.length s = 3 andalso eq(Real32ArraySlice.sub(s,0),~2.5))
val vs = Real32VectorSlice.slice(v,1,SOME 3)
val _ = check "vector slice" (eq(Real32VectorSlice.sub(vs,0),~2.5))
val m = Real32Array2.array(2,3,~1.0)
val () = Real32Array2.update(m,1,2,3.5)
val _ = check "array2" (eq(Real32Array2.sub(m,1,2),3.5))
val _ = List.app (fn x =>
  (check "big packing" (bits(PackReal32Big.fromBytes(PackReal32Big.toBytes x)) = bits x);
   check "little packing" (bits(PackReal32Little.fromBytes(PackReal32Little.toBytes x)) = bits x))) values
val _ = check "packing endianness" (Word8Vector.foldr (op ::) [] (PackReal32Big.toBytes 1.0) =
                                     [0wx3f,0wx80,0w0,0w0])
val bytes = Word8Array.array(8,0w0)
val () = PackReal32Little.update(bytes,1,~3.5)
val _ = check "pack array" (eq(PackReal32Little.subArr(bytes,1),~3.5))
val _ = check "packing bounds" ((PackReal32Big.subVec(PackReal32Big.toBytes 1.0,valOf Int.maxInt); false)
                                handle Subscript => true)
val _ = List.app (fn x =>
  if Real32.isNan x then () else
  check "decimal roundtrip" (bits(valOf(Real32.fromDecimal(Real32.toDecimal x))) = bits x)) values
val _ = check "shortest decimal" (#digits (Real32.toDecimal 1.25) = [1,2,5])
val _ = check "fmt eager bounds" ((Real32.fmt (StringCvt.SCI (SOME ~1)); false) handle Size => true)
(* Keep narrow values live in a polymorphic list through allocations. The
 * constructors can embed low-unboxed payloads in high-unboxed data. *)
datatype narrow = Empty | Single of Real32.real | Half of Real16.real
fun halfBits (x:Real16.real) : word = prim ("id", x)
fun build (0,xs) = xs
  | build (n,xs) = build(n-1,Single(Real32.fromInt n) :: Half 1.0 :: xs)
fun walk ([],_) = ()
  | walk (Single x :: Half y :: xs,n) =
      (check "live narrow values" (eq(x,Real32.fromInt n) andalso halfBits y = 0w15360);
       walk(xs,n+1))
  | walk _ = raise Fail "narrow constructors"
val () = walk(build(2000,[]),1)
val _ = print "OK\n"
