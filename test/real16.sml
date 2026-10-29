fun check name b = if b then () else raise Fail name
fun bits (x:Real16.real) : word = prim ("id", x)
fun eq (x,y) = Real16.== (x,y)
fun read s = valOf (Real16.fromString s)
val _ = check "precision" (Real16.precision = 11 andalso Real16.radix = 2)
val _ = check "Math sharing" (eq(Real16.Math.pi,Math16.pi))
val _ = check "constants" (Real16.isNormal Real16.minNormalPos andalso
                           Real16.class Real16.minPos = IEEEReal.SUBNORMAL andalso
                           not (Real16.isFinite Real16.posInf))
val _ = check "gradual underflow" (bits(Real16.minNormalPos * 0.5) = 0w512 andalso
                                  bits(Real16.minPos + Real16.minPos) = 0w2)
val _ = check "direct int rounding" (eq(Real16.fromInt 2049,2048.0))
val _ = check "direct IntInf rounding" (eq(Real16.fromLargeInt 2051,2052.0))
val _ = check "decimal midpoint" (bits (read "1.00048828125000000001") = 0w15361)
val _ = check "negative zero" (Real16.signBit (read "~0.0"))
val _ = check "min/max signed zero" (Real16.signBit (Real16.min(0.0,~0.0)) andalso
                                     Real16.signBit (Real16.min(~0.0,0.0)) andalso
                                     not (Real16.signBit (Real16.max(0.0,~0.0))) andalso
                                     not (Real16.signBit (Real16.max(~0.0,0.0))))
val _ = check "Math16" (eq(Math16.sin 0.0,0.0) andalso eq(Math16.cos 0.0,1.0) andalso
                                  eq(Math16.pow(2.0,3.0),8.0) andalso eq(Math16.atan2(0.0,1.0),0.0))
val _ = check "remainder" (eq(Real16.rem(7.0,2.0),1.0))
val _ = check "split zero sign" (Real16.signBit (#frac (Real16.split ~2.0)))
val _ = check "large integer rounding" (Real16.toLargeInt IEEEReal.TO_NEGINF (~1.25) = ~2)
val _ = check "man/exp" (eq(Real16.fromManExp (Real16.toManExp Real16.minPos),Real16.minPos))
val _ = check "nextAfter" (bits (Real16.nextAfter(1.0,2.0)) = 0w15361)
val halfway = 1.00048828125 : real
val _ = check "convert up" (bits (Real16.fromLarge IEEEReal.TO_POSINF halfway) = 0w15361)
val _ = check "convert nearest" (bits (Real16.fromLarge IEEEReal.TO_NEAREST halfway) = 0w15360)
val _ = check "convert down" (bits (Real16.fromLarge IEEEReal.TO_NEGINF halfway) = 0w15360)
val _ = check "rounding restored" (IEEEReal.getRoundingMode() = IEEEReal.TO_NEAREST)
val () = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val midpointDecimal = valOf (IEEEReal.fromString "1.00048828125")
val _ = check "fromDecimal nearest" (bits(valOf(Real16.fromDecimal midpointDecimal)) = 0w15360)
val _ = check "scan current rounding" (bits(read "1.00048828125") = 0w15361)
val _ = check "decimal directed underflow" (bits (read "1.0e~1000") = 0w1)
fun add (x:Real16.real,y:Real16.real) = x+y
val _ = check "arithmetic directed rounding" (bits(add(1.0,Real16.minPos)) = 0w15361)
val () = IEEEReal.setRoundingMode IEEEReal.TO_ZERO
val _ = check "decimal directed overflow" (eq(read "1.0e1000",Real16.maxFinite))
val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST
val nan = PackReal16Big.fromBytes (Word8Vector.fromList [0wxfe,0wx23])
val values : Real16.real list = [1.5,~2.5,~0.0,Real16.minPos,Real16.maxFinite,nan]
val a = Real16Array.fromList values
val v = Real16Array.vector a
val _ = List.app (fn i => check "packed values" (bits(Real16Array.sub(a,i)) = bits(List.nth(values,i))))
                 [0,1,2,3,4,5]
val () = Real16Array.update(a,0,~4.0)
val _ = check "array copy/vector independence" (eq(Real16Array.sub(a,0),~4.0) andalso eq(Real16Vector.sub(v,0),1.5))
val s = Real16ArraySlice.slice(a,1,SOME 3)
val _ = check "slice" (Real16ArraySlice.length s = 3 andalso eq(Real16ArraySlice.sub(s,0),~2.5))
val vs = Real16VectorSlice.slice(v,1,SOME 3)
val _ = check "vector slice" (eq(Real16VectorSlice.sub(vs,0),~2.5))
val m = Real16Array2.array(2,3,~1.0)
val () = Real16Array2.update(m,1,2,3.5)
val _ = check "array2" (eq(Real16Array2.sub(m,1,2),3.5))
val _ = List.app (fn x =>
  (check "big packing" (bits(PackReal16Big.fromBytes(PackReal16Big.toBytes x)) = bits x);
   check "little packing" (bits(PackReal16Little.fromBytes(PackReal16Little.toBytes x)) = bits x))) values
val _ = check "packing endianness" (Word8Vector.foldr (op ::) [] (PackReal16Big.toBytes 1.0) =
                                     [0wx3c,0w0])
val bytes = Word8Array.array(8,0w0)
val () = PackReal16Little.update(bytes,1,~3.5)
val _ = check "pack array" (eq(PackReal16Little.subArr(bytes,1),~3.5))
val _ = check "packing bounds" ((PackReal16Big.subVec(PackReal16Big.toBytes 1.0,valOf Int.maxInt); false)
                                handle Subscript => true)
val _ = List.app (fn x =>
  if Real16.isNan x then () else
  check "decimal roundtrip" (bits(valOf(Real16.fromDecimal(Real16.toDecimal x))) = bits x)) values
val _ = check "shortest decimal" (#digits (Real16.toDecimal 1.25) = [1,2,5])
val _ = check "fmt eager bounds" ((Real16.fmt (StringCvt.SCI (SOME ~1)); false) handle Size => true)
val _ = check "half division rounding" (bits(1.0/3.0:Real16.real) = 0w13653)
val _ = check "half sqrt rounding" (bits(Math16.sqrt 2.0) = 0w15784)
val _ = check "nextAfter negative" (bits(Real16.nextAfter(~1.0,~2.0)) = 0w48129)
val _ = check "array bounds" ((Real16Array.sub(a,Real16Array.length a); false)
                              handle Subscript => true)
val _ = check "slice bounds" ((Real16ArraySlice.slice(a,0,SOME 100); false)
                              handle Subscript => true)
(* Keep narrow values live in a polymorphic list through allocations. The
 * constructors can embed low-unboxed payloads in high-unboxed data. *)
datatype narrow = Empty | Single of Real16.real | Half of Real32.real
fun halfBits (x:Real32.real) : word = prim ("id", x)
fun build (0,xs) = xs
  | build (n,xs) = build(n-1,Single(Real16.fromInt n) :: Half 1.0 :: xs)
fun walk ([],_) = ()
  | walk (Single x :: Half y :: xs,n) =
      (check "live narrow values" (eq(x,Real16.fromInt n) andalso halfBits y = 0w1065353216);
       walk(xs,n+1))
  | walk _ = raise Fail "narrow constructors"
val () = walk(build(2000,[]),1)
val _ = print "OK\n"
