(* Packing preserves raw bits, including signalling NaNs and signed zero. *)
functor CheckNarrowPack(A : sig
  structure P : PACK_REAL
  val bits : P.real -> word
  val one : P.real
  val patterns : (word -> unit) -> unit
end) =
struct
  structure P = A.P
  val width = P.bytesPerElem
  fun check name condition = if condition then () else raise Fail name
  fun subscript action =
      check "expected Subscript" ((action (); false) handle Subscript => true)
  fun pattern w =
      let val expected = Word8Vector.tabulate(width,fn j =>
              Word8.fromInt (Word.toInt (Word.andb(
                Word.>>(w,Word.fromInt (8*(if P.isBigEndian then width-1-j else j))),0w255))))
          val x = P.fromBytes expected
          val array = Word8Array.array(3*width+1,0w165)
          val () = check "fromBytes bits" (A.bits x = w)
          val () = check "toBytes byte order" (P.toBytes x = expected)
          val () = P.update(array,1,x)
          val () = Word8Array.appi (fn (i,b) =>
              check "update bytes and neighbours"
                (b = (if i >= width andalso i < 2*width
                      then Word8Vector.sub(expected,i-width) else 0w165))) array
          val () = check "subArr bits" (A.bits(P.subArr(array,1)) = w)
          val () = check "subVec bits" (A.bits(P.subVec(Word8Array.vector array,1)) = w)
      in ()
      end
  val () = A.patterns pattern
  fun bounds n =
      let val array = Word8Array.array(n,0w165)
          val vector = Word8Array.vector array
          val invalid = [~1,valOf Int.minInt,valOf Int.maxInt,n div width,n div width+1]
          fun index i =
              (subscript (fn () => ignore(P.subArr(array,i)));
               subscript (fn () => ignore(P.subVec(vector,i)));
               subscript (fn () => P.update(array,i,A.one));
               check "failed update changed bytes" (Word8Array.vector array = vector))
          val () = List.app index invalid
          val () = if n < width then () else
              let val last = n div width-1
                  val () = P.update(array,last,A.one)
                  val () = check "last subArr" (A.bits(P.subArr(array,last)) = A.bits A.one)
                  val () = check "last subVec" (A.bits(P.subVec(Word8Array.vector array,last)) = A.bits A.one)
              in check "trailing bytes" (Word8ArraySlice.all (fn b => b = 0w165)
                   (Word8ArraySlice.slice(array,(n div width)*width,NONE)))
              end
          fun sentinel (0,w) = w
            | sentinel (left,w) = sentinel(left-1,Word.orb(Word.<<(w,0w8),0w165))
      in if n < width then subscript (fn () => ignore(P.fromBytes vector))
         else check "fromBytes ignores trailing bytes" (A.bits(P.fromBytes vector) = sentinel(width,0w0))
      end
  val () = List.app bounds (List.tabulate(4*width+1,fn n => n))
end

fun patterns16 f =
    let fun loop n = if n = 65536 then () else (f (Word.fromInt n); loop(n+1))
    in loop 0
    end
fun patterns32 f =
    let
      val () = List.app f [0wx00000000,0wx80000000,0wx00000001,0wx80000001,
                           0wx007fffff,0wx807fffff,0wx00800000,0wx80800000,
                           0wx3f800000,0wxbf800000,0wx7f7fffff,0wxff7fffff,
                           0wx7f800000,0wxff800000,0wx7f800001,0wxff800001,
                           0wx7fc00000,0wxffc00000,0wx7fffffff,0wxffffffff]
      (* Exercise every high/low halfword and varying sign/exponent/payload. *)
      fun halves n =
          if n = 65536 then ()
          else
            let val w = Word.fromInt n
                val high = Word.<<(w,0w16)
            in f w; f high; f (Word.orb(high,Word.xorb(w,0wxffff))); halves(n+1)
            end
      fun random (0,_) = ()
        | random (n,w) =
            let val a = Word.andb(Word.xorb(w,Word.<<(w,0w13)),0wxffffffff)
                val b = Word.xorb(a,Word.>>(a,0w17))
                val c = Word.andb(Word.xorb(b,Word.<<(b,0w5)),0wxffffffff)
            in f c; random(n-1,c)
            end
    in halves 0; random(10000,0wx12345678)
    end
structure CheckLittle16 = CheckNarrowPack(struct
  structure P = PackReal16Little
  fun bits (x:Real16.real) : word = prim ("id", x)
  val one = 1.0 : Real16.real
  val patterns = patterns16
end)
structure CheckBig16 = CheckNarrowPack(struct
  structure P = PackReal16Big
  fun bits (x:Real16.real) : word = prim ("id", x)
  val one = 1.0 : Real16.real
  val patterns = patterns16
end)
structure CheckLittle32 = CheckNarrowPack(struct
  structure P = PackReal32Little
  fun bits (x:Real32.real) : word = prim ("id", x)
  val one = 1.0 : Real32.real
  val patterns = patterns32
end)
structure CheckBig32 = CheckNarrowPack(struct
  structure P = PackReal32Big
  fun bits (x:Real32.real) : word = prim ("id", x)
  val one = 1.0 : Real32.real
  val patterns = patterns32
end)
val () = print "OK\n"
