(* Encode bytes explicitly so packing is independent of host byte order.
 * Two-byte reads and writes retain all NaN payload and sign bits. *)
functor PackReal16(val isBigEndian : bool) : PACK_REAL where type real = Real16.real =
struct
  type real = Real16.real
  val isBigEndian = isBigEndian
  val bytesPerElem = 2
  fun bits (x:real) : word = prim ("id", x)
  fun fromBits (x:word) : real = prim ("id", x)
  fun shift j = Word.fromInt (8 * (if isBigEndian then 1-j else j))
  fun byte (w,j) = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,shift j),0w255)))
  fun toBytes x = Word8Vector.tabulate(2,fn j => byte(bits x,j))
  fun read sub (v,i) =
      let fun loop (2,w) = fromBits w
            | loop (j,w) = loop(j+1,Word.orb(w,Word.<<(Word.fromInt(Word8.toInt(sub(v,i+j))),shift j)))
      in loop(0,0w0)
      end
  fun check (length,i) =
      if i < 0 orelse i >= length div 2 then raise Subscript else 2*i
  fun fromBytes v =
      if Word8Vector.length v < 2 then raise Subscript else read Word8Vector.sub (v,0)
  fun subVec (v,i) = read Word8Vector.sub (v,check(Word8Vector.length v,i))
  fun subArr (a,i) = read Word8Array.sub (a,check(Word8Array.length a,i))
  fun update (a,i,x) =
      let val offset = check(Word8Array.length a,i)
          val w = bits x
          fun loop 2 = ()
            | loop j = (Word8Array.update(a,offset+j,byte(w,j)); loop(j+1))
      in loop 0
      end
end

(** SigDoc *)
structure PackReal16Little = PackReal16(val isBigEndian = false)
(** SigDoc *)
structure PackReal16Big = PackReal16(val isBigEndian = true)
