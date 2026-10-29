(* Encode bytes explicitly so packing is independent of host byte order.
 * Four-byte reads and writes retain all NaN payload and sign bits. *)
functor PackReal32(val isBigEndian : bool) : PACK_REAL where type real = Real32.real =
struct
  type real = Real32.real
  val isBigEndian = isBigEndian
  val bytesPerElem = 4
  fun bits (x:real) : word = prim ("id", x)
  fun fromBits (x:word) : real = prim ("id", x)
  fun shift j = Word.fromInt (8 * (if isBigEndian then 3-j else j))
  fun byte (w,j) = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,shift j),0w255)))
  fun toBytes x = Word8Vector.tabulate(4,fn j => byte(bits x,j))
  fun read sub (v,i) =
      let fun loop (4,w) = fromBits w
            | loop (j,w) = loop(j+1,Word.orb(w,Word.<<(Word.fromInt(Word8.toInt(sub(v,i+j))),shift j)))
      in loop(0,0w0)
      end
  fun check (length,i) =
      if i < 0 orelse i >= length div 4 then raise Subscript else 4*i
  fun fromBytes v =
      if Word8Vector.length v < 4 then raise Subscript else read Word8Vector.sub (v,0)
  fun subVec (v,i) = read Word8Vector.sub (v,check(Word8Vector.length v,i))
  fun subArr (a,i) = read Word8Array.sub (a,check(Word8Array.length a,i))
  fun update (a,i,x) =
      let val offset = check(Word8Array.length a,i)
          val w = bits x
          fun loop 4 = ()
            | loop j = (Word8Array.update(a,offset+j,byte(w,j)); loop(j+1))
      in loop 0
      end
end

(** SigDoc *)
structure PackReal32Little = PackReal32(val isBigEndian = false)
(** SigDoc *)
structure PackReal32Big = PackReal32(val isBigEndian = true)
