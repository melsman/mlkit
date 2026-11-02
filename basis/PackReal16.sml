(* Encode bytes explicitly so packing is independent of host byte order.
 * Two-byte reads and writes retain all NaN payload and sign bits. *)
functor PackReal16(val isBigEndian : bool) : PACK_REAL where type real = Real16.real =
struct
  type real = Real16.real
  val isBigEndian = isBigEndian
  val bytesPerElem = 2
  fun bits (x:real) : word = prim ("id", x)
  fun fromBits (x:word) : real = prim ("id", x)
  (* These private operations are only used after validating the complete
   * two-byte range. Neither arrays nor vectors can change length afterwards. *)
  val unsafeSubVec = PackRealBytes.subVec
  val unsafeSubArr = PackRealBytes.subArr
  val unsafeUpdate = PackRealBytes.update
  fun shift j = Word.fromInt (8 * (if isBigEndian then 1-j else j))
  fun byte (w,j) = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,shift j),0w255)))
  fun toBytes x = Word8Vector.tabulate(2,fn j => byte(bits x,j))
  (* Keep the fixed-width read straight-line so shifts and byte accesses can
   * be inlined without relying on recursive-loop unrolling. *)
  fun read sub (v,i) =
      let val first = Word.fromInt (Word8.toInt (sub(v,i)))
          val second = Word.fromInt (Word8.toInt (sub(v,i+1)))
      in fromBits (if isBigEndian
                   then Word.orb(Word.<<(first,0w8),second)
                   else Word.orb(first,Word.<<(second,0w8)))
      end
  (* Byte lengths are nonnegative, so an unsigned shift gives the element
   * count. Check before scaling: invalid large indices must raise Subscript,
   * not Overflow. A valid index satisfies 2*i <= length-2. *)
  fun check (length,i) =
      if i < 0 orelse Word.fromInt i >= Word.>>(Word.fromInt length,0w1)
      then raise Subscript else i+i
  fun fromBytes v =
      if Word8Vector.length v < 2 then raise Subscript else read unsafeSubVec (v,0)
  fun subVec (v,i) = read unsafeSubVec (v,check(Word8Vector.length v,i))
  fun subArr (a,i) = read unsafeSubArr (a,check(Word8Array.length a,i))
  fun update (a,i,x) =
      let val offset = check(Word8Array.length a,i)
          val w = bits x
          val low = Word8.fromInt (Word.toInt (Word.andb(w,0w255)))
          val high = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,0w8),0w255)))
      in if isBigEndian
         then (unsafeUpdate(a,offset,high); unsafeUpdate(a,offset+1,low))
         else (unsafeUpdate(a,offset,low); unsafeUpdate(a,offset+1,high))
      end
end

(** SigDoc *)
structure PackReal16Little : PACK_REAL where type real = Real16.real = PackReal16(val isBigEndian = false)
(** SigDoc *)
structure PackReal16Big : PACK_REAL where type real = Real16.real = PackReal16(val isBigEndian = true)
