(* Encode bytes explicitly so packing is independent of host byte order.
 * Four-byte reads and writes retain all NaN payload and sign bits. *)
functor PackReal32(val isBigEndian : bool) : PACK_REAL where type real = Real32.real =
struct
  type real = Real32.real
  val isBigEndian = isBigEndian
  val bytesPerElem = 4
  fun bits (x:real) : word = prim ("id", x)
  fun fromBits (x:word) : real = prim ("id", x)
  (* Public operations validate the complete four-byte range before using
   * these private accesses. Arrays and vectors have immutable lengths. *)
  val unsafeSubVec = PackRealBytes.subVec
  val unsafeSubArr = PackRealBytes.subArr
  val unsafeUpdate = PackRealBytes.update
  fun shift j = Word.fromInt (8 * (if isBigEndian then 3-j else j))
  fun byte (w,j) = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,shift j),0w255)))
  fun toBytes x = Word8Vector.tabulate(4,fn j => byte(bits x,j))
  (* Spell out the fixed-width accesses to permit inlining and constant shifts
   * without relying on recursive-loop unrolling. *)
  fun read sub (v,i) =
      let val b0 = Word.fromInt (Word8.toInt (sub(v,i)))
          val b1 = Word.fromInt (Word8.toInt (sub(v,i+1)))
          val b2 = Word.fromInt (Word8.toInt (sub(v,i+2)))
          val b3 = Word.fromInt (Word8.toInt (sub(v,i+3)))
      in fromBits (if isBigEndian
                   then Word.orb(Word.orb(Word.<<(b0,0w24),Word.<<(b1,0w16)),
                                 Word.orb(Word.<<(b2,0w8),b3))
                   else Word.orb(Word.orb(b0,Word.<<(b1,0w8)),
                                 Word.orb(Word.<<(b2,0w16),Word.<<(b3,0w24))))
      end
  (* Lengths are nonnegative. Check the element index before scaling: a valid
   * index satisfies 4*i <= length-4, so both additions below are safe. *)
  fun check (length,i) =
      if i < 0 orelse Word.fromInt i >= Word.>>(Word.fromInt length,0w2)
      then raise Subscript
      else
        let val twice = i+i
        in twice+twice
        end
  fun fromBytes v =
      if Word8Vector.length v < 4 then raise Subscript else read unsafeSubVec (v,0)
  fun subVec (v,i) = read unsafeSubVec (v,check(Word8Vector.length v,i))
  fun subArr (a,i) = read unsafeSubArr (a,check(Word8Array.length a,i))
  fun update (a,i,x) =
      let val offset = check(Word8Array.length a,i)
          val w = bits x
          val b0 = Word8.fromInt (Word.toInt (Word.andb(w,0w255)))
          val b1 = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,0w8),0w255)))
          val b2 = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,0w16),0w255)))
          val b3 = Word8.fromInt (Word.toInt (Word.andb(Word.>>(w,0w24),0w255)))
      in if isBigEndian
         then (unsafeUpdate(a,offset,b3); unsafeUpdate(a,offset+1,b2);
               unsafeUpdate(a,offset+2,b1); unsafeUpdate(a,offset+3,b0))
         else (unsafeUpdate(a,offset,b0); unsafeUpdate(a,offset+1,b1);
               unsafeUpdate(a,offset+2,b2); unsafeUpdate(a,offset+3,b3))
      end
end

(** SigDoc *)
structure PackReal32Little : PACK_REAL where type real = Real32.real = PackReal32(val isBigEndian = false)
(** SigDoc *)
structure PackReal32Big : PACK_REAL where type real = Real32.real = PackReal32(val isBigEndian = true)
