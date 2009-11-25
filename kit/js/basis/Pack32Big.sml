(* Pack32Big.sml
 *
 * Implementation originates from SML/NJ;  mael 2001-10-31
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the non-native implementation of 32-bit big-endian packing
 * operations.
 *
 *)

(** 32-bit big-endian packing operations. *)
structure PackWord32Big : PACK_WORD =
  struct
    structure W = LargeWord
    structure W8 = Word8
    structure W8V = Word8Vector
    structure W8A = Word8Array

    val bytesPerElem = 4
    val isBigEndian = true

  (* convert the byte length into word32 length (n div 4), and check the index *)
    fun chkIndex (len, i) = let
	  val len = Word.>>(Word.fromInt len, 0w2)
	  in
	    if Word.fromInt i < len then () else raise Subscript
	  end

    fun mkWord (b1, b2, b3, b4) =
	  W.orb (W.<<(Word8.toLargeWord b1, 0w24),
	  W.orb (W.<<(Word8.toLargeWord b2, 0w16),
	  W.orb (W.<<(Word8.toLargeWord b3,  0w8),
		      Word8.toLargeWord b4)))

    fun subVec (vec, i) = let
	  val _ = chkIndex (W8V.length vec, i)
	  val k = Word.toIntX(Word.<<(Word.fromInt i, 0w2))
	  in
	    mkWord (W8V.sub(vec, k), W8V.sub(vec, k+1),
	      W8V.sub(vec, k+2), W8V.sub(vec, k+3))
	  end
  (* since LargeWord is 32-bits, no sign extension is required *)
    fun subVecX(vec, i) = subVec (vec, i)

    fun subArr (arr, i) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = Word.toIntX(Word.<<(Word.fromInt i, 0w2))
	  in
	    mkWord (W8A.sub(arr, k), W8A.sub(arr, k+1),
	      W8A.sub(arr, k+2), W8A.sub(arr, k+3))
	  end
  (* since LargeWord is 32-bits, no sign extension is required *)
    fun subArrX(arr, i) = subArr (arr, i)

    fun update (arr, i, w) = let
	  val _ = chkIndex (W8A.length arr, i)
	  val k = Word.toIntX(Word.<<(Word.fromInt i, 0w2))
	  in
	    W8A.update (arr, k,   W8.fromLargeWord(W.>>(w, 0w24)));
	    W8A.update (arr, k+1, W8.fromLargeWord(W.>>(w, 0w16)));
	    W8A.update (arr, k+2, W8.fromLargeWord(W.>>(w,  0w8)));
	    W8A.update (arr, k+3, W8.fromLargeWord w)
	  end

  end
