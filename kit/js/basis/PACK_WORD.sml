signature PACK_WORD = 
  sig
    val bytesPerElem : int
    val isBigEndian : bool
    val subVec  : Word8Vector.vector * int -> LargeWord.word
    val subVecX : Word8Vector.vector * int -> LargeWord.word
    val subArr  : Word8Array.array * int -> LargeWord.word
    val subArrX : Word8Array.array * int -> LargeWord.word
    val update : Word8Array.array * int * LargeWord.word
                   -> unit
  end

(*
val bytesPerElem : int
    The number of bytes per element. Most implementations will provide several
    structures with values of bytesPerElem that are small powers of two (e.g.,
    1, 2, 4, and 8, corresponding to N of 8, 16, 32, 64, respectively).

val isBigEndian : bool
    True if the structure implements a big-endian view of the data
    (most-significant byte first). Otherwise, the structure implements a
    little-endian view (least-significant byte first).

subVec (vec, i)
subVecX (vec, i)
    These extract the subvector

vec[bytesPerElem*i..bytesPerElem*(i+1)-1]

    of the vector vec and convert it into a word according to the endianness of
    the structure.  The subVecX version extends the sign bit (most significant
    bit) when converting the subvector to a word. The functions raise the
    Subscript exception if i < 0 or if Word8Vector.length vec < bytesPerElem *
    (i + 1).

subArr (arr, i)
subArrX (arr, i)
    These extract the subarray

arr[bytesPerElem*i..bytesPerElem*(i+1)-1]

    of the array arr and convert it into a word according to the endianness of
    the structure.  The subArrX version extends the sign bit (most significant
    bit) when converting the subarray into a word. The functions raise the
    Subscript exception if i < 0 or if Word8Array.length arr < bytesPerElem *
    (i+1).

update (arr, i, w)
    stores the bytesPerElem low-order bytes of the word w into the bytes
    bytesPerElem*i through bytesPerElem*(i+1)-1 of the array arr, according to
    the structure's endianness. It raises the Subscript exception if i < 0 or
    if Word8Array.length arr < bytesPerElem * (i+1).

*)
