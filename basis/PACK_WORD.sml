signature PACK_WORD =
  sig
    val bytesPerElem: int 
    val isBigEndian: bool 
    val subVec: Word8Vector.vector * int -> LargeWord.word 
    val subVecX: Word8Vector.vector * int -> LargeWord.word 
    val subArr: Word8Array.array * int -> LargeWord.word 
    val subArrX: Word8Array.array * int -> LargeWord.word 
    val update: Word8Array.array * int * LargeWord.word -> unit
  end

(* 
 The Pack{N}Big and Pack{N}Little structures provide facilities for
 packing and unpacking N-bit word elements into Word8 vectors. The
 Pack{N}Big structures perform big-endian packing and unpacking, and
 the Pack{N}Little structures perform little-endian packing and
 unpacking. In a big-endian system, the word order is
 high-byte/low-byte; in a little-endian system, it is
 low-byte/high-byte.

 [bytesPerElem] is the number of bytes per element. Most
 implementations will provide several structures with values of
 bytesPerElem that are small powers of two (e.g., 1, 2, 4, and 8,
 corresponding to N of 8, 16, 32, 64, respectively).

 [isBigEndian] is true if the structure implements a big-endian view
 of the data.

 [subVec (vec, i)] [subVecX (vec, i)] extracts the subvector
 vec[bytesPerElem*i .. bytesPerElem*i+bytesPerElem-1] of the vector
 vec and converts it into a word according to the endianess of the
 structure. The subVecX version extends the sign bit (most significant
 bit) when converting the subvector to a word. Raise exception
 Subscript if i < 0, or if (Word8Vector.length(vec) / bytesPerElem) <=
 i.

 [subArr (arr, i)] [subArrX (arr, i)] extracts the subvector
 arr[bytesPerElem*i .. bytesPerElem*i+bytesPerElem-1] of the array arr
 and converts it into a word according to the endianess of the
 structure. The subArrX version extends the sign bit (most significant
 bit) when converting the subarray into a word. Raises exception
 Subscript if i < 0 or if (Word8Array.length(arr) / bytesPerElem) <=
 i.

 [update (arr, i, j)] stores the bytesPerElem low-order bytes of the
 word w into the bytes bytesPerElem*i through bytesPerElem*(i+1)-1 of
 the array arr, according to the structure's endianess. Raises
 exception Subscript if i < 0 or if (Word8Array.length(arr) /
 bytesPerElem) <= i.  
*)
