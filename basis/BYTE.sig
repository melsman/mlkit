(** Conversion operations between Word8.word values and characters.

Bytes are 8-bit integers as provided by the Word8 structure, but serve
the dual role as elements composing the extended ASCII character
set. The Byte structure provides functions for converting values
between these two roles.
*)

signature BYTE =
  sig
    val byteToChar      : Word8.word -> char
    val charToByte      : char -> Word8.word
    val bytesToString   : Word8Vector.vector -> string
    val stringToBytes   : string -> Word8Vector.vector
    val unpackStringVec : Word8VectorSlice.slice -> string
    val unpackString    : Word8ArraySlice.slice -> string
    val packString      : Word8Array.array * int * substring -> unit
  end

(**

[byteToChar i] returns the character whose code is i.

[charToByte c] returns an 8-bit word holding the code for the
character c.

[bytesToString v]

[stringToBytes s] These functions convert between a vector of
character codes and the corresponding string. Note that these
functions do not perform end-of-line, or other character,
translations. The semantics of these functions can be defined as
follows, although one expects actual implementations will be more
efficient:

    fun bytesToString bv =
      CharVector.tabulate(
        Word8Vector.length bv,
        fn i => byteToChar(Word8Vector.sub(bv, i)))
    fun stringToBytes s =
      Word8Vector.tabulate(
        String.size s,
        fn i => charToByte(String.sub(s, i)))

For implementations where the underlying representation of the
Word8Vector.vector and string types are the same, these functions
should be constant-time operations.

[unpackStringVec slice] returns the string consisting of characters
whose codes are held in the vector slice slice.

[unpackString slice] returns the string consisting of characters whose
codes are held in the array slice slice.

[packString (arr, i, s)] puts the substring s into the array arr
starting at offset i. It raises Subscript if i < 0 or size s + i >
|arr|.

*)
