signature PACK_REAL =
  sig
    type real
    val bytesPerElem : int
    val isBigEndian : bool
    val toBytes   : real -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> real
    val subVec : Word8Vector.vector * int -> real
    val subArr : Word8Array.array * int -> real
    val update : Word8Array.array * int * real -> unit
  end

(*

val bytesPerElem : int
    The number of bytes per element, sufficient to store a value of type real.

isBigEndian
    is true if the structure implements a big-endian view of the data.

val toBytes : real -> Word8Vector.vector
val fromBytes : Word8Vector.vector -> real
    These functions pack and unpack floating-point values into and out of
    Word8Vector.vector values. The function fromBytes raises the Subscript
    exception if the argument vector does not have length at least
    bytesPerElem; otherwise the first bytesPerElem bytes are used.

subVec (seq, i)
subArr (seq, i)
    These functions extract the subsequence

    seq[bytesPerElem*i..bytesPerElem*(i+1)-1]

    of the aggregate seq and convert it into a real value according to the
    endianness of the structure. They raise the Subscript exception if i < 0 or
    if Word8Array.length seq < bytesPerElem * (i + 1).

update (arr, i, r)
    stores r into the bytes bytesPerElem*i through bytesPerElem*(i+1)-1 of the
    array arr, according to the structure's endianness. It raises the Subscript
    exception if i < 0 or if Word8Array.length arr < bytesPerElem * (i + 1).

*)
