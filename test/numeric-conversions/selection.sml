(* These exported functions must contain no IntInf conversion or list allocation
 * after optimization, including when the incoming operation is imported. *)
fun byteToInt32 (x : Word8.word) : Int32.int =
    Int32.fromLarge(ConversionLibrary.fromByte x)
fun byteToInt64 (x : Word8.word) : Int64.int =
    let val large = ConversionLibrary.fromByte x
        val alias = large
    in Int64.fromLarge alias
    end
fun int64ToInt32 (x : Int64.int) : Int32.int =
    Int32.fromLarge(ConversionLibrary.fromInt64 x)
