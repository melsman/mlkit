structure ConversionLibrary =
struct
  fun fromByte (x : Word8.word) = Word8.toLargeInt x
  fun fromSignedByte (x : Word8.word) = Word8.toLargeIntX x
  fun fromInt64 (x : Int64.int) = Int64.toLarge x
end
