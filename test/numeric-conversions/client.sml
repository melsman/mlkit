fun require b = if b then () else raise Fail "conversion regression"
val input = case CommandLine.arguments() of
                s::_ => valOf(Int.fromString s)
              | _ => 255
val byte = Word8.fromInt input
val () = require (Int32.fromLarge(ConversionLibrary.fromByte byte) = 255)
val () = require (Int32.fromLarge(ConversionLibrary.fromSignedByte byte) = ~1)
val () = require(Word32.fromLargeWord(Word8.toLargeWord byte) = 0w255)
val () = require(Word32.fromLargeWord(Word8.toLargeWordX byte) = 0wxFFFFFFFF)
val calls = ref 0
fun next () = (calls := !calls + 1; byte)
val intermediate = ConversionLibrary.fromByte(next())
val alias = intermediate
val a = Int32.fromLarge alias
val b = Word32.fromLargeInt alias
val () = require (a = 255 andalso b = 0w255 andalso !calls = 1)
(* The intermediate has another consumer and must remain available. *)
val () = require (intermediate = 255)
val () = calls := 0
val large = valOf(Int64.fromString "2147483648")
val () = ((let val n = Int32.fromLarge(ConversionLibrary.fromInt64 large)
               val () = calls := !calls + 1
           in require(n = 0)
           end; raise Fail "missing Overflow")
          handle Overflow => require(!calls = 0))
(* Non-composed conversions must still lower when optimization is disabled. *)
val huge = valOf(IntInf.fromString "~18446744073709551617")
val () = require(Word64.fromLargeInt huge = 0wxFFFFFFFFFFFFFFFF)
val () = require(Word63.fromLargeInt huge = 0wx7FFFFFFFFFFFFFFF)
val () = print "conversion client: OK\n"
