val bv_big = PackRealBig.toBytes 43233.32

fun test t bv =
    let val l = map Word8.toString (rev(Word8Vector.foldl (op ::) nil bv))
        val s = String.concatWith "," l
    in print (t ^ ": " ^ s ^ "</br>")
    end

val r = ~3.285232
val () = test "big" (PackRealBig.toBytes r)
val () = test "little" (PackRealLittle.toBytes r)
