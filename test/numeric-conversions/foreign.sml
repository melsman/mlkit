(* An ordinary C symbol must not be decoded as an internal conversion. *)
fun foreignConversion (input : word) =
    Word.toLargeInt (prim("wordcode_to_word", input) : word)
val () = if foreignConversion 0w7 = IntInf.fromInt 7 then
           print "foreign conversion: OK\n"
         else raise Fail "foreign conversion result"
