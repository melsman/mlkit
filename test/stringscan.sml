(* String.scan, fromString and fromCString. *)
fun p s = print (s ^ "\n")
fun so NONE = "NONE"
  | so (SOME s) = "SOME \"" ^ String.toString s ^ "\""
fun sr NONE = "NONE"
  | sr (SOME (s, rest)) = "SOME (\"" ^ String.toString s ^ "\", \"" ^ String.toString (Substring.string rest) ^ "\")"
fun fs s = p ("fromString \"" ^ String.toString s ^ "\" = " ^ so (String.fromString s))
fun fc s = p ("fromCString \"" ^ String.toString s ^ "\" = " ^ so (String.fromCString s))
fun scan s = p ("scan \"" ^ String.toString s ^ "\" = " ^ sr (String.scan Substring.getc (Substring.full s)))

val () = (fs "\\q"; fs "a\^D"; fs "a\\ \\\\q"; fs "\\ \\"; fs ""; fs "\\ \\\^D"; fs "\\ a"; fs "\^D")
val () = (fs "  ab"; fs "\tab"; fs "\nab"; fs ("ab" ^ str (Char.chr 4) ^ "cd"); fs "ab\\ncd\\u0041\\065\\^A"; fs "a\\ \n \\b")
val () = (scan "ab\\ncd"; scan "ab\\q"; scan "\\ \\\^Dx"; scan "\^Dx"; scan ""; scan "a\\ \\")
val () = (fc "a'b"; fc "a\"b"; fc "\""; fc ""; fc "a\\nb\\x41\\101"; fc "a\^Db"; fc "\\q"; fc "ab\\")
val () = p ("toString/fromString round trip: " ^ (if String.fromString (String.toString "a\n\t\"\\\255\000") = SOME "a\n\t\"\\\255\000" then "ok" else "bad"))
