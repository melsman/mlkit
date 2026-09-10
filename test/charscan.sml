(* Char.scan, fromString and fromCString: printable characters, escape
 * sequences, escaped formatting sequences, and what is rejected. *)
fun p s = print (s ^ "\n")
fun sc NONE = "NONE"
  | sc (SOME c) = "SOME " ^ Char.toString c
fun scr NONE = "NONE"
  | scr (SOME (c, rest)) = "SOME (" ^ Char.toString c ^ ", \"" ^ String.toString (Substring.string rest) ^ "\")"
fun fs s = p ("fromString \"" ^ String.toString s ^ "\" = " ^ sc (Char.fromString s))
fun fc s = p ("fromCString \"" ^ String.toString s ^ "\" = " ^ sc (Char.fromCString s))
fun scan s = p ("scan \"" ^ String.toString s ^ "\" = " ^ scr (Char.scan Substring.getc (Substring.full s)))

val () = (fs "a"; fs "\\q"; fs "a\^D"; fs "a\\ \\\\q"; fs "\\ \\"; fs ""; fs "\\ \\\^D"; fs "\\ a")
val () = (fs "\\a"; fs "\\b"; fs "\\t"; fs "\\n"; fs "\\v"; fs "\\f"; fs "\\r"; fs "\\\""; fs "\\\\")
val () = (fs "\\^@"; fs "\\^H"; fs "\\^_"; fs "\\255"; fs "\\u0041"; fs "\\u00ff"; fs "\\u0100"; fs "\\256")
val () = (fs "\^D"; fs "\n"; fs " "; fs "\\^?"; fs "\\^a"; fs "\\12"; fs "\\u41"; fs "\\")
val () = (scan "\\ \\ab"; scan "a\\ \\b"; scan "\\\n\t \\Z"; scan "a\\ b"; scan "ab"; scan "\\ \\"; scan "\\ \\\\ \\x\\ \\y")
val () = (fc "\\n"; fc "\\101"; fc "a"; fc "\\a"; fc "\\b"; fc "\\t"; fc "\\v"; fc "\\f"; fc "\\r"; fc "\\?"; fc "\\'"; fc "\\\""; fc "\\\\"; fc "\\^H")
val () = (fc "\\1"; fc "\\12"; fc "\\377"; fc "\\10"; fc "\\x41"; fc "\\xff"; fc "\\q"; fc ""; fc "\^D"; fc "'"; fc "\""; fc "\\400"; fc "\\x100")
