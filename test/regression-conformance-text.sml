local
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

in
end
local
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

in
end
local
(* Bounds checks of the form i+n <= size overflow for a huge n and raised
 * Overflow instead of Subscript. *)
fun p s = print (s ^ "\n")
fun t f = (f (); "no exception") handle Subscript => "Subscript" | Overflow => "Overflow" | Size => "Size"
val big = valOf Int.maxInt
val () = p ("Substring.slice huge n: " ^ t (fn () => Substring.slice (Substring.full "abcde", 1, SOME big)))
val () = p ("Substring.slice huge i: " ^ t (fn () => Substring.slice (Substring.full "abcde", big, SOME 1)))
val () = p ("Substring.substring huge n: " ^ t (fn () => Substring.substring ("abcde", 1, big)))
val () = p ("Substring.extract huge n: " ^ t (fn () => Substring.extract ("abcde", 1, SOME big)))
val () = p ("String.extract huge n: " ^ t (fn () => String.extract ("abcde", 1, SOME big)))
val () = p ("String.substring huge n: " ^ t (fn () => String.substring ("abcde", 1, big)))
val () = p ("CharVectorSlice.slice huge n: " ^ t (fn () => CharVectorSlice.slice ("abcde", 1, SOME big)))
val () = p ("Word8ArraySlice.slice huge n: " ^ t (fn () => Word8ArraySlice.slice (Word8Array.array (5, 0w0), 1, SOME big)))
val () = p ("CharArray.copy huge len: " ^ t (fn () => CharArray.copy {src = CharArray.array (5, #"a"), dst = CharArray.array (5, #"b"), di = 0}))
val () = p ("Word8Array.copyVec huge si: " ^ t (fn () => Word8ArraySlice.copyVec {src = Word8VectorSlice.slice (Word8Vector.fromList [0w1,0w2], big, NONE), dst = Word8Array.array (5, 0w0), di = 0}))
val () = p ("valid slices: " ^ Substring.string (Substring.slice (Substring.full "abcde", 1, SOME 3)) ^ " " ^ String.extract ("abcde", 2, NONE) ^ " " ^ Substring.string (Substring.slice (Substring.full "abcde", 5, SOME 0)))

in
end
local
(* A byte array of a negative length is a Size error, as for the
 * polymorphic arrays; it used to be allocated. *)
fun p s = print (s ^ "\n")
fun t f = (f (); "no exception") handle Subscript => "Subscript" | Overflow => "Overflow" | Size => "Size"
val () = p ("Word8Array.array ~1: " ^ t (fn () => Word8Array.array (~1, 0w0)))
val () = p ("CharArray.array ~1: " ^ t (fn () => CharArray.array (~1, #"a")))
val () = p ("CharArray.tabulate ~1: " ^ t (fn () => CharArray.tabulate (~1, fn _ => #"a")))
val () = p ("Word8Array.array 0: " ^ t (fn () => Word8Array.array (0, 0w0)))
val () = p ("Word8Array.array 3: " ^ Int.toString (Word8Array.length (Word8Array.array (3, 0w7))))
val () = p ("Array.array ~1: " ^ t (fn () => Array.array (~1, 0)))

in
end
