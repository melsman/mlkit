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
