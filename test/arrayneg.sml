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
