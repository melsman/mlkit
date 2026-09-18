(* Word negation is two's complement negation, which wraps; it used to
 * go through toInt, which raises Overflow on a top bit that is set. *)
fun p s = print (s ^ "\n")
fun t f = (f ()) handle Overflow => "Overflow"
val () = p ("Word63: " ^ t (fn () => Word63.toString (Word63.~ 0wx6C7D9922F47CE479)))
val () = p ("Word63: " ^ t (fn () => Word63.toString (Word63.~ 0w1)))
val () = p ("Word: " ^ t (fn () => Word.toString (Word.~ 0w0)))
val () = p ("Word63: " ^ t (fn () => Word63.toString (Word63.~ 0wx4000000000000001)))
val () = p ("Word31: " ^ t (fn () => Word31.toString (Word31.~ 0wx40000001)))
val () = p ("Word32: " ^ t (fn () => Word32.toString (Word32.~ 0wx80000001)))
val () = p ("Word64: " ^ t (fn () => Word64.toString (Word64.~ 0wx8000000000000001)))
val () = p ("Word8: " ^ t (fn () => Word8.toString (Word8.~ 0wx81)))
val () = p ("Word16: " ^ t (fn () => Word16.toString (Word16.~ (Word16.fromInt 0x8001))))
val () = p ("law: " ^ (if Word.~ 0wx123456789ABCDEF = 0w0 - 0wx123456789ABCDEF then "ok" else "bad"))
