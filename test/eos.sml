(* endOfStream is true once inputAll has exhausted a stream, which
 * left its buffer unconsumed. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
fun check s =
    let val ins = TextIO.openString s
        val atStart = TextIO.endOfStream ins
        val all = TextIO.inputAll ins
        val atEnd = TextIO.endOfStream ins
        val again = TextIO.inputAll ins
    in TextIO.closeIn ins;
       "\"" ^ s ^ "\": " ^ b atStart ^ " " ^ b (all = s) ^ " " ^ b atEnd ^ " " ^ b (again = "")
    end
val () = List.app (p o check) ["", "e", "hello", "a\nb\n"]
val () = p ("input after inputAll: " ^ let val ins = TextIO.openString "xy" val _ = TextIO.inputAll ins in "\"" ^ TextIO.input ins ^ "\" " ^ b (TextIO.endOfStream ins) end)
