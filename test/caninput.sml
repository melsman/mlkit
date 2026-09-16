(* StreamIO.canInput reports what is buffered, including what another
 * stream has already read ahead of this one. *)
fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
structure SIO = TextIO.StreamIO
fun stream s = SIO.mkInstream (TextPrimIO.openVector s, "")
fun can s =
    let val f = stream s
        val (v, f') = SIO.input f
        fun show NONE = "NONE" | show (SOME n) = "SOME " ^ Int.toString n
    in "\"" ^ s ^ "\": read " ^ Int.toString (size v) ^ ", canInput (f, 1) = " ^ show (SIO.canInput (f, 1))
       ^ ", canInput (f, 100) = " ^ show (SIO.canInput (f, 100)) ^ ", canInput (f', 1) = " ^ show (SIO.canInput (f', 1))
       ^ ", endOfStream f = " ^ b (SIO.endOfStream f) ^ ", endOfStream f' = " ^ b (SIO.endOfStream f')
    end
val () = List.app (p o can) ["", "L", "hello"]
val () = p ("fresh stream: " ^ (case SIO.canInput (stream "abc", 2) of SOME n => Int.toString n | NONE => "NONE"))
