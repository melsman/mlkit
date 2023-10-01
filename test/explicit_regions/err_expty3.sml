(* Escaping functions cannot live in local regions *)

val f : unit -> int -> int =
    (fn () =>
        let with r
            val g = (fn x => 4) : (int -> int)`r
        in g
        end)
