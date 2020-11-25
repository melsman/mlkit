(* Strings can be allocated in explicit regions *)

fun size (s : string) : int = prim ("__bytetable_size", s)

fun f () : int =
    let region r
        val x = "Hi there"`r
    in size x
    end
