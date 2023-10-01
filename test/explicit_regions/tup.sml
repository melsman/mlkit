(* Tuples can be allocated in explicit regions *)
fun f () : int =
    let with r
        val x = (3,5)`r
    in #1 x
    end
