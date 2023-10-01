(* Two tuples can be allocated in the same explicit region *)

infix +
fun f () : int =
    let with r
        val x = (3,5)`r
        val y = (5,8)`r
    in #1 x + #2 y
    end
