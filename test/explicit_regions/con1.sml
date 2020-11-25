(* Con1 can be allocated in explicit regions *)

datatype t = A of int | B  (* t is boxed, which means that B and A are allocated *)

fun f () : int =
    let region r
        val x = if true then B else A`r 9
    in case x of
           B => 1
         | A a => a
    end
