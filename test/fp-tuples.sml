(* Projection-only FP tuples, branch joins, and escaping tuple fallback. *)
fun check (name,b) = print (name ^ (if b then ": OK\n" else ": WRONG\n"))
fun localTuple__noinline (x,y,z,b) =
    let val t = (x+1.0,y+2.0,z+3.0)
        val first = #1 t * #1 t + #2 t * #2 t + #3 t * #3 t
    in if b then first + #1 t + #2 t + #3 t
       else first - #1 t - #2 t - #3 t
    end
fun escape__noinline (x,y,z) = (x+1.0,y+2.0,z+3.0)
val _ = check ("true",Real.==(localTuple__noinline(1.0,2.0,3.0,true),68.0))
val _ = check ("false",Real.==(localTuple__noinline(1.0,2.0,3.0,false),44.0))
val (x,y,z) = escape__noinline (1.0,2.0,3.0)
val _ = check ("escape",Real.==(x+y+z,12.0))
