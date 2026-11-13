(* Real equality primitives must round-trip through legal Basis calls. *)
fun compare (x:real, y:real) = (Real.==(x,y), Real.!=(x,y))
fun direct (x:real, y:real) : bool * bool =
    (prim ("__equal_real", (x,y)), prim ("__unequal_real", (x,y)))
val nan = 0.0 / 0.0
val () =
    if compare (nan,nan) = (false,true) andalso
       compare (0.0,~0.0) = (true,false) andalso
       compare (1.0,2.0) = (false,true) andalso
       compare (2.0,2.0) = (true,false) andalso
       direct (nan,nan) = (false,true) andalso
       direct (0.0,~0.0) = (true,false) andalso
       direct (1.0,2.0) = (false,true) andalso
       direct (2.0,2.0) = (true,false)
    then print "Real operations passed\n"
    else raise Fail "Real operations"
