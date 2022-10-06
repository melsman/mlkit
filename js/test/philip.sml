fun bar x =
    fn x => ()

fun foo () =
    (bar (Real.fromString "4.0"); ())

val () = foo ()

val () = print "ok\n"
