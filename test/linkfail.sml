(* A call of a C function that the runtime does not have compiles, and
 * the link then fails.  The compiler must report that failure through
 * its exit status; it used to ignore the linker's status and report
 * success, leaving no executable behind. *)

fun missing () : int = prim ("no_such_function_in_the_runtime", ())

val () = print (Int.toString (missing ()) ^ "\n")
