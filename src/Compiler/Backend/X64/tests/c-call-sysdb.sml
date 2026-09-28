(* Run each lookup in a fresh process so libc initializes NSS on that call.
 * Call the runtime directly: the Basis wrappers interpret an untagged status
 * field and can raise Overflow, a separate runtime bug noted in issue #233. *)
fun getCtx () : foreignptr = prim("__get_ctx", ())
val missing = Fail "missing root account"
val () =
    case CommandLine.arguments () of
        ["group"] =>
        ignore (prim("sml_getgrnam", (getCtx(), "root", 16384, missing))
                : int * string list * int)
      | ["user"] =>
        ignore (prim("sml_getpwnam", (getCtx(), "root", 16384, missing))
                : int * int * string * string * int)
      | _ => raise Fail "expected group or user"
val () = print "OK\n"
