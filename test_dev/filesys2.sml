(* Auxiliary functions for test cases *)

infix ^
fun (s : string) ^ (s' : string) : string = prim ("concatString", "concatStringProfiling", (s, s'))

fun print (s:string) : unit = prim("printString", "printString", s)

fun fullPath p = raise Match handle Match => raise Bind

fun tst0 s s' = print (s ^ " " ^ s' ^ "\n");
fun tst' s f = tst0 s (f () handle _ => "EXN");

val _ = tst' "test8b" (fn _ => fullPath ());

val _ = print "end\n"

