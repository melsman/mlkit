fun sample () : unit = prim("mlkit_rp_sample", ());
val retained = [1,2,3];
val _ = sample();
fun retainedClosure x = x + List.length retained;
val _ = sample();
val _ = if retainedClosure 4 = 7 then print "repl profile ok\n" else raise Fail "closure";
:quit;
