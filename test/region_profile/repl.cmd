fun sample () : unit = prim("mlkit_rp_sample", ());
val retained = [1,2,3];
fun sampleLocal__noinline x = let val a = Array.array(4,x) val _ = sample() in Array.sub(a,0) end;
val _ = sampleLocal__noinline 7;
fun retainedClosure x = x + List.length retained;
val _ = sample();
val _ = if retainedClosure 4 = 7 then print "repl profile ok\n" else raise Fail "closure";
:quit;
