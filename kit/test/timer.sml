(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds  

(* test/timer.sml
   PS 1995-03-20
*)

val _ = print "\nFile timer.sml: Testing structure Timer...\n"

local 
    fun fib n = if n<2 then 1 else fib(n-1) + fib(n-2);

    open Time Timer
    val totalRealTime = startRealTimer ()
    val totalCPUTime  = startCPUTimer ()


val test1 = tst "test1" (checkRealTimer totalRealTime <= checkRealTimer totalRealTime
		  andalso (checkRealTimer totalRealTime before fib 25 seq ())
  		           < checkRealTimer totalRealTime);

local
    val rtmr = startRealTimer ();
in
val test2 = tst "test2" (checkRealTimer rtmr <= checkRealTimer rtmr
		  andalso (checkRealTimer rtmr before fib 25 seq ())
  		           < checkRealTimer rtmr);
end

local
    val op <= = fn ({usr=usr1, sys=sys1, gc=gc1}, {usr=usr2, sys=sys2, gc=gc2})
	=> usr1 <= usr2 andalso sys1 <= sys2 andalso gc1 <= gc1;
    fun cput1 < cput2 = (cput1 <= cput2) andalso (cput1 <> cput2);
in
val test3 = tst "test3" (checkCPUTimer totalCPUTime <= checkCPUTimer totalCPUTime
		  andalso (checkCPUTimer totalCPUTime before fib 25 seq ())
  		           < checkCPUTimer totalCPUTime);
val ctmr = startCPUTimer ();
val test4 = tst "test4" (checkCPUTimer ctmr <= checkCPUTimer ctmr
		  andalso (checkCPUTimer ctmr before fib 25 seq ())
  		           < checkCPUTimer ctmr);
end;
(*
val _ = 
let
    fun time f arg =
	let open Timer
	    val cputimer  = startCPUTimer ()
	    val realtimer = startRealTimer ()
	    val res = f arg
	    val {usr, sys, gc} = checkCPUTimer cputimer;
	    val rea = checkRealTimer realtimer;
	    fun format t = Time.toString t
	in 
	    print("User: " ^ format usr ^
		"  System: " ^ format sys ^ 
		"  Gc: " ^ format gc ^ 
		"  Real: " ^ format rea ^ "\n");
	    res
	end;

    val _ = print "\nEach line below should show roughly \
                   \the same User, System, and Gc times:\n";
in
    map (time fib) [28, 28, 28, 28, 28, 28, 28, 28] seq () 
end 
*)
in
end
