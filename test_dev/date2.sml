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

(* test/date.sml
   PS 1995-03-20, 1995-05-12, 1996-07-05, 1998-04-07
*)

(* MosML test file ported to the ML Kit; ME 1998-07-17 *)

val _ = print "\nFile date.sml: Testing structure Date...\n"

(*local *)
    val _ = 
	Date.date {year=2000, month=Date.Jan, day=1, hour=0, minute=0, second=0,
	      offset = SOME Time.zeroTime}
    val _ = print "hello2\n"
(*
in
end
*)