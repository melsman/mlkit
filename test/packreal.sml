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

(* test/real.sml -- PS 1995-03-24, 1996-05-16, 1996-07-02, 1996-09-25 *)

(*KILL 05/11/1997 11:01. tho.:
use "auxil.sml";
*)

val _ = print "Testing structure PackRealBig...\n"

local 
    open Real PackRealBig
    infix ==
    fun inv r = 
	let val _ = print ("[r = " ^ Real.toString r)
	    val bytes = toBytes r
	    val _ = print (", sz(vector) = " ^ Int.toString (Word8Vector.length bytes))
	    val bytes_s = Byte.bytesToString bytes
	    val _ = print (", sz(bs) " ^ Int.toString(size bytes_s))
	    val r' = fromBytes bytes
	    val _ = print (", r' = " ^ Real.toString r' ^ "]\n")
	in r == r'
	end    
in	
val test1 = tst "test1" (inv 0.0 andalso inv 2.0 andalso inv 43.23 andalso inv ~43.2);
end