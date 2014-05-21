(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

val _ = print "\nFile stringcvt.sml: Testing structure StringCvt...\n";

local 
    open StringCvt
    (* Read all upper case letters, skip lowercase letters, scan an
     * integer, and return the excess characters: *)

    val tmpfile = "textio.tmp";
    fun putandscan scan s = 
	let open TextIO 
	    val os = openOut tmpfile
	    val _  = output(os, s)
	    val _  = closeOut os 
	    val is = openIn tmpfile
	in
	    scanStream scan is
	    before 
	    closeIn is
	end;
	    
in
val test9 = 
    tst' "test9" (fn _ =>
    let fun getstring b getc src = 
	    SOME(takel (fn _ => b) getc src, src)
	fun dup 0 s = s
	  | dup n s = dup (n-1) (s^s);
	val longstring = dup 11 "abcDEFGHI"
    in 
	scanString (getstring true) longstring = SOME longstring 
	andalso scanString (getstring false) longstring = SOME ""
        andalso putandscan (getstring true) longstring = SOME longstring
    end)
end
