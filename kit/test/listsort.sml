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

(* File "test/listsort.sml"  *)

val _ = print "\nFile listsort.sml: Testing structure ListSort...\n"

local
    val a_unsort = Random.randomlist (10000, Random.newgen ());
    val a_sort = ListSort.sort Real.compare a_unsort;
in
    val test1 = tst "test1"
      (not (ListSort.sorted Real.compare a_unsort
	    orelse ListSort.sorted Real.compare [2.1, 1.0]))
	
    val test2 = tst "test2"
      (ListSort.sorted Real.compare a_sort
       andalso ListSort.sorted Real.compare []
       andalso ListSort.sorted Real.compare [1.0, 2.1])
end
