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

val _ = print "Testing stream structure (part of Pickle library)...\n"

local
open Stream

fun test (outw,getw) ws =
    let val os = openOut()
	val osr = ref os
	fun ow w = osr := outw(w,!osr)
	val _ = app ow ws 
	val s = toString (!osr)
	val _ = print ("sz(s) = " ^ Int.toString(size s) ^ "\n")
	val is = openIn s
	val isr = ref is
	fun gw() = 
	    let val (w,is) = getw(!isr)
	    in isr:=is ; w
	    end
	val ws' = map (fn _ => gw()) ws
    in ws = ws'
    end

val ws = [0w0,0w1,0w2,0w254,0w255,0w256,0w257,
	  0w0,0w0,0w0,0w0,0w0,
	  0w1,0w0,0w2,0w0,0w254,0w0,0w255,0w0,0w256,0w0,0w257,
	  0wxFFFFFFFC,0wxFFFFFFFD,0wxFFFFFFFE,0wxFFFFFFFF,
	  0w0,0wxFFFFFFFC,0w0,0wxFFFFFFFD,0w0,0wxFFFFFFFE,0w0,0wxFFFFFFFF,
	  0w0,0wxFFFFFFF0,0w0,0wxFFFFFFF1,0w0,0wxFFFFFFF2,0w0,0wxFFFFFFF3,
	  0w0,0wxFFFFFF00,0w0,0wxFFFFFF01,0w0,0wxFFFFFF02,0w0,0wxFFFFFF03,
	  0w0,0wxFF00FF00,0w0,0wxFF01FF01,0w0,0wxFF02FF02,0w0,0wxFF03FF03,
	  0wxFFFBFFFC,0wxFFFCFFFD,0wxFFFDFFFE,0wxFFFEFFFF]

val ws = ws @ ws
val ws = ws @ ws
val ws = ws @ [0w0,0wxFFFFFFFF, 0w255] @ ws
val ws = ws @ ws
val ws = ws @ ws

in
    val _ = tst "test1" (test (outw,getw) ws)
    val _ = tst "test2" (test (outcw,getcw) ws)

    fun toggle (f1,f2) =
	let val br = ref true
	in fn a => if !br then f1 a before br:=false
		   else f2 a before br:=true
	end

    val _ = tst "test3" (test (toggle (outw,outcw),
			       toggle (getw,getcw)) ws)

    val _ = tst "test4" (test (toggle (outcw,outw),
			       toggle (getcw,getw)) ws)

    val cs = [#"\000",#"\001",#"\002",#"\253",#"\254",#"\255"]
    val _ = tst "test5" (test (out,get) cs)
end

