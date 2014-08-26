(* File array.sml: Testing Array operations.
 * Copyright (c) 1995-2014, Peter Sestoft, Martin Elsman.
 * MIT License.
 *)

open Utest

val _ = tstStart "structure Array"

infix 1 seq
fun e1 seq e2 = e2;

local
    open Array 
    infix 9 sub
    val array0 : int array = fromList []
in

val a = fromList [1,11,21,31,41,51,61];
val b = fromList [441,551,661];
val c = fromList [1,11,21,31,41,51,61];

val test1 = tst' "test1" (fn () => a<>c);

val test2 = 
    tst' "test2" (fn () => 
	   array(0, 11) <> array0
	   andalso array(0,()) <> tabulate(0, fn _ => ())
	   andalso tabulate(0, fn _ => ()) <> fromList [] 
	   andalso fromList [] <> fromList [] 
	   andalso array(0, ()) <> array(0, ())
	   andalso tabulate(0, fn _ => ()) <> tabulate(0, fn _ => ()));

val d = tabulate(100, fn i => i mod 7 * 10 + 1);

val test3 = 
    tst' "test3" (fn () => d sub 27 = 61);

val test4a = tst0 "test4a"
             ((tabulate(maxLen+1, fn i => i) seq "WRONG")
            handle Size => "OK" | _ => "WRONG");

val test4b = tst0 "test4b"
             ((tabulate(~1, fn i => i) seq "WRONG")
            handle Size => "OK" | _ => "WRONG");

val test4c = 
  tst' "test4c" (fn () => length (tabulate(0, fn i => i div 0)) = 0);

val test5a = 
    tst' "test5a" (fn () => length (fromList []) = 0 andalso length a = 7);
val test5b = 
    tst' "test5b" (fn () => length array0 = 0);

val test6a = tst0 "test6a" ((c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG");
val test6b = tst0 "test6b" ((c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG");
val test6c = tst' "test6c" (fn () => c sub 0 = 1);

val e = array(203, 0);
val _ = (copy{src=d, dst=e, di=0}; 
	 copy{src=b, dst=e, di=length d};
	 copy{src=d, dst=e, di=length d + length b});
	 
fun a2v a = vector a
val ev = Vector.concat [a2v d, a2v b, a2v d]; (* length e = 203 *)

val test7 = tst' "test7" (fn () => length e = 203);

val test8a = tst0 "test8a" ((update(e, ~1, 99) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG");
val test8b = tst0 "test8b" ((update(e, length e, 99) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG");

val test10i = 
    tst' "test10i" (fn () => (copy{src=array0, dst=array0, di=0}; 
		     array0 <> array(0, 999999)));
local 
    val v = ref 0
    fun setv c = v := c;
    fun addv c = v := c + !v;
    fun setvi (i, c) = v := c + i;
    fun addvi (i, c) = v := c + i + !v;
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    val inplist = [7,9,13];
    val inp = fromList inplist
    val pni = fromList (rev inplist)
    fun copyinp a = 
	copy{src=inp, dst=a, di=0}
in 

val array0 = fromList [] : int array;

val test12a =
    tst' "test12a" (fn _ =>
	           foldl cons [1,2] array0 = [1,2]
	   andalso foldl cons [1,2] inp = [13,9,7,1,2]
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = 13));

val test12b =
    tst' "test12b" (fn _ =>
	           foldr cons [1,2] array0 = [1,2]
	   andalso foldr cons [1,2] inp = [7,9,13,1,2]
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = 7));

val test12d = 
    tst' "test12d" (fn _ =>
           (setv 117; app setv array0; !v = 117)
	   andalso (setv 0; app addv inp; !v = 7+9+13)
	   andalso (app setv inp; !v = 13));

val test12e = 
    let val a = array(length inp, inp sub 0)
    in 
	tst' "test12e" (fn _ =>
           (modify (~ : int -> int) array0; true)
	   andalso (copyinp a; modify ~ a; foldr (op::) [] a = map ~ inplist)
	   andalso (setv 117; modify (fn x => (setv x; 37)) a; !v = ~13))
    end

val test13a =
    tst' "test13a" (fn _ =>
	           foldli consi [] (array0) = []
	   andalso foldri consi [] (array0) = []
	   andalso foldli consi [] (inp) = [(2,13),(1,9),(0,7)]
	   andalso foldri consi [] (inp) = [(0,7),(1,9),(2,13)])

val test15a = 
    tst' "test15a" (fn _ =>
           (setvi (0,117); appi setvi (array0); !v = 117)
	   andalso (setvi (0,0); appi addvi (inp); !v = 0+7+1+9+2+13)
	   andalso (appi setvi (inp); !v = 2+13));
end
val _ = tstEnd()
end

