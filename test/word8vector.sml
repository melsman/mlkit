(* test/word8vector.sml -- some test cases for Word8Vector 
   PS 1994-12-10, 1995-06-14, 2000-10-24 

   - modified for the MLKit; mael 2005-11-28 *)

fun ptest t s = print(t ^ ": " ^ s ^ "\n")
infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds p = check'(fn _ => range bounds p)


local
    open Word8Vector;
    val i2w = Word8.fromInt;
    infix 9 sub;
in

val a = fromList (List.map i2w [0,1,2,3,4,5,6]);
val b = fromList (List.map i2w [44,55,66]);
val c = fromList (List.map i2w [0,1,2,3,4,5,6]);

val test1 = check'(fn _ => a<>b);
val _ = ptest "test1" test1
val test2 = check'(fn _ => a=c);
val _ = ptest "test2" test2

val d = tabulate(100, fn i => i2w (i mod 7));

val test3 = check'(fn _ => d sub 27 = i2w 6);
val _ = ptest "test3" test3

val test4a = (tabulate(maxLen+1, i2w) seq "WRONG")
             handle Size => "OK" | _ => "WRONG";
val _ = ptest "test4a" test4a

val test4b = (tabulate(~1, i2w)       seq "WRONG")
             handle Size => "OK" | _ => "WRONG"
val _ = ptest "test4b" test4b

val test4c = check'(fn _ => length (tabulate(0, fn i => i2w (i div 0))) = 0);
val _ = ptest "test4c" test4c

val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7);
val _ = ptest "test5" test5

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test6a" test6a
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test6b" test6b
val test6c = check'(fn _ => c sub 0 = i2w 0);
val _ = ptest "test6c" test6c

val e = concat [d, b, d];

val test7 = check'(fn _ => length e = 203);
val _ = ptest "test7" test7

val test8 = check'(fn _ => length (concat []) = 0);
val _ = ptest "test8" test8

val f = Word8VectorSlice.vector(Word8VectorSlice.slice(e, 100, SOME 3));

val test9 = check'(fn _ => f = b);
val _ = ptest "test9" test9

fun chkiter iter f vec reslast =
    check'(fn _ =>
	   let val last = ref (0w255:Word8.word)
	       val res = iter (fn x => (last := x; f x)) vec
	   in (res, !last) = reslast end)

fun chkiteri iter f vec reslast =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = iter (fn (i, x) => (last := i; f x)) vec
	   in (res, !last) = reslast end)
fun chkfold fold f start vec reslast =
    check'(fn _ =>
	   let val last = ref (0w255:Word8.word)
	       val res = fold (fn (x, r) => (last := x; f(x, r))) start vec
	   in (res, !last) = reslast end)
fun chkfoldi fold f start vec reslast =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = fold (fn (i, x, r) => (last := i; f(x, r))) start vec
	   in (res, !last) = reslast end)

val test10a = 
    chkiter map (fn x => 0w2*x) b (fromList [0w88,0w110,0w132], 0w66)
val _ = ptest "test10a" test10a
val test10b = 
    chkiter app (fn x => ignore(0w2*x)) b ((), 0w66)
val _ = ptest "test10b" test10b
val test10c = 
    chkiter find (fn x => false) b (NONE, 0w66)
val _ = ptest "test10c" test10c
val test10d = 
    chkiter exists (fn x => false) b (false, 0w66)
val _ = ptest "test10d" test10d
val test10e = 
    chkiter all (fn x => true) b (true, 0w66)
val _ = ptest "test10e" test10e
val test10f = 
    chkfold foldl (op +) 0w0 b (0w165, 0w66)
val _ = ptest "test10f" test10f
val test10g = 
    chkfold foldr (op +) 0w0 b (0w165, 0w44)
val _ = ptest "test10g" test10g

val test11a = 
    chkiteri mapi (fn x => 0w2*x) b (fromList [0w88,0w110,0w132], 2)
val _ = ptest "test11a" test11a
val test11b = 
    chkiteri appi (fn x => ignore(0w2*x)) b ((), 2)
val _ = ptest "test11b" test11b
val test11c = 
    chkiteri findi (fn x => false) b (NONE, 2)
val _ = ptest "test11c" test11c
val test11d = 
    chkfoldi foldli (op +) 0w0 b (0w165, 2)
val _ = ptest "test11d" test11d
val test11e = 
    chkfoldi foldri (op +) 0w0 b (0w165, 0)
val _ = ptest "test11e" test11e

val test12a = 
    check'(fn _ => 
	   a = update(a, 0, 0w0) 
	   andalso a = update(a, 6, 0w6)
	   andalso fromList (List.map i2w [78,1,2,3,4,5,6]) 
	           = update(a, 0, 0w78)
	   andalso fromList (List.map i2w [0,1,2,33,4,5,6]) 
	           = update(a, 3, 0w33))
val _ = ptest "test12a" test12a
val test12b =
    (update(b, ~1, 0w17) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test12b" test12b
val test12c =
    (update(b, 7, 0w17) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test12c" test12c
val test12d =
    (update(fromList [], 0, 0w17) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test12d" test12d

val test13 = 
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Word8.compare(c2, c1)
	       fun coll s1 s2 = 
		   collate invcompare (Byte.stringToBytes s1,
				       Byte.stringToBytes s2)
	   in 
	       coll "" "" = EQUAL
	       andalso coll "" " " = LESS
	       andalso coll " " "" = GREATER
	       andalso coll "ABCD" "ABCD" = EQUAL
	       andalso coll "ABCD" "ABCD " = LESS
	       andalso coll "ABCD " "ABCD" = GREATER
	       andalso coll "B" "ABCD" = LESS
	       andalso coll "ABCD" "B" = GREATER
	       andalso coll "CCCB" "CCCABCD" = LESS
	       andalso coll "CCCABCD" "CCCB" = GREATER
	       andalso coll "CCCB" "CCCA" = LESS
	       andalso coll "CCCA" "CCCB" = GREATER
	   end)
val _ = ptest "test13" test13

val test14 = 
    check'(fn _ => 
	   NONE = find (fn i => i > 0w7) a
	   andalso SOME 0w5 = find (fn i => i > 0w4) a
	   andalso NONE = find (fn _ => true) (fromList []));
val _ = ptest "test14" test14

val test15 = 
    check'(fn _ => 
	   not (exists (fn i => i > 0w7) a)
	   andalso exists (fn i => i > 0w4) a
	   andalso not (exists (fn _ => true) (fromList [])));
val _ = ptest "test15" test15

val test16 = 
    check'(fn _ => 
	   not (all (fn i => i < 0w6) a)
	   andalso all (fn i => i < 0w7) a
	   andalso all (fn _ => false) (fromList []));
val _ = ptest "test16" test16
end;
