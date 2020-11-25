(* test/real64vector.sml -- test cases for Real64Vector *)

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

val op == = Real.==
val op != = Real.!=

fun eq_opt eq (NONE,NONE) = true
  | eq_opt eq (SOME a,SOME b) = eq(a,b)
  | eq_opt _ _ = false

local
    open Real64Vector
    val i2r = Real64.fromInt
    infix 9 sub
    infix != ==
    fun eq (v1,v2) =
        length v1 = length v2 andalso
        let fun loop (i:int) =
                if i >= length v1 then true
                else v1 sub i == v2 sub i andalso loop (i+1)
        in loop 0
        end
    val op === = eq
    val op !== = not o eq
    infix === !==
in

val a = fromList (List.map i2r [0,1,2,3,4,5,6]);
val b = fromList (List.map i2r [44,55,66]);
val c = fromList (List.map i2r [0,1,2,3,4,5,6]);

val test1 = check'(fn _ => a!==b);
val _ = ptest "test1" test1
val test2 = check'(fn _ => a===c);
val _ = ptest "test2" test2

val d = tabulate(100, fn i => i2r (i mod 7));

val test3 = check'(fn _ => d sub 27 == i2r 6);
val _ = ptest "test3" test3

val test4a = (tabulate(maxLen+1, i2r) seq "WRONG")
             handle Size => "OK" | _ => "EXN";
val _ = ptest "test4a" test4a

val test4b = (tabulate(~1, i2r) seq "WRONG")
             handle Size => "OK" | _ => "EXN"
val _ = ptest "test4b" test4b

val test4c = check'(fn _ => length (tabulate(0, fn i => i2r (i div 0))) = 0);
val _ = ptest "test4c" test4c

val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7);
val _ = ptest "test5" test5

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "EXN";
val _ = ptest "test6a" test6a
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "EXN";
val _ = ptest "test6b" test6b
val test6c = check'(fn _ => c sub 0 == i2r 0);
val _ = ptest "test6c" test6c

val e = concat [d, b, d];

val test7 = check'(fn _ => length e = 203);
val _ = ptest "test7" test7

val test8 = check'(fn _ => length (concat []) = 0);
val _ = ptest "test8" test8

(*val f = RealVectorSlice.vector(RealVectorSlice.slice(e, 100, SOME 3))   <------- MEMO *)
val f = fromList [44.0,55.0,66.0]

val test9 = check'(fn _ => f === b);
val _ = ptest "test9" test9

fun chkiter iter f vec eq_res (res0,last0) =
    check'(fn _ =>
	   let val last = ref 255.0
	       val res = iter (fn x => (last := x; f x)) vec
	   in eq_res(res,res0) andalso !last == last0 end)

fun chkiteri iter f vec eq_res (res0,last0) =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = iter (fn (i, x) => (last := i; f x)) vec
	   in eq_res(res,res0) andalso !last = last0 end)

fun chkfold fold f start vec (res0,last0) =
    check'(fn _ =>
	   let val last = ref 255.0
	       val res = fold (fn (x, r) => (last := x; f(x, r))) start vec
	   in res == res0 andalso !last == last0 end)

fun chkfoldi fold f start vec (res0,last0) =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = fold (fn (i, x, r) => (last := i; f(x, r))) start vec
	   in res == res0 andalso !last = last0 end)

val test10a =
    chkiter map (fn x => 2.0*x) b (op ===) (fromList [88.0,110.0,132.0], 66.0)
val _ = ptest "test10a" test10a
val test10b =
    chkiter app (fn x => ignore(2.0*x)) b (op =) ((), 66.0)
val _ = ptest "test10b" test10b
val test10c =
    chkiter find (fn x => false) b (eq_opt (op ==)) (NONE, 66.0)

val _ = ptest "test10c" test10c
val test10d =
    chkiter exists (fn x => false) b (op =) (false, 66.0)
val _ = ptest "test10d" test10d
val test10e =
    chkiter all (fn x => true) b (op =) (true, 66.0)
val _ = ptest "test10e" test10e
val test10f =
    chkfold foldl (op +) 0.0 b (165.0, 66.0)
val _ = ptest "test10f" test10f
val test10g =
    chkfold foldr (op +) 0.0 b (165.0, 44.0)
val _ = ptest "test10g" test10g

val test11a =
    chkiteri mapi (fn x => 2.0*x) b (op ===) (fromList [88.0,110.0,132.0], 2)
val _ = ptest "test11a" test11a
val test11b =
    chkiteri appi (fn x => ignore(2.0*x)) b (op =) ((), 2)
val _ = ptest "test11b" test11b
val test11c =
    chkiteri findi (fn x => false) b (eq_opt (fn((i1,r1),(i2,r2)) => i1=i2 andalso r1==r2)) (NONE, 2)
val _ = ptest "test11c" test11c
val test11d =
    chkfoldi foldli (op +) 0.0 b (165.0, 2)
val _ = ptest "test11d" test11d
val test11e =
    chkfoldi foldri (op +) 0.0 b (165.0, 0)
val _ = ptest "test11e" test11e

val _ = ptest "test12a0"
              (check'(fn _ => a === update(a, 0, 0.0)))

val _ = ptest "test12a1"
              (check'(fn _ => a === update(a, 6, 6.0)))

val _ = ptest "test12a2"
              (check'(fn _ => fromList (List.map i2r [78,1,2,3,4,5,6])
	                               === update(a, 0, 78.0)))

val _ = ptest "test12a3"
              (check'(fn _ => fromList (List.map i2r [0,1,2,33,4,5,6])
	                               === update(a, 3, 33.0)))

val test12a =
    check'(fn _ =>
	   a === update(a, 0, 0.0)
	   andalso a === update(a, 6, 6.0)
	   andalso fromList (List.map i2r [78,1,2,3,4,5,6])
	           === update(a, 0, 78.0)
	   andalso fromList (List.map i2r [0,1,2,33,4,5,6])
	           === update(a, 3, 33.0))
val _ = ptest "test12a" test12a

val test12b =
    (update(b, ~1, 17.0) seq "WRONG")
    handle Subscript => "OK" | _ => "EXN";
val _ = ptest "test12b" test12b
val test12c =
    (update(b, 7, 17.0) seq "WRONG")
    handle Subscript => "OK" | _ => "EXN";
val _ = ptest "test12c" test12c
val test12d =
    (update(fromList [], 0, 17.0) seq "WRONG")
    handle Subscript => "OK" | _ => "EXN";
val _ = ptest "test12d" test12d

val test13 =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Real.compare(c2, c1)
	       fun coll l1 l2 =
		   collate invcompare (fromList l1,
				       fromList l2)
	   in
	       coll [] [] = EQUAL
	       andalso coll [] [0.0] = LESS
	       andalso coll [0.0] [] = GREATER
	       andalso coll [0.0,1.0,2.0,3.0] [0.0,1.0,2.0,3.0] = EQUAL
	       andalso coll [0.0,1.0,2.0,3.0] [0.0,1.0,2.0,3.0,4.0] = LESS
	       andalso coll [0.0,1.0,2.0,3.0,4.0] [0.0,1.0,2.0,3.0] = GREATER
	       andalso coll [1.0] [0.0,1.0,2.0,3.0] = LESS
	       andalso coll [0.0,1.0,2.0,3.0] [1.0] = GREATER
	       andalso coll [2.0,2.0,2.0,1.0] [2.0,2.0,2.0,0.0,1.0,2.0,3.0] = LESS
	       andalso coll [2.0,2.0,2.0,0.0,1.0,2.0,3.0] [2.0,2.0,2.0,1.0] = GREATER
	       andalso coll [2.0,2.0,2.0,1.0] [2.0,2.0,2.0,0.0] = LESS
	       andalso coll [2.0,2.0,2.0,0.0] [2.0,2.0,2.0,1.0] = GREATER
	   end)
val _ = ptest "test13" test13

val test14 =
    check'(fn _ =>
	   eq_opt (op ==) (NONE, find (fn i => i > 7.0) a)
	   andalso eq_opt (op ==) (SOME 5.0, find (fn i => i > 4.0) a)
	   andalso eq_opt (op ==) (NONE,find (fn _ => true) (fromList [])));
val _ = ptest "test14" test14

val test15 =
    check'(fn _ =>
	   not (exists (fn i => i > 7.0) a)
	   andalso exists (fn i => i > 4.0) a
	   andalso not (exists (fn _ => true) (fromList [])));
val _ = ptest "test15" test15

val test16 =
    check'(fn _ =>
	   not (all (fn i => i < 6.0) a)
	   andalso all (fn i => i < 7.0) a
	   andalso all (fn _ => false) (fromList []));
val _ = ptest "test16" test16
end
