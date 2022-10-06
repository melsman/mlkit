(* test/vectorslice.sml -- some test cases for VectorSlice
   sestoft@dina.kvl.dk 2000-10-20

   - modified for the MLKit; mael 2005-11-28 *)

fun ptest t s = print(t ^ ": " ^ s ^ "<br>")
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

val _ = print "<h2>File vectorslice.sml: Testing structure VectorSlice...</h2>";

local
    open Vector VectorSlice
    infix 9 sub
    val array0 = Array.fromList []
    val vec0 = fromList []
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r

in

val a = fromList [1,11,21,31,41,51,61];
val b = fromList [441,551,661];
val c = fromList [1,11,21,31,41,51,61];

val slice00 = slice(vec0, 0, NONE)
val slice01 = slice(vec0, 0, SOME 0)
val slice02 = slice(a, 0, SOME 0)
val slice03 = slice(a, 7, NONE)
val slice04 = slice(a, 7, SOME 0)
val slice05 = slice(a, 4, SOME 0)

val slicea07 = full a
val slicea02 = slice(a, 0, SOME 2);
val slicea23 = slice(a, 2, SOME 3);
val slicea25 = slice(a, 2, SOME 5);

val slice06 = subslice(slicea23, 0, SOME 0)
val slice07 = subslice(slicea23, 1, SOME 0)
val slice08 = subslice(slicea23, 3, NONE)
val slice09 = subslice(slicea23, 3, SOME 0)

val slice0s = [slice00, slice01, slice02, slice03, slice04, slice05,
	       slice06, slice07, slice08, slice09];

val sliceas = [slicea07, slicea02, slicea23, slicea25];

val test1a =
    check'(fn _ => List.all
	   (fn sli => vector sli = Vector.fromList[]
	    andalso length sli = 0
	    andalso isEmpty sli
	    andalso vector (subslice(sli, 0, NONE)) = Vector.fromList[]
	    andalso vector (subslice(sli, 0, SOME 0)) = Vector.fromList[]
	    andalso all (fn _ => false) sli
	    andalso not (exists (fn _ => true) sli)
	    andalso NONE = find (fn _ => true) sli
	    andalso NONE = findi (fn _ => true) sli
	    andalso not (Option.isSome (getItem sli))
	    andalso (ArraySlice.copyVec{src=sli, dst=array0, di=0}; true)
	    andalso (app (fn _ => raise Fail "1a app") sli; true)
	    andalso (appi (fn _ => raise Fail "1a appi") sli; true)
	    andalso foldl cons [1,2] sli = [1,2]
	    andalso foldli consi [] sli = []
	    andalso foldr cons [1,2] sli = [1,2]
	    andalso foldri consi [] sli = []
	    andalso collate Int.compare (sli, slice00) = EQUAL)
	   slice0s);
val _ = ptest "test1a" test1a

val test1b =
    check'(fn _ =>
	   vector slicea02 = Vector.fromList[1, 11]
	   andalso vector slicea23 = Vector.fromList[21,31,41]
	   andalso vector slicea25 = Vector.fromList[21,31,41,51,61]
	   andalso vector slicea07 = Vector.fromList[1,11,21,31,41,51,61]
	   andalso base slicea02 = (a, 0, 2)
	   andalso base slicea23 = (a, 2, 3)
	   andalso base slicea25 = (a, 2, 5)
	   andalso base slicea07 = (a, 0, 7)
	   andalso length slicea02 = 2
	   andalso length slicea23 = 3
	   andalso length slicea25 = 5
	   andalso length slicea07 = 7);
val _ = ptest "test1b" test1b

val test2a =
    check'(fn _ =>
	   slicea07 sub 0 = 1
	   andalso slicea07 sub 6 = 61
	   andalso slicea23 sub 0 = 21
	   andalso slicea23 sub 2 = 41);
val _ = ptest "test2a" test2a

val test2b =
    (slicea07 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test2b" test2b

val test2c =
    (slicea07 sub 7; "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test2c" test2c

val test2cc =
    (slicea23 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test2cc" test2cc

val test2d =
    (slicea23 sub 3; "WRONG") handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test2d" test2d

val test2e =
    check'(fn _ =>
	   List.all (fn sli => ((sli sub 0; false)
				handle Subscript => true)) slice0s);
val _ = ptest "test2e" test2e

val test3a =
    check'(fn _ => List.all (not o isEmpty) sliceas)
val _ = ptest "test3a" test3a

val test4a =
    check'(fn _ => vector (subslice(slicea23, 0, SOME 0)) = Vector.fromList[]
	   andalso vector (subslice(slicea23, 0, NONE)) = Vector.fromList[21,31,41]
	   andalso vector (subslice(slicea23, 0, SOME 1)) = Vector.fromList[21]
	   andalso vector (subslice(slicea23, 0, SOME 2)) = Vector.fromList[21,31]
	   andalso vector (subslice(slicea23, 1, SOME 2)) = Vector.fromList[31,41]
	   andalso vector (subslice(slicea23, 3, SOME 0)) = Vector.fromList[]);
val _ = ptest "test4a" test4a

val test4b =
    (subslice(slicea23, 3, SOME 1); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test4b" test4b

val test4c =
    (subslice(slicea23, ~1, NONE); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test4c" test4c

val test4d =
    (subslice(slicea23, ~1, SOME 2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test4d" test4d

val test4e =
    (subslice(slicea23, 4, NONE); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test4e" test4e

val test4f =
    (subslice(slicea23, 4, SOME ~2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test4f" test4f

val test4g =
    (subslice(slicea23, 2, SOME 2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test4g" test4g

val test5 =
    check'(fn _ => let val (i1, r1) = Option.valOf (getItem slicea23)
		       val (i2, r2) = Option.valOf (getItem r1)
		       val (i3, r3) = Option.valOf (getItem r2)
		   in
		       i1 = 21 andalso i2 = 31 andalso i3 = 41
		       andalso not (Option.isSome (getItem r3))
		   end);
val _ = ptest "test5" test5

val sliced = full (tabulate(100, fn i => i mod 7 * 10 + 1));
val sliceb = full b;

val e = Array.array(203, 0);
val _ = (ArraySlice.copyVec{src=sliced, dst=e, di=0};
	 ArraySlice.copyVec{src=sliceb, dst=e, di=length sliced};
	 ArraySlice.copyVec{src=sliced, dst=e,
			    di=length sliced + length sliceb});

val ev = Vector.concat [vector sliced, vector sliceb, vector sliced];
(* length e = 203 *)

val slicee = full (Array.vector e)

val test9a =
    check'(fn () => vector(subslice(slicee, 100, SOME 3)) = vector sliceb);
val _ = ptest "test9a" test9a
val test9b =
    check'(fn () =>
	   ev = vector (subslice(slicee, 0, SOME (length slicee)))
	   andalso ev = vector (subslice(slicee, 0, NONE)));
val _ = ptest "test9b" test9b

val _ = ArraySlice.copyVec{src=slicee, dst=e, di=0};
val g = Array.array(203, 9999999);
val _ = ArraySlice.copyVec{src=slicee, dst=g, di=0};

val sliceg = full (Array.vector g);

val test10a =
	   check'(fn () => ev = Array.vector e
		  andalso ev = Array.vector g);
val _ = ptest "test10a" test10a

val sliceg0 = slice(Array.vector g, 0, SOME (Array.length g - 1));
val _ = ArraySlice.copyVec{src=sliceg0, dst=g, di=1};
val test10b =
    check'(fn () =>
	   vector sliceb = vector (slice(Array.vector g, 101, SOME 3)));
val _ = ptest "test10b" test10b

val sliceg1 = slice(Array.vector g, 1, SOME (Array.length g - 1));
val _ = ArraySlice.copyVec{src=sliceg1, dst=g, di=0};
val test10c =
    check'(fn () =>
	   vector sliceb = vector (slice(Array.vector g, 100, SOME 3)));
val _ = ptest "test10c" test10c

val sliceg202 = slice(Array.vector g, 202, SOME 1);
val _ = ArraySlice.copyVec{src=sliceg202, dst=g, di=202};
val test10d =
    check'(fn () => Array.sub(g, 202) = 10 * (202-1-103) mod 7 + 1);
val _ = ptest "test10d" test10d

val test11a = (ArraySlice.copyVec{src=sliceg, dst=g, di= ~1}; "WRONG")
              handle Subscript => "OK" | _ => "WRONG"
val _ = ptest "test11a" test11a
val test11b = (ArraySlice.copyVec{src=sliceg1, dst=g, di=0}; "OK")
              handle _ => "WRONG"
val _ = ptest "test11b" test11b
val test11c = (ArraySlice.copyVec{src=sliceg, dst=g, di=1}; "WRONG")
              handle Subscript => "OK" | _ => "WRONG"
val _ = ptest "test11c" test11c

local
    val v = ref 0
    fun setv c = v := c;
    fun addv c = v := c + !v;
    fun setvi (i, c) = v := c + i;
    fun setvif (i, c, _) = v := c + i;
    fun addvi (i, c) = v := c + i + !v;
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    val inplist = [1,2,3,4,7,9,13,4,5,6,8,0];
    val inpa = Vector.fromList inplist
    val inp = slice(inpa, 4, SOME 3)
    val pnia = Vector.fromList (rev inplist)
    val pni = slice(pnia, 5, SOME 3)
in

val test12a =
    check'(fn _ =>
	           foldl cons [1,2] inp = [13,9,7,1,2]
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = 13));
val _ = ptest "test12a" test12a

val test12b =
    check'(fn _ =>
	           foldr cons [1,2] inp = [7,9,13,1,2]
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = 7));
val _ = ptest "test12b" test12b

val test12c =
    check'(fn _ =>
	   find (fn _ => false) inp = NONE
	   andalso find (fn x => x=7) inp = SOME 7
	   andalso find (fn x => x=9) inp = SOME 9
	   andalso (setv 0; find (fn x => (addv x; x=9)) inp; !v = 7+9));
val _ = ptest "test12c" test12c

val test12d =
    check'(fn _ =>
           ((setv 0; app addv inp; !v = 7+9+13)
	    andalso (app setv inp; !v = 13)));
val _ = ptest "test12d" test12d

val test12f =
    check'(fn _ =>
	   not (exists (fn i => i>13) inp)
	   andalso exists (fn i => i>12) inp);
val _ = ptest "test12f" test12f
val test12g =
    check'(fn _ =>
	   (setv 117; exists (fn x => (setv x; false)) slice05; !v = 117)
	   andalso (setv 0; exists (fn x => (addv x; false)) inp; !v = 7+9+13)
	   andalso (exists (fn x => (setv x; false)) inp; !v = 13));
val _ = ptest "test12g" test12g
val test12h =
    check'(fn _ =>
	   not (all (fn i => i<13) inp)
	   andalso all (fn i => i<14) inp);
val _ = ptest "test12h" test12h
val test12i =
    check'(fn _ =>
	   (setv 117; all (fn x => (setv x; true)) slice05; !v = 117)
	   andalso (setv 0; all (fn x => (addv x; true)) inp; !v = 7+9+13)
	   andalso (all (fn x => (setv x; true)) inp; !v = 13));
val _ = ptest "test12i" test12i


val test13 =
    check'(fn _ =>
	   foldli consi [] inp = [(2,13),(1,9),(0,7)]
	   andalso foldri consi [] inp = [(0,7),(1,9),(2,13)]
	   andalso (setv 117; foldli setvif () inp; !v = 2+13)
	   andalso (setv 117; foldri setvif () inp; !v = 0+7));
val _ = ptest "test13" test13

val test14a =
    check'(fn _ =>
	   findi (fn _ => false) inp = NONE
	   andalso findi (fn (i,x) => x=9) inp = SOME (1,9)
	   andalso findi (fn (i,x) => i=2) inp = SOME (2,13));
val _ = ptest "test14a" test14a

val test14b =
    check'(fn _ =>
	   List.all (fn sli => NONE=findi (fn (j, x) => j >= 7 orelse j < 0 orelse x mod 10 <> 1) sli)
	            sliceas)
val _ = ptest "test14b" test14b

val test15 =
    check'(fn _ =>
           ((setvi (0,0); appi addvi inp; !v = 3+7+9+13)
	   andalso (appi setvi inp; !v = 2+13)));
val _ = ptest "test15" test15

end

val test17a =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Char.compare (c2, c1)
	       fun coll s1 s2 =
		   collate invcompare (full (fromList (explode s1)),
				       full (fromList (explode s2)))
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
val _ = ptest "test17a" test17a

val test17b =
    check'(fn _ =>
  let val sa = fromList(explode "AAAAaAbAABBBB");
                              (* 0123456789012 *)
      fun invcompare (c1, c2) = Char.compare (c2, c1)
      fun coll s1 s2 = collate invcompare (s1, s2)
  in
      coll (full sa) (slice(sa, 0, SOME 13)) = EQUAL
      andalso coll (slice(sa, 0, SOME 0)) (slice(sa, 13, SOME 0)) = EQUAL
      andalso coll (slice(sa, 0, SOME 0)) (slice(sa, 0, SOME 13)) = LESS
      andalso coll (slice(sa, 0, SOME 13)) (slice(sa, 0, SOME 0)) = GREATER
      andalso coll (slice(sa, 0, SOME 3)) (slice(sa, 1, SOME 3)) = EQUAL
      andalso coll (slice(sa, 0, SOME 4)) (slice(sa, 1, SOME 4)) = GREATER
      andalso coll (slice(sa, 1, SOME 4)) (slice(sa, 0, SOME 4)) = LESS
  end)
val _ = ptest "test17b" test17b

val _ = print "Test ended."
end
