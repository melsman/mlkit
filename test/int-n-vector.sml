(* Test cases for Int<N>Vector, Int<N>VectorSlice, Int<N>Array, and
   Int<N>ArraySlice structures.
 *)

val errs = ref 0
fun incr_errs () = errs := !errs + 1

fun ptest t s = print(t ^ ": " ^ s ^ "\n")
infix 1 seq
fun e1 seq e2 = e2
fun check b = if b then "OK" else (incr_errs(); "WRONG")
fun check' f = (if f () then "OK" else (incr_errs(); "WRONG"))
               handle _ => (incr_errs(); "EXN")
fun range (from, to) p =
    let open Int
    in (from > to) orelse (p from) andalso (range (from+1, to) p)
    end
fun checkrange bounds p = check'(fn _ => range bounds p)

fun report () =
    if !errs = 0 then print "*** TESTS SUCCEEDED! ***\n"
    else print ("*** TESTS FAILED: " ^ Int.toString (!errs) ^ " Error(s)!! ***\n")

val pr_n = ref true

functor Test(structure I : INTEGER
             structure V : MONO_VECTOR
             structure A : MONO_ARRAY
             structure VS : MONO_VECTOR_SLICE
             structure AS : MONO_ARRAY_SLICE
             sharing type I.int = V.elem = VS.elem = A.elem = AS.elem
             sharing type V.vector = VS.vector = A.vector = AS.vector
             sharing type A.array = AS.array
             sharing type VS.slice = AS.vector_slice) : sig end =
struct

  fun eqv (v1,v2) = V.collate I.compare (v1,v2) = EQUAL

  fun pr_section s =
      let val word_size =
              case I.precision of
                  SOME i => Int.toString i
                | NONE => "Inf"
      in print ("[Testing Int"
                ^ (if !pr_n then word_size else "")
                ^ s ^ "]\n")
      end

  val () = pr_section "Vector"

  open V
  val i2w = I.fromInt
  infix 9 sub
  val neqv = not o eqv

  fun pr x =
      "[" ^ String.concatWith "," (List.tabulate(length x, fn i => I.toString (V.sub(x,i))))
      ^ "]"

  val op < = I.<
  val op > = I.>

  val a = fromList (List.map i2w [0,1,2,3,4,5,6])
  val b = fromList (List.map i2w [44,55,11])
  val c = fromList (List.map i2w [0,1,2,3,4,5,6])

  val hi = case I.precision of
               SOME 8 => 127
             | SOME 16 => 32767
             | SOME 31 => 1073741823
             | SOME 64 => 1073741823*1073741823
             | SOME 63 => 1073741823*1073741823
             | _ => 300
  val h = i2w hi
  val ah = fromList (List.map i2w [0,1,2,hi,4,5,6])

  val test0 = check'(fn _ => eqv(update(a,3,h),ah))
  val _ = ptest "test0" test0

  val test0a = check'(fn _ => h = V.sub(ah,3))
  val _ = ptest "test0a" test0a

  val test0b = check'(fn _ => (i2w 4) = V.sub(ah,4))
  val _ = ptest "test0b" test0b

  val test1 = check'(fn _ => neqv(a,b))
  val _ = ptest "test1" test1
  val test2 = check'(fn _ => eqv(a,c))
  val _ = ptest "test2" test2

  val d = tabulate(100, fn i => i2w (i mod 7))

  val test3 = check'(fn _ => d sub 27 = i2w 6)
  val _ = ptest "test3" test3

  val test4a = (tabulate(maxLen+1, i2w) seq "WRONG")
               handle Size => "OK" | _ => "WRONG"
  val _ = ptest "test4a" test4a

  val test4b = (tabulate(~1, i2w)       seq "WRONG")
               handle Size => "OK" | _ => "WRONG"
  val _ = ptest "test4b" test4b

  val test4c = check'(fn _ => length (tabulate(0, fn i => i2w (i div 0))) = 0)
  val _ = ptest "test4c" test4c

  val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7)
  val _ = ptest "test5" test5

  val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test6a" test6a
  val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test6b" test6b
  val test6c = check'(fn _ => c sub 0 = i2w 0)
  val _ = ptest "test6c" test6c

  val e = concat [d, b, d]

  val test7 = check'(fn _ => length e = 203)
  val _ = ptest "test7" test7

  val test8 = check'(fn _ => length (concat []) = 0)
  val _ = ptest "test8" test8

  val f = VS.vector(VS.slice(e, 100, SOME 3))

  val test9 = check'(fn _ => eqv(f,b))
  val _ = ptest "test9" test9

  fun chkiter iter f vec eq =
      check'(fn _ =>
	        let val last = ref (i2w ~1)
	            val res = iter (fn x => (last := x; f x)) vec
	        in eq(res, !last) end)

  fun chkiteri iter f vec eq =
      check'(fn _ =>
	        let val last = ref ~1
	            val res = iter (fn (i, x) => (last := i; f x)) vec
	        in eq(res, !last) end)

  fun chkfold fold f start vec reslast =
      check'(fn _ =>
	        let val last = ref (i2w ~1)
	            val res = fold (fn (x, r) => (last := x; f(x, r))) start vec
	        in (res, !last) = reslast end)

  fun chkfoldi fold f start vec reslast =
      check'(fn _ =>
	        let val last = ref ~1
	            val res = fold (fn (i, x, r) => (last := i; f(x, r))) start vec
	        in (res, !last) = reslast end)

  val test10a1 =
      chkiter map (fn x => I.*(i2w 2,x)) b
              (fn (x,y) => eqv(x,fromList (List.map i2w [88,110,22])))
  val _ = ptest "test10a1" test10a1

  val test10a2 =
      chkiter map (fn x => I.*(i2w 2,x)) b
              (fn (x,y) => y = i2w 11)
  val _ = ptest "test10a2" test10a2

  val test10b =
      chkiter app (fn x => ignore(I.*(i2w 2,x))) b
              (fn ((),y) => y = i2w 11)
  val _ = ptest "test10b" test10b

  val test10c =
      chkiter find (fn x => false) b
              (fn (opt,y) => opt = NONE andalso y = i2w 11)
  val _ = ptest "test10c" test10c

  val test10d =
      chkiter exists (fn x => false) b
              (fn (b,y) => b = false andalso y = i2w 11)
  val _ = ptest "test10d" test10d

  val test10e =
      chkiter all (fn x => true) b
              (fn (b,y) => b andalso y = i2w 11)
  val _ = ptest "test10e" test10e

  val test10f =
      chkfold foldl (I.+) (i2w 0) b (i2w 110, i2w 11)
  val _ = ptest "test10f" test10f

  val test10g =
      chkfold foldr (I.+) (i2w 0) b (i2w 110, i2w 44)
  val _ = ptest "test10g" test10g

  val test11a =
      chkiteri mapi (fn x => I.*(i2w 2,x)) b
               (fn (x,y) => eqv(x,fromList (List.map i2w [88,110,22]))
                            andalso y = 2)
  val _ = ptest "test11a" test11a

  val test11b =
      chkiteri appi (fn x => ignore(I.*(i2w 2,x))) b
               (fn ((),y) => y = 2)
  val _ = ptest "test11b" test11b

  val test11c =
      chkiteri findi (fn x => false) b
               (fn (opt, y) => opt = NONE andalso y = 2)
  val _ = ptest "test11c" test11c

  val test11d =
      chkfoldi foldli (I.+) (i2w 0) b (i2w 110, 2)
  val _ = ptest "test11d" test11d

  val test11e =
      chkfoldi foldri (I.+) (i2w 0) b (i2w 110, 0)
  val _ = ptest "test11e" test11e

  val test12a =
      check'(fn _ =>
	        eqv(a, update(a, 0, i2w 0))
	        andalso eqv(a, update(a, 6, i2w 6))
	        andalso eqv(fromList (List.map i2w [78,1,2,3,4,5,6]),
	                    update(a, 0, i2w 78))
	        andalso eqv(fromList (List.map i2w [0,1,2,33,4,5,6]),
	                    update(a, 3, i2w 33)))
  val _ = ptest "test12a" test12a
  val test12b =
      (update(b, ~1, i2w 17) seq "WRONG")
      handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test12b" test12b

  val test12c =
      (update(b, 7, i2w 17) seq "WRONG")
      handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test12c" test12c

  val test12d =
      (update(fromList [], 0, i2w 17) seq "WRONG")
      handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test12d" test12d

  fun fromString s : vector =
      V.tabulate(size s,
                 fn i => I.fromLarge(Word8.toLargeInt(Byte.charToByte(String.sub(s,i)))))

  val test13 =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = I.compare(c2, c1)
	       fun coll s1 s2 =
		   collate invcompare (fromString s1,
				       fromString s2)
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
	        NONE = find (fn i => i > (i2w 7)) a
	        andalso SOME (i2w 5) = find (fn i => i > (i2w 4)) a
	        andalso NONE = find (fn _ => true) (fromList []))
  val _ = ptest "test14" test14

  val test15 =
      check'(fn _ =>
	        not (exists (fn i => i > (i2w 7)) a)
	        andalso exists (fn i => i > (i2w 4)) a
	        andalso not (exists (fn _ => true) (fromList [])))
  val _ = ptest "test15" test15

  val test16 =
      check'(fn _ =>
	        not (all (fn i => i < (i2w 6)) a)
	        andalso all (fn i => i < (i2w 7)) a
	        andalso all (fn _ => false) (fromList []))
  val _ = ptest "test16" test16

  (* Vector Slice Testing *)

  val () = pr_section "VectorSlice"

  open V VS
  infix 9 sub

  val array0 = A.fromList []
  val vec0 = fromList []
  fun cons (x,r) = x ::  r
  fun consi (i,x,r) = (i,x) ::  r
  fun l2v xs = V.fromList (List.map i2w xs)
  fun eqvs ((v1,a1,b1),(v2,a2,b2)) =
      eqv(v1,v2) andalso a1=a2 andalso b1=b2

  val a = l2v [1,11,21,31,41,51,61]
  val b = l2v [44,55,66]
  val c = l2v [1,11,21,31,41,51,61]

  val slice00 = slice(vec0, 0, NONE)
  val slice01 = slice(vec0, 0, SOME 0)
  val slice02 = slice(a, 0, SOME 0)
  val slice03 = slice(a, 7, NONE)
  val slice04 = slice(a, 7, SOME 0)
  val slice05 = slice(a, 4, SOME 0)

  val slicea07 = full a
  val slicea02 = slice(a, 0, SOME 2)
  val slicea23 = slice(a, 2, SOME 3)
  val slicea25 = slice(a, 2, SOME 5)

  val slice06 = subslice(slicea23, 0, SOME 0)
  val slice07 = subslice(slicea23, 1, SOME 0)
  val slice08 = subslice(slicea23, 3, NONE)
  val slice09 = subslice(slicea23, 3, SOME 0)

  val slice0s = [slice00, slice01, slice02, slice03, slice04, slice05,
	         slice06, slice07, slice08, slice09]

  val sliceas = [slicea07, slicea02, slicea23, slicea25]

  val test1a =
    check'(fn _ => List.all
	   (fn sli => eqv(vector sli, fromList [])
	    andalso length sli = 0
	    andalso isEmpty sli
	    andalso eqv(vector (subslice(sli, 0, NONE)), fromList [])
	    andalso eqv(vector (subslice(sli, 0, SOME 0)), fromList [])
	    andalso all (fn _ => false) sli
	    andalso not (exists (fn _ => true) sli)
	    andalso NONE = find (fn _ => true) sli
	    andalso NONE = findi (fn _ => true) sli
	    andalso not (Option.isSome (getItem sli))
	    andalso (AS.copyVec{src=sli, dst=array0, di=0}; true)
	    andalso (app (fn _ => raise Fail "1a app") sli; true)
	    andalso (appi (fn _ => raise Fail "1a appi") sli; true)
	    andalso foldl cons [i2w 1,i2w 2] sli = [i2w 1,i2w 2]
	    andalso foldli consi [] sli = []
	    andalso foldr cons [i2w 1,i2w 2] sli = [i2w 1,i2w 2]
	    andalso foldri consi [] sli = []
	    andalso collate I.compare (sli, slice00) = EQUAL)
	   slice0s)
  val _ = ptest "test-vs-1a" test1a

  val test1b =
    check'(fn _ =>
	   eqv(vector slicea02, l2v [1, 11])
	   andalso eqv(vector slicea23, l2v [21,31,41])
	   andalso eqv(vector slicea25, l2v [21,31,41,51,61])
	   andalso eqv(vector slicea07, l2v [1,11,21,31,41,51,61])
	   andalso eqvs(base slicea02, (a, 0, 2))
	   andalso eqvs(base slicea23, (a, 2, 3))
	   andalso eqvs(base slicea25, (a, 2, 5))
	   andalso eqvs(base slicea07, (a, 0, 7))
	   andalso length slicea02 = 2
	   andalso length slicea23 = 3
	   andalso length slicea25 = 5
	   andalso length slicea07 = 7)
  val _ = ptest "test-vs-1b" test1b

  val test2a =
    check'(fn _ =>
	   slicea07 sub 0 = i2w 1
	   andalso slicea07 sub 6 = i2w 61
	   andalso slicea23 sub 0 = i2w 21
	   andalso slicea23 sub 2 = i2w 41)
  val _ = ptest "test-vs-2a" test2a

  val test2b =
    (slicea07 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-2b" test2b

  val test2c =
    (slicea07 sub 7; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-2c" test2c

  val test2cc =
    (slicea23 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-2cc" test2cc

  val test2d =
    (slicea23 sub 3; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-2d" test2d

  val test2e =
    check'(fn _ =>
	   List.all (fn sli => ((sli sub 0; false)
				handle Subscript => true)) slice0s)
  val _ = ptest "test-vs-2e" test2e

  val test3a =
    check'(fn _ => List.all (not o isEmpty) sliceas)
  val _ = ptest "test-vs-3a" test3a

  val test4a =
    check'(fn _ => eqv(vector (subslice(slicea23, 0, SOME 0)), l2v [])
	   andalso eqv(vector (subslice(slicea23, 0, NONE)), l2v [21,31,41])
	   andalso eqv(vector (subslice(slicea23, 0, SOME 1)), l2v [21])
	   andalso eqv(vector (subslice(slicea23, 0, SOME 2)), l2v [21,31])
	   andalso eqv(vector (subslice(slicea23, 1, SOME 2)), l2v [31,41])
	   andalso eqv(vector (subslice(slicea23, 3, SOME 0)), l2v []))
  val _ = ptest "test-vs-4a" test4a

  val test4b =
    (subslice(slicea23, 3, SOME 1); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-4b" test4b

  val test4c =
    (subslice(slicea23, ~1, NONE); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-4c" test4c

  val test4d =
    (subslice(slicea23, ~1, SOME 2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-4d" test4d

  val test4e =
    (subslice(slicea23, 4, NONE); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-4e" test4e

  val test4f =
    (subslice(slicea23, 4, SOME ~2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-4f" test4f

  val test4g =
    (subslice(slicea23, 2, SOME 2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-4g" test4g

  val test5 =
    check'(fn _ => let val (i1, r1) = Option.valOf (getItem slicea23)
		       val (i2, r2) = Option.valOf (getItem r1)
		       val (i3, r3) = Option.valOf (getItem r2)
		   in
		       i1 = i2w 21 andalso i2 = i2w 31 andalso i3 = i2w 41
		       andalso not (Option.isSome (getItem r3))
		   end)
  val _ = ptest "test-vs-5" test5

  val sliced = full (tabulate(100, fn i => i2w (i mod 7 * 10 + 1)))
  val sliceb = full b

  val e = A.array(203, i2w 0)
  val _ = (AS.copyVec{src=sliced, dst=e, di=0};
	   AS.copyVec{src=sliceb, dst=e, di=length sliced};
	   AS.copyVec{src=sliced, dst=e,
		      di=length sliced + length sliceb});

  val ev = V.concat [vector sliced, vector sliceb, vector sliced]
  (* length e = 203 *)

  val slicee = full (A.vector e)

  val test9a =
    check'(fn () => eqv(vector(subslice(slicee, 100, SOME 3)), vector sliceb))
  val _ = ptest "test-vs-9a" test9a

  val test9b =
    check'(fn () =>
	   eqv(ev, vector (subslice(slicee, 0, SOME (length slicee))))
	   andalso eqv(ev, vector (subslice(slicee, 0, NONE))))
  val _ = ptest "test-vs-9b" test9b

  val _ = AS.copyVec{src=slicee, dst=e, di=0}
  val g = A.array(203, i2w 99)
  val _ = AS.copyVec{src=slicee, dst=g, di=0}

  val sliceg = full (A.vector g)

  val test10a =
	   check'(fn () => eqv(ev, A.vector e)
		           andalso eqv(ev, A.vector g))
  val _ = ptest "test-vs-10a" test10a

  val sliceg0 = slice(A.vector g, 0, SOME (A.length g - 1))
  val _ = AS.copyVec{src=sliceg0, dst=g, di=1}

  val test10b =
    check'(fn () =>
	   eqv(vector sliceb, vector (slice(A.vector g, 101, SOME 3))))
  val _ = ptest "test-vs-10b" test10b

  val sliceg1 = slice(A.vector g, 1, SOME (A.length g - 1))
  val _ = AS.copyVec{src=sliceg1, dst=g, di=0}

  val test10c =
    check'(fn () =>
	   eqv(vector sliceb, vector (slice(A.vector g, 100, SOME 3))))
  val _ = ptest "test-vs-10c" test10c

  val sliceg202 = slice(A.vector g, 202, SOME 1)
  val _ = AS.copyVec{src=sliceg202, dst=g, di=202}

  val test10d =
    check'(fn () => A.sub(g, 202) = i2w(10 * (202-1-103) mod 7 + 1))
  val _ = ptest "test-vs-10d" test10d

  val test11a = (AS.copyVec{src=sliceg, dst=g, di= ~1}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-11a" test11a

  val test11b = (AS.copyVec{src=sliceg1, dst=g, di=0}; "OK")
                handle _ => "WRONG"
  val _ = ptest "test-vs-11b" test11b

  val test11c = (AS.copyVec{src=sliceg, dst=g, di=1}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-vs-11c" test11c


  val v = ref (i2w 0)
  fun setv c = v := c
  fun addv c = v := I.+(c, !v)
  fun setvi (i, c) = v := I.+(c, i2w i)
  fun setvif (i, c, _) = v := I.+(c, i2w i)
  fun addvi (i, c) = v := I.+(c, I.+(i2w i, !v))
  fun cons (x,r) = x :: r
  fun consi (i,x,r) = (i,x) :: r
  val inplist = [1,2,3,4,7,9,13,4,5,6,8,0]
  val inpa = l2v inplist
  val inp = slice(inpa, 4, SOME 3)
  val pnia = l2v (rev inplist)
  val pni = slice(pnia, 5, SOME 3)

  val test12a =
    check'(fn _ =>
	           (foldl cons (List.map i2w [1,2]) inp = List.map i2w [13,9,7,1,2])
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = i2w 13))
  val _ = ptest "test-vs-12a" test12a

  val test12b =
    check'(fn _ =>
	           foldr cons (List.map i2w [1,2]) inp = (List.map i2w [7,9,13,1,2])
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = i2w 7))
  val _ = ptest "test-vs-12b" test12b

  val test12c =
    check'(fn _ =>
	   find (fn _ => false) inp = NONE
	   andalso find (fn x => x = i2w 7) inp = SOME (i2w 7)
	   andalso find (fn x => x = i2w 9) inp = SOME (i2w 9)
	   andalso (setv (i2w 0); find (fn x => (addv x; x = i2w 9)) inp;
		    !v = I.+(i2w 7, i2w 9)));
  val _ = ptest "test-vs-12c" test12c

  val test12d =
    check'(fn _ =>
           ((setv (i2w 0); app addv inp; !v = i2w 29)
	    andalso (app setv inp; !v = i2w 13)));
  val _ = ptest "test-vs-12d" test12d

  val test12f =
    check'(fn _ =>
	   not (exists (fn i => i> i2w 13) inp)
	   andalso exists (fn i => i> i2w 12) inp);
  val _ = ptest "test-vs-12f" test12f

  val test12g =
    check'(fn _ =>
	   (setv (i2w 117); exists (fn x => (setv x; false)) slice05; !v = i2w 117)
	   andalso (setv (i2w 0); exists (fn x => (addv x; false)) inp;
		    !v = i2w 29)
	   andalso (exists (fn x => (setv x; false)) inp; !v = i2w 13));
  val _ = ptest "test-vs-12g" test12g

  val test12h =
    check'(fn _ =>
	   not (all (fn i => i< i2w 13) inp)
	   andalso all (fn i => i< i2w 14) inp);
  val _ = ptest "test-vs-12h" test12h

  val test12i =
    check'(fn _ =>
	   (setv (i2w 117); all (fn x => (setv x; true)) slice05; !v = i2w 117)
	   andalso (setv (i2w 0); all (fn x => (addv x; true)) inp;
		    !v = i2w 29)
	   andalso (all (fn x => (setv x; true)) inp; !v = i2w 13));
  val _ = ptest "test-vs-12i" test12i

  val test13 =
    check'(fn _ =>
	   foldli consi [] inp = [(2,i2w 13),(1, i2w 9),(0, i2w 7)]
	   andalso foldri consi [] inp = [(0,i2w 7),(1,i2w 9),(2,i2w 13)]
	   andalso (setv (i2w 117); foldli setvif () inp; !v = (i2w 15))
	   andalso (setv (i2w 117); foldri setvif () inp; !v = (i2w 7)))
  val _ = ptest "test-vs-13" test13

  val test14a =
    check'(fn _ =>
	   findi (fn _ => false) inp = NONE
	   andalso findi (fn (i,x) => x=i2w 9) inp = SOME (1,i2w 9)
	   andalso findi (fn (i,x) => i=2) inp = SOME (2, i2w 13));
  val _ = ptest "test-vs-14a" test14a

  val test14b =
    check'(fn _ =>
	   List.all (fn sli => NONE=findi (fn (j, x) => Int.>=(j,7) orelse Int.<(j, 0) orelse I.mod(x, i2w 10) <> i2w 1) sli)
	            sliceas)
  val _ = ptest "test-vs-14b" test14b

  val test15 =
    check'(fn _ =>
           ((setvi (0,i2w 0); appi addvi inp; !v = i2w(3+7+9+13))
	   andalso (appi setvi inp; !v = i2w 15)));
  val _ = ptest "test-vs-15" test15

  fun fromString s : vector =
      V.tabulate(size s,
                 fn i => I.fromLarge(Word8.toLargeInt(Byte.charToByte(String.sub(s,i)))))
  val test17a =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = I.compare (c2, c1)
	       fun coll s1 s2 =
		   collate invcompare (full (fromString s1),
				       full (fromString s2))
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
  val _ = ptest "test-vs-17a" test17a

  val test17b =
    check'(fn _ =>
  let val sa = fromString "AAAAaAbAABBBB"
                                (* 0123456789012 *)
      fun invcompare (c1, c2) = I.compare (c2, c1)
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
  val _ = ptest "test-vs-17b" test17b

  (* WordArray tests *)

  val () = pr_section "Array"

  open A
  infix 9 sub
  val array0 = fromList []

  val w127 = i2w 127

  val a = fromList (List.map i2w [0,1,2,3,4,5,6])
  val b = fromList (List.map i2w [44,55,66])
  val c = fromList (List.map i2w [0,1,2,3,4,5,6])

  val test1 =
    check'(fn () => a<>c)
  val _ = ptest "test-a-1" test1

  val test2 =
    check'(fn () =>
	   array(0, w127) <> array0
	   andalso array(0, i2w 127) <> tabulate(0, fn _ => i2w 127)
	   andalso tabulate(0, fn _ => i2w 127) <> fromList []
	   andalso array(0, w127) <> array(0, i2w 127)
	   andalso tabulate(0, fn _ => i2w 127) <> tabulate(0, fn _ => i2w 127)
	   andalso fromList [] <> fromList [])
  val _ = ptest "test-a-2" test2

  val d = tabulate(100, fn i => i2w (i mod 7))

  val test3 = check' (fn () => d sub 27 = i2w 6)
  val _ = ptest "test-a-3" test3

  val test4a = (tabulate(maxLen+1, i2w) seq "WRONG")
               handle Size => "OK" | _ => "WRONG"
  val _ = ptest "test-a-4a" test4a

  val test4b = (tabulate(~1, i2w)       seq "WRONG")
               handle Size => "OK" | _ => "WRONG"
  val _ = ptest "test-a-4b" test4b

  val test4c =
    check'(fn () => length (tabulate(0, fn i => i2w (i div 0))) = 0)
  val _ = ptest "test-a-4c" test4c

  val test5a = check'(fn () => length (fromList []) = 0 andalso length a = 7)
  val _ = ptest "test-a-5a" test5a
  val test5b = check'(fn () => length array0 = 0)
  val _ = ptest "test-a-5b" test5b

  val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
  val _ = ptest "test-a-6a" test6a
  val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
  val _ = ptest "test-a-6b" test6b
  val test6c = check'(fn () => c sub 0 = i2w 0)
  val _ = ptest "test-a-6c" test6c

  val e = array(203, i2w 0);
  val _ = (copy{src=d, dst=e, di=0};
	   copy{src=b, dst=e, di=length d};
	   copy{src=d, dst=e, di=length d + length b})

  fun a2v a = vector a
  val ev = V.concat [a2v d, a2v b, a2v d]

  val test7 = check'(fn () => length e = 203)
  val _ = ptest "test-a-7" test7

  val test8a = (update(e, ~1, i2w 127); "WRONG")
               handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-a-8a" test8a
  val test8b = (update(e, length e, i2w 127); "WRONG")
               handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-a-8b" test8b

  val f = AS.vector(AS.slice(e, 100, SOME 3))

  val test9 = check'(fn () => eqv(f, a2v b))
  val _ = ptest "test-a-9" test9

  val test9a =
    check'(fn () => eqv(ev, vector e))
  val _ = ptest "test-a-9a" test9a
  val test9b =
    check'(fn () => eqv(V.fromList [], vector array0))
  val _ = ptest "test-a-9b" test9b

  val _ = copy{src=e, dst=e, di=0}
  val g = array(203, i2w 127)
  val _ = copy{src=e, dst=g, di=0}

  val test10a = check'(fn () => eqv(ev, vector g))
  val _ = ptest "test-a-10a" test10a

  val test10b =
    check'(fn () => (copy{src=array0, dst=array0, di=0};
		     array0 <> array(0, i2w 99)))
  val _ = ptest "test-a-10b" test10b
  val test10c =
    check'(fn () => (copy{src=array0, dst=g, di=0};
		     eqv(ev, vector g)))
  val _ = ptest "test-a-10c" test10c
  val test10d =
    check'(fn () => (copy{src=array0, dst=g, di=203};
		     eqv(ev, vector g)))
  val _ = ptest "test-a-10d" test10d
  val test10e =
    check'(fn () => (copy{src=array0, dst=g, di=1};
		     eqv(ev, vector g)))
  val _ = ptest "test-a-10e" test10e

  val test11a = (copy{src=g, dst=g, di=1}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-a-11a" test11a
  val test11b = (copy{src=g, dst=g, di= 202}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-a-11b" test11b
  val test11c = (copy{src=b, dst=g, di = ~1}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-a-11c" test11c
  val test11d = (copy{src=b, dst=g, di=203}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-a-11d" test11d
  val test11e = check'(fn () => eqv(ev, vector g))
  val _ = ptest "test-a-11e" test11e

  val test12 =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = I.compare(c2, c1)
	       val fromString =
		   fromList o List.map (I.fromInt o ord) o explode
	       fun coll s1 s2 =
		   collate invcompare (fromString s1, fromString s2)
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
  val _ = ptest "test-a-12" test12

  val test13 =
    check'(fn _ =>
	   NONE = find (fn i => i > i2w 7) a
	   andalso SOME (i2w 5) = find (fn i => i > i2w 4) a
	   andalso NONE = find (fn _ => true) (fromList []))
  val _ = ptest "test-a-13" test13

  val test14 =
    check'(fn _ =>
	   not (exists (fn i => i > i2w 7) a)
	   andalso exists (fn i => i > i2w 4) a
	   andalso not (exists (fn _ => true) (fromList [])))
  val _ = ptest "test-a-14" test14

  val test15 =
    check'(fn _ =>
	   not (all (fn i => i < i2w 6) a)
	   andalso all (fn i => i < i2w 7) a
	   andalso all (fn _ => false) (fromList []))
  val _ = ptest "test-a-15" test15

  (* WordArraySlice tests *)

  val () = pr_section "ArraySlice"

  open A AS
  infix 9 sub
  val array0 = fromList []
  fun cons (x,r) = x :: r
  fun consi (i,x,r) = (i,x) :: r
  fun l2a xs = A.fromList (List.map i2w xs)
  fun l2v xs = V.fromList (List.map i2w xs)
  fun cl2a xs = A.fromList (List.map (i2w o Char.ord) xs)

  val a = l2a [1,11,21,31,41,51,61]
  val b = l2a [44,55,66]
  val c = l2a [1,11,21,31,41,51,61]

  val slice00 = slice(array0, 0, NONE)
  val slice01 = slice(array0, 0, SOME 0)
  val slice02 = slice(a, 0, SOME 0)
  val slice03 = slice(a, 7, NONE)
  val slice04 = slice(a, 7, SOME 0)
  val slice05 = slice(a, 4, SOME 0)

  val slicea07 = full a
  val slicea02 = slice(a, 0, SOME 2)
  val slicea23 = slice(a, 2, SOME 3)
  val slicea25 = slice(a, 2, SOME 5)

  val slice06 = subslice(slicea23, 0, SOME 0)
  val slice07 = subslice(slicea23, 1, SOME 0)
  val slice08 = subslice(slicea23, 3, NONE)
  val slice09 = subslice(slicea23, 3, SOME 0)

  val slice0s = [slice00, slice01, slice02, slice03, slice04, slice05,
	         slice06, slice07, slice08, slice09]

  val sliceas = [slicea07, slicea02, slicea23, slicea25]

  val test1a =
    check'(fn _ => List.all
	   (fn sli => eqv(vector sli, l2v [])
	    andalso length sli = 0
	    andalso isEmpty sli
	    andalso eqv(vector (subslice(sli, 0, NONE)), l2v [])
	    andalso eqv(vector (subslice(sli, 0, SOME 0)), l2v [])
	    andalso all (fn _ => false) sli
	    andalso not (exists (fn _ => true) sli)
	    andalso NONE = find (fn _ => true) sli
	    andalso NONE = findi (fn _ => true) sli
	    andalso not (Option.isSome (getItem sli))
	    andalso (copy{src=sli, dst=array0, di=0}; true)
	    andalso (app (fn _ => raise Fail "1a app") sli; true)
	    andalso (appi (fn _ => raise Fail "1a appi") sli; true)
	    andalso foldl cons [i2w 1,i2w 2] sli = [i2w 1,i2w 2]
	    andalso foldli consi [] sli = []
	    andalso foldr cons [i2w 1,i2w 2] sli = [i2w 1,i2w 2]
	    andalso foldri consi [] sli = []
	    andalso (modify I.~ sli; eqv(vector sli, l2v []))
	    andalso (modifyi (fn (_, x) => I.~ x) sli; eqv(vector sli, l2v []))
	    andalso collate I.compare (sli, slice00) = EQUAL)
	   slice0s)
  val _ = ptest "test-as-1a" test1a

  val test1b =
    check'(fn _ =>
	   eqv(vector slicea02, l2v[1, 11])
	   andalso eqv(vector slicea23, l2v[21,31,41])
	   andalso eqv(vector slicea25, l2v[21,31,41,51,61])
	   andalso eqv(vector slicea07, l2v[1,11,21,31,41,51,61])
	   andalso base slicea02 = (a, 0, 2)
	   andalso base slicea23 = (a, 2, 3)
	   andalso base slicea25 = (a, 2, 5)
	   andalso base slicea07 = (a, 0, 7)
	   andalso length slicea02 = 2
	   andalso length slicea23 = 3
	   andalso length slicea25 = 5
	   andalso length slicea07 = 7)
  val _ = ptest "test-as-1b" test1b

  val test2a =
    check'(fn _ =>
	   slicea07 sub 0 = i2w 1
	   andalso slicea07 sub 6 = i2w 61
	   andalso slicea23 sub 0 = i2w 21
	   andalso slicea23 sub 2 = i2w 41)
  val _ = ptest "test-as-2a" test2a

  val test2b =
    (slicea07 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-2b" test2b

  val test2c =
    (slicea07 sub 7; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-2c" test2c

  val test2cc =
    (slicea23 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-2cc" test2cc

  val test2d =
    (slicea23 sub 3; "WRONG") handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as2d" test2d

  val test2e =
    check'(fn _ =>
	   List.all (fn sli => ((sli sub 0; false)
				handle Subscript => true)) slice0s);
  val _ = ptest "test-as-2e" test2e

  val test3a =
    check'(fn _ => List.all (not o isEmpty) sliceas)
  val _ = ptest "test-as-3a" test3a

  val test4a =
    check'(fn _ => eqv(vector (subslice(slicea23, 0, SOME 0)), l2v[])
	   andalso eqv(vector (subslice(slicea23, 0, NONE)), l2v[21,31,41])
	   andalso eqv(vector (subslice(slicea23, 0, SOME 1)), l2v[21])
	   andalso eqv(vector (subslice(slicea23, 0, SOME 2)), l2v[21,31])
	   andalso eqv(vector (subslice(slicea23, 1, SOME 2)), l2v[31,41])
	   andalso eqv(vector (subslice(slicea23, 3, SOME 0)), l2v[]))
  val _ = ptest "test-as-4a" test4a

  val test4b =
    (subslice(slicea23, 3, SOME 1); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-4b" test4b

  val test4c =
    (subslice(slicea23, ~1, NONE); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-4c" test4c

  val test4d =
    (subslice(slicea23, ~1, SOME 2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-4d" test4d

  val test4e =
    (subslice(slicea23, 4, NONE); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-4e" test4e

  val test4f =
    (subslice(slicea23, 4, SOME ~2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-4f" test4f

  val test4g =
    (subslice(slicea23, 2, SOME 2); "WRONG")
    handle Subscript => "OK" | _ => "WRONG";
  val _ = ptest "test-as-4g" test4g

  val test5 =
    check'(fn _ => let val (i1, r1) = Option.valOf (getItem slicea23)
		       val (i2, r2) = Option.valOf (getItem r1)
		       val (i3, r3) = Option.valOf (getItem r2)
		   in
		       i1 = i2w 21 andalso i2 = i2w 31 andalso i3 = i2w 41
		       andalso not (Option.isSome (getItem r3))
		   end);
  val _ = ptest "test-as-5" test5

  val test6a = (update(slicea23, ~1, i2w 99) seq "WRONG")
               handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-6a" test6a
  val test6b = (update(slicea23, 3, i2w 99) seq "WRONG")
               handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-6b" test6b

  val test6c =
    check'(fn _ =>
	   (update(slicea23, 0, i2w 99); A.sub(a, 2) = i2w 99)
	   andalso (update(slicea23, 2, i2w 17); A.sub(a, 4) = i2w 17)
	   andalso (update(slicea23, 0, i2w 21);  A.sub(a, 2) = i2w 21)
	   andalso (update(slicea23, 2, i2w 41);  A.sub(a, 4) = i2w 41))
  val _ = ptest "test-as-6c" test6c

  val sliced = full (tabulate(100, fn i => i2w(i mod 7 * 10 + 1)))
  val sliceb = full b

  val e = array(203, i2w 0)
  val _ = (copy{src=sliced, dst=e, di=0};
	   copy{src=sliceb, dst=e, di=length sliced};
	   copy{src=sliced, dst=e, di=length sliced + length sliceb})

  val ev = V.concat [vector sliced, vector sliceb, vector sliced]
  (* length e = 203 *)

  val slicee = full e

  val test9a =
    check'(fn () => eqv(vector(subslice(slicee, 100, SOME 3)), vector sliceb))
  val _ = ptest "test-as-9a" test9a
  val test9b =
    check'(fn () =>
	   eqv(ev, vector (subslice(slicee, 0, SOME (length slicee))))
	   andalso eqv(ev, vector (subslice(slicee, 0, NONE))))
  val _ = ptest "test-as-9b" test9b

  val _ = copy{src=slicee, dst=e, di=0}
  val g = array(203, i2w 99)
  val _ = copy{src=slicee, dst=g, di=0}

  val sliceg = full g

  val test10a =
	   check'(fn () => eqv(ev, A.vector e)
		  andalso eqv(ev, A.vector g))
  val _ = ptest "test-as-10a" test10a

  val sliceg0 = slice(g, 0, SOME (A.length g - 1))
  val _ = copy{src=sliceg0, dst=g, di=1}
  val test10b = check'(fn () => eqv(vector sliceb, vector (slice(g, 101, SOME 3))))
  val _ = ptest "test-as-10b" test10b

  val sliceg1 = slice(g, 1, SOME (A.length g - 1))
  val _ = copy{src=sliceg1, dst=g, di=0}
  val test10c = check'(fn () => eqv(vector sliceb, vector (slice(g, 100, SOME 3))))
  val _ = ptest "test-as-10c" test10c

  val sliceg202 = slice(g, 202, SOME 1)
  val _ = copy{src=sliceg202, dst=g, di=202}
  val test10d =
    check'(fn () => sliceg sub 202 = i2w(10 * (202-1-103) mod 7 + 1))
  val _ = ptest "test-as-10d" test10d

  val test11a = (copy{src=sliceg, dst=g, di= ~1}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-11a" test11a
  val test11b = (copy{src=sliceg1, dst=g, di=0}; "OK")
                handle _ => "WRONG"
  val _ = ptest "test-as-11b" test11b
  val test11c = (copy{src=sliceg, dst=g, di=1}; "WRONG")
                handle Subscript => "OK" | _ => "WRONG"
  val _ = ptest "test-as-11c" test11c

  val v = ref (i2w 0)
  fun setv c = v := c
  fun addv c = v := I.+(c, !v)
  fun setvi (i, c) = v := I.+(c, i2w i)
  fun setvif (i, c, _) = v := I.+(c, i2w i)
  fun addvi (i, c) = v := I.+(c, I.+(i2w i, !v))
  fun cons (x,r) = x :: r
  fun consi (i,x,r) = (i,x) :: r
  val inplist = [1,2,3,4,7,9,13,4,5,6,8,0]
  val inpa = l2a inplist
  val inp = slice(inpa, 4, SOME 3)
  val pnia = l2a (rev inplist)
  val pni = slice(pnia, 5, SOME 3)
  fun resetinp () = copy{src=full(l2a inplist), dst=inpa, di=0}

  val test12a =
    check'(fn _ =>
	           foldl cons (List.map i2w [1,2]) inp = List.map i2w [13,9,7,1,2]
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = i2w 13))
  val _ = ptest "test-as-12a" test12a

  val test12b =
    check'(fn _ =>
	           foldr cons (List.map i2w [1,2]) inp = List.map i2w [7,9,13,1,2]
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = i2w 7))
  val _ = ptest "test-as-12b" test12b

val test12c =
    check'(fn _ =>
	   find (fn _ => false) inp = NONE
	   andalso find (fn x => x = i2w 7) inp = SOME (i2w 7)
	   andalso find (fn x => x = i2w 9) inp = SOME (i2w 9)
	   andalso (setv (i2w 0); find (fn x => (addv x; x = i2w 9)) inp; !v = i2w 16));
  val _ = ptest "test-as-12c" test12c

  val test12d =
    check'(fn _ =>
           ((setv (i2w 0); app addv inp; !v = i2w 29)
	    andalso (app setv inp; !v = i2w 13)));
  val _ = ptest "test-as-12d" test12d

  val test12e =
    check'(fn _ =>
	   (resetinp(); modify I.~ inp;
	    foldr (op::) [] inp = [I.~ (i2w 7), I.~ (i2w 9), I.~ (i2w 13)])
	   andalso (resetinp(); setv (i2w 117);
		    modify (fn x => (setv x; i2w 37)) inp; !v = i2w 13))
  val _ = ptest "test-as-12e" test12e

  val _ = resetinp()

  val test12f =
    check'(fn _ =>
	   not (exists (fn i => i > i2w 13) inp)
	   andalso exists (fn i => i > i2w 12) inp)
  val _ = ptest "test-as-12f" test12f
  val test12g =
    check'(fn _ =>
	   (setv (i2w 117); exists (fn x => (setv x; false)) slice05; !v = i2w 117)
	   andalso (setv (i2w 0); exists (fn x => (addv x; false)) inp;
		    !v = i2w 29)
	   andalso (exists (fn x => (setv x; false)) inp; !v = i2w 13))
  val _ = ptest "test-as-12g" test12g
  val test12h =
    check'(fn _ =>
	   not (all (fn i => i < i2w 13) inp)
	   andalso all (fn i => i < i2w 14) inp)
  val _ = ptest "test-as-12h" test12h
  val test12i =
    check'(fn _ =>
	   (setv (i2w 117); all (fn x => (setv x; true)) slice05; !v = i2w 117)
	   andalso (setv (i2w 0); all (fn x => (addv x; true)) inp;
		    !v = i2w 29)
	   andalso (all (fn x => (setv x; true)) inp; !v = i2w 13))
  val _ = ptest "test-as-12i" test12i

  val _ = resetinp()

  val test13 =
    check'(fn _ =>
	   foldli consi [] inp = [(2,i2w 13),(1, i2w 9),(0, i2w 7)]
	   andalso foldri consi [] inp = [(0,i2w 7),(1,i2w 9),(2,i2w 13)]
	   andalso (resetinp(); setv (i2w 117);
		    foldli setvif () inp; !v = i2w 15)
	   andalso (resetinp(); setv (i2w 117);
		    foldri setvif () inp; !v = i2w 7))
  val _ = ptest "test-as-13" test13

  val _ = resetinp()

  val test14a =
    check'(fn _ =>
	   findi (fn _ => false) inp = NONE
	   andalso findi (fn (i,x) => x = i2w 9) inp = SOME (1,i2w 9)
	   andalso findi (fn (i,x) => i = 2) inp = SOME (2, i2w 13))
  val _ = ptest "test-as-14a" test14a

  val test14b =
    check'(fn _ =>
	      List.all (fn sli => NONE=findi (fn (j, x) => Int.>=(j, 7) orelse Int.<(j, 0)
                                                           orelse I.mod(x, i2w 10) <> i2w 1) sli)
	            sliceas)
  val _ = ptest "test-as-14b" test14b

  val test15 =
    check'(fn _ =>
           ((setvi (0,i2w 0); appi addvi inp; !v = i2w 32)
	    andalso (appi setvi inp; !v = i2w 15)))
  val _ = ptest "test-as-15" test15

  val test16 =
    check'(fn _ =>
	   let fun iwsub (i, w) = I.-(i2w i, w)
	   in
	       (resetinp(); modifyi iwsub inp;
		eqv(vector inp, V.fromList (List.map I.fromInt [~7,~8,~11])))
	       andalso (resetinp(); setv (i2w 117);
			modifyi (fn x => (setvi x; i2w 37)) inp; !v = i2w 15)
	   end)
  val _ = ptest "test-as-16" test16

  val test17a =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = I.compare (c2, c1)
	       fun coll s1 s2 =
		   collate invcompare (full (cl2a (explode s1)),
				       full (cl2a (explode s2)))
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
  val _ = ptest "test-as-17a" test17a

  val test17b =
    check'(fn _ =>
              let val sa = cl2a (explode "AAAAaAbAABBBB")
                  (* 0123456789012 *)
                  fun invcompare (c1, c2) = I.compare (c2, c1)
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
  val _ = ptest "test-as-17b" test17b

end


structure TV8 = Test(structure I = Int8
                     structure V = Int8Vector
                     structure A = Int8Array
                     structure VS = Int8VectorSlice
                     structure AS = Int8ArraySlice)

structure TV16 = Test(structure I = Int16
                      structure V = Int16Vector
                      structure A = Int16Array
                      structure VS = Int16VectorSlice
                      structure AS = Int16ArraySlice)

structure TV31 = Test(structure I = Int31
                      structure V = Int31Vector
                      structure A = Int31Array
                      structure VS = Int31VectorSlice
                      structure AS = Int31ArraySlice)

structure TV32 = Test(structure I = Int32
                      structure V = Int32Vector
                      structure A = Int32Array
                      structure VS = Int32VectorSlice
                      structure AS = Int32ArraySlice)

structure TV63 = Test(structure I = Int63
                      structure V = Int63Vector
                      structure A = Int63Array
                      structure VS = Int63VectorSlice
                      structure AS = Int63ArraySlice)

structure TV64 = Test(structure I = Int64
                      structure V = Int64Vector
                      structure A = Int64Array
                      structure VS = Int64VectorSlice
                      structure AS = Int64ArraySlice)

val () = pr_n := false

structure TV = Test(structure I = Int
                    structure V = IntVector
                    structure A = IntArray
                    structure VS = IntVectorSlice
                    structure AS = IntArraySlice)

val () = report ()
