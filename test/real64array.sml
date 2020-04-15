(* test/real64array.sml -- some test cases for Real64Array *)

fun ptest t s = print(t ^ ": " ^ s ^ "\n")
infix 1 seq
fun e1 seq e2 = e2
fun check b = if b then "OK" else "WRONG"
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN"

fun range (from, to) p =
    let open Int
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end

fun checkrange bounds p = check'(fn _ => range bounds p)

local
  val op == = Real.==
  infix ==

  fun eq_opt eq (NONE,NONE) = true
    | eq_opt eq (SOME a,SOME b) = eq(a,b)
    | eq_opt _ _ = false

  open Real64Array
  val i2r = Real64.fromInt
  infix 9 sub
  val array0 = fromList []
  fun eq (v1,v2) =
      Real64Vector.length v1 = Real64Vector.length v2 andalso
      let fun loop (i:int) =
              if i >= Real64Vector.length v1 then true
              else Real64Vector.sub(v1,i) == Real64Vector.sub(v2,i) andalso loop (i+1)
      in loop 0
      end
  val op === = eq               (* structural equality on vectors *)
  infix ===
in

val r127 = i2r 127

val a = fromList (map i2r [0,1,2,3,4,5,6])
val b = fromList (map i2r [44,55,66])
val c = fromList (map i2r [0,1,2,3,4,5,6])

val test1 =
    check'(fn () => a<>c)
val _ = ptest "test1" test1
val test2 =
    check'(fn () =>
	   array(0, r127) <> array0
	   andalso array(0, r127) <> tabulate(0, fn _ => r127)
	   andalso tabulate(0, fn _ => r127) <> fromList []
	   andalso array(0, r127) <> array(0, r127)
	   andalso tabulate(0, fn _ => r127) <> tabulate(0, fn _ => r127)
	   andalso fromList [] <> fromList [])
val _ = ptest "test2" test2

val d = tabulate(100, fn i => i2r (i mod 7))

val test3 = check' (fn () => d sub 27 == i2r 6)
val _ = ptest "test3" test3

val test4a = (tabulate(maxLen+1, i2r) seq "WRONG")
             handle Size => "OK" | _ => "EXN"
val _ = ptest "test4a" test4a

val test4b = (tabulate(~1, i2r) seq "WRONG")
             handle Size => "OK" | _ => "EXN"
val _ = ptest "test4b" test4b

val test4c =
    check'(fn () => length (tabulate(0, fn i => i2r (i div 0))) = 0)
val _ = ptest "test4c" test4c

val test5a = check'(fn () => length (fromList []) = 0 andalso length a = 7)
val _ = ptest "test5a" test5a
val test5b = check'(fn () => length array0 = 0)
val _ = ptest "test5b" test5b

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "EXN"
val _ = ptest "test6a" test6a
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "EXN"
val _ = ptest "test6b" test6b
val test6c = check'(fn () => c sub 0 == i2r 0)
val _ = ptest "test6c" test6c

val e = array(203, i2r 0)
val _ = (copy{src=d, dst=e, di=0};
	 copy{src=b, dst=e, di=length d};
	 copy{src=d, dst=e, di=length d + length b})

fun a2v a = vector a
val ev = Real64Vector.concat [a2v d, a2v b, a2v d]

val test7 = check'(fn () => length e = 203)
val _ = ptest "test7" test7

val test8a = (update(e, ~1, r127); "WRONG")
             handle Subscript => "OK" | _ => "WRONG"
val _ = ptest "test8a" test8a
val test8b = (update(e, length e, r127); "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val _ = ptest "test8b" test8b

(*val f = Real64ArraySlice.vector(Real64ArraySlice.slice(e, 100, SOME 3))  <-------- MEMO *)
val f = Real64Vector.fromList [44.0,55.0,66.0]

val test9 = check'(fn () => f === a2v b)
val _ = ptest "test9" test9

val test9a =
    check'(fn () => ev === vector e)
val _ = ptest "test9a" test9a
val test9b =
    check'(fn () => Real64Vector.fromList [] === vector array0);
val _ = ptest "test9b" test9b

val _ = copy{src=e, dst=e, di=0}
val g = array(203, r127)
val _ = copy{src=e, dst=g, di=0}

val test10a = check'(fn () => ev === vector g)
val _ = ptest "test10a" test10a

val test10b =
    check'(fn () => (copy{src=array0, dst=array0, di=0};
		     array0 <> array(0, 99.0)))
val _ = ptest "test10b" test10b
val test10c =
    check'(fn () => (copy{src=array0, dst=g, di=0};
		     ev === vector g))
val _ = ptest "test10c" test10c
val test10d =
    check'(fn () => (copy{src=array0, dst=g, di=203};
		     ev === vector g))
val _ = ptest "test10d" test10d
val test10e =
    check'(fn () => (copy{src=array0, dst=g, di=1};
		     ev === vector g))
val _ = ptest "test10e" test10e

val test11a = (copy{src=g, dst=g, di=1}; "WRONG")
              handle Subscript => "OK" | _ => "EXN"
val _ = ptest "test11a" test11a
val test11b = (copy{src=g, dst=g, di=202}; "WRONG")
              handle Subscript => "OK" | _ => "EXN"
val _ = ptest "test11b" test11b
val test11c = (copy{src=b, dst=g, di= ~1}; "WRONG")
              handle Subscript => "OK" | _ => "EXN"
val _ = ptest "test11c" test11c
val test11d = (copy{src=b, dst=g, di=203}; "WRONG")
              handle Subscript => "OK" | _ => "EXN"
val _ = ptest "test11d" test11d
val test11e = check'(fn () => ev === vector g)
val _ = ptest "test11e" test11e

val test12 =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Real64.compare(c2, c1)
	       val fromString =
		   fromList o List.map (Real64.fromInt o ord) o explode
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
val _ = ptest "test12" test12

val test13 =
    check'(fn _ =>
	   eq_opt (op ==) (NONE, find (fn i => i > 7.0) a)
	   andalso eq_opt (op ==) (SOME 5.0, find (fn i => i > 4.0) a)
	   andalso eq_opt (op ==) (NONE, find (fn _ => true) (fromList [])))
val _ = ptest "test13" test13

val test14 =
    check'(fn _ =>
	   not (exists (fn i => i > 7.0) a)
	   andalso exists (fn i => i > 4.0) a
	   andalso not (exists (fn _ => true) (fromList [])))
val _ = ptest "test14" test14

val test15 =
    check'(fn _ =>
	   not (all (fn i => i < 6.0) a)
	   andalso all (fn i => i < 7.0) a
	   andalso all (fn _ => false) (fromList []))
val _ = ptest "test15" test15
end
