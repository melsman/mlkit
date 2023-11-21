(* test/word-n-vector.sml -- some test cases for Word<N>Vector
   structures. PS 1994-12-10, 1995-06-14, 2000-10-24

   - modified for the MLKit; mael 2005-11-28, mael 2023-11-21
 *)

fun ptest t s = print(t ^ ": " ^ s ^ "\n")
infix 1 seq
fun e1 seq e2 = e2
fun check b = if b then "OK" else "WRONG"
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN"
fun range (from, to) p =
    let open Int
    in (from > to) orelse (p from) andalso (range (from+1, to) p)
    end
fun checkrange bounds p = check'(fn _ => range bounds p)

functor Test(structure W : WORD
             structure V : MONO_VECTOR
             structure VS : MONO_VECTOR_SLICE
             sharing type W.word = V.elem = VS.elem
             sharing type V.vector = VS.vector
             val eqv : V.vector * V.vector -> bool) : sig end =
struct
  open V
  val i2w = W.fromInt
  infix 9 sub
  val neqv = not o eqv

  fun pr x =
      "[" ^ String.concatWith "," (List.tabulate(length x, fn i => W.toString (V.sub(x,i))))
      ^ "]"

  val op < = W.<
  val op > = W.>

  val a = fromList (List.map i2w [0,1,2,3,4,5,6])
  val b = fromList (List.map i2w [44,55,66])
  val c = fromList (List.map i2w [0,1,2,3,4,5,6])

  val hi = case W.wordSize of
               8 => 255
             | 31 => 1073741823
             | 64 => 1073741823*1073741823
             | 63 => 1073741823*1073741823
             | _ => 300
  val h : W.word = i2w hi
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
	        let val last = ref (i2w 255)
	            val res = iter (fn x => (last := x; f x)) vec
	        in eq(res, !last) end)

  fun chkiteri iter f vec eq =
      check'(fn _ =>
	        let val last = ref ~1
	            val res = iter (fn (i, x) => (last := i; f x)) vec
	        in eq(res, !last) end)

  fun chkfold fold f start vec reslast =
      check'(fn _ =>
	        let val last = ref (i2w 255)
	            val res = fold (fn (x, r) => (last := x; f(x, r))) start vec
	        in (res, !last) = reslast end)

  fun chkfoldi fold f start vec reslast =
      check'(fn _ =>
	        let val last = ref ~1
	            val res = fold (fn (i, x, r) => (last := i; f(x, r))) start vec
	        in (res, !last) = reslast end)

  val test10a1 =
      chkiter map (fn x => W.*(i2w 2,x)) b
              (fn (x,y) => eqv(x,fromList (List.map i2w [88,110,132])))
  val _ = ptest "test10a1" test10a1

  val test10a2 =
      chkiter map (fn x => W.*(i2w 2,x)) b
              (fn (x,y) => y = i2w 66)
  val _ = ptest "test10a2" test10a2

  val test10b =
      chkiter app (fn x => ignore(W.*(i2w 2,x))) b
              (fn ((),y) => y = i2w 66)
  val _ = ptest "test10b" test10b

  val test10c =
      chkiter find (fn x => false) b
              (fn (opt,y) => opt = NONE andalso y = i2w 66)
  val _ = ptest "test10c" test10c

  val test10d =
      chkiter exists (fn x => false) b
              (fn (b,y) => b = false andalso y = i2w 66)
  val _ = ptest "test10d" test10d

  val test10e =
      chkiter all (fn x => true) b
              (fn (b,y) => b andalso y = i2w 66)
  val _ = ptest "test10e" test10e

  val test10f =
      chkfold foldl (W.+) (i2w 0) b (i2w 165, i2w 66)
  val _ = ptest "test10f" test10f

  val test10g =
      chkfold foldr (W.+) (i2w 0) b (i2w 165, i2w 44)
  val _ = ptest "test10g" test10g

  val test11a =
      chkiteri mapi (fn x => W.*(i2w 2,x)) b
               (fn (x,y) => eqv(x,fromList (List.map i2w [88,110,132]))
                            andalso y = 2)
  val _ = ptest "test11a" test11a

  val test11b =
      chkiteri appi (fn x => ignore(W.*(i2w 2,x))) b
               (fn ((),y) => y = 2)
  val _ = ptest "test11b" test11b

  val test11c =
      chkiteri findi (fn x => false) b
               (fn (opt, y) => opt = NONE andalso y = 2)
  val _ = ptest "test11c" test11c

  val test11d =
      chkfoldi foldli (W.+) (i2w 0) b (i2w 165, 2)
  val _ = ptest "test11d" test11d

  val test11e =
      chkfoldi foldri (W.+) (i2w 0) b (i2w 165, 0)
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
                 fn i => W.fromLarge(Word8.toLarge(Byte.charToByte(String.sub(s,i)))))

  val test13 =
    check'(fn _ =>
	   let fun invcompare (c1, c2) = W.compare(c2, c1)
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
end

val () = print "[Testing Word8Vector]\n"

structure TV8 = Test(structure W = Word8
                     structure V = Word8Vector
                     structure VS = Word8VectorSlice
                     val eqv : V.vector * V.vector -> bool = (op =))

val () = print "[Testing Word31Vector]\n"

structure TV31 = Test(structure W = Word31
                      structure V = Word31Vector
                      structure VS = Word31VectorSlice
                      val eqv : V.vector * V.vector -> bool = (op =))

val () = print "[Testing Word32Vector]\n"

structure TV32 = Test(structure W = Word32
                      structure V = Word32Vector
                      structure VS = Word32VectorSlice
                      val eqv : V.vector * V.vector -> bool = (op =))
(*
val () = print "[Testing Word63Vector]\n"

structure TV63 = Test(structure W = Word63
                      structure V = Word63Vector
                      structure VS = Word63VectorSlice
                      val eqv : V.vector * V.vector -> bool = (op =))
*)
val () = print "[Testing Word64Vector]\n"

structure TV64 = Test(structure W = Word64
                      structure V = Word64Vector
                      structure VS = Word64VectorSlice
                      val eqv : V.vector * V.vector -> bool = (op =))

val () = print "[Testing WordVector]\n"

structure TV = Test(structure W = Word
                    structure V = WordVector
                    structure VS = WordVectorSlice
                    val eqv : V.vector * V.vector -> bool = (op =))
