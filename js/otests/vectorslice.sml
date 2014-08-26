(* File vectorslice.sml: Testing VectorSlice operations.
 * Copyright (c) 2000-2014, Peter Sestoft, Martin Elsman.
 * MIT License.
 *)

open Utest
val _ = tstStart "structure VectorSlice"

local 
    open Vector VectorSlice 
    infix 9 sub
    val array0 = Array.fromList []
    val vec0 = fromList []
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
in

val a = fromList [1,11,21,31,41,51,61]
val b = fromList [441,551,661]
val c = fromList [1,11,21,31,41,51,61]

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
    tst' "test1a" (fn _ => List.all 
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
           slice0s)

val test1b = 
    tst' "test1b" (fn _ => 
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
           andalso length slicea07 = 7)

val test2a = 
    tst' "test2a" (fn _ => 
           slicea07 sub 0 = 1 
           andalso slicea07 sub 6 = 61 
           andalso slicea23 sub 0 = 21 
           andalso slicea23 sub 2 = 41)

val test2b = 
    tst' "test2b" (fn () => (slicea07 sub ~1; false) handle Subscript => true)

val test2c = 
    tst' "test2c" (fn () => (slicea07 sub 7; false) handle Subscript => true)

val test2cc = 
    tst' "test2cc" (fn () => (slicea23 sub ~1; false) handle Subscript => true)

val test2d = 
    tst' "test2d" (fn () => (slicea23 sub 3; false) handle Subscript => true)

val test2e = 
    tst' "test2e" (fn _ =>
           List.all (fn sli => ((sli sub 0; false) 
                                handle Subscript => true)) slice0s)

val test3a = 
    tst' "test3a" (fn _ => List.all (not o isEmpty) sliceas)

val test4a =
    tst' "test4a" (fn _ => vector (subslice(slicea23, 0, SOME 0)) = Vector.fromList[]
           andalso vector (subslice(slicea23, 0, NONE)) = Vector.fromList[21,31,41]
           andalso vector (subslice(slicea23, 0, SOME 1)) = Vector.fromList[21]
           andalso vector (subslice(slicea23, 0, SOME 2)) = Vector.fromList[21,31]
           andalso vector (subslice(slicea23, 1, SOME 2)) = Vector.fromList[31,41]
           andalso vector (subslice(slicea23, 3, SOME 0)) = Vector.fromList[])

val test4b =
    tst' "test4b" (fn () => (subslice(slicea23, 3, SOME 1); false) 
                            handle Subscript => true)

val test4c =
    tst' "test4c" (fn () => (subslice(slicea23, ~1, NONE); false) 
                            handle Subscript => true)

val test4d =
    tst' "test4d" (fn () => (subslice(slicea23, ~1, SOME 2); false) 
                            handle Subscript => true)

val test4e =
    tst' "test4e" (fn () => (subslice(slicea23, 4, NONE); false) 
                            handle Subscript => true)  

val test4f =
    tst' "test4f" (fn () => (subslice(slicea23, 4, SOME ~2); false) 
                            handle Subscript => true)

val test4g =
    tst' "test4g" (fn () => (subslice(slicea23, 2, SOME 2); false) 
                            handle Subscript => true)

val test5 = 
    tst' "test5" (fn _ => let val (i1, r1) = Option.valOf (getItem slicea23)
                              val (i2, r2) = Option.valOf (getItem r1)
                              val (i3, r3) = Option.valOf (getItem r2)
                          in 
                            i1 = 21 andalso i2 = 31 andalso i3 = 41 
                            andalso not (Option.isSome (getItem r3))
                          end)

val sliced = full (tabulate(100, fn i => i mod 7 * 10 + 1))
val sliceb = full b

val e = Array.array(203, 0)
val _ = (ArraySlice.copyVec{src=sliced, dst=e, di=0}; 
         ArraySlice.copyVec{src=sliceb, dst=e, di=length sliced};
         ArraySlice.copyVec{src=sliced, dst=e, 
                            di=length sliced + length sliceb})

val ev = Vector.concat [vector sliced, vector sliceb, vector sliced] 
(* length e = 203 *)

val slicee = full (Array.vector e)

val test9a = 
    tst' "test9a" (fn () => vector(subslice(slicee, 100, SOME 3)) = vector sliceb)

val test9b = 
    tst' "test9b" (fn () => 
                      ev = vector (subslice(slicee, 0, SOME (length slicee)))
                      andalso ev = vector (subslice(slicee, 0, NONE)))

val _ = ArraySlice.copyVec{src=slicee, dst=e, di=0}
val g = Array.array(203, 9999999)
val _ = ArraySlice.copyVec{src=slicee, dst=g, di=0}

val sliceg = full (Array.vector g)

val test10a = 
    tst' "test10a" (fn () => ev = Array.vector e
                             andalso ev = Array.vector g)

val sliceg0 = slice(Array.vector g, 0, SOME (Array.length g - 1))
val _ = ArraySlice.copyVec{src=sliceg0, dst=g, di=1}

val test10b = 
    tst' "test10b" (fn () => 
                       vector sliceb = vector (slice(Array.vector g, 101, SOME 3)))

val sliceg1 = slice(Array.vector g, 1, SOME (Array.length g - 1))
val _ = ArraySlice.copyVec{src=sliceg1, dst=g, di=0}

val test10c = 
    tst' "test10c" (fn () => 
                       vector sliceb = vector (slice(Array.vector g, 100, SOME 3)))

val sliceg202 = slice(Array.vector g, 202, SOME 1)
val _ = ArraySlice.copyVec{src=sliceg202, dst=g, di=202}

val test10d = 
    tst' "test10d" (fn () => Array.sub(g, 202) = 10 * (202-1-103) mod 7 + 1)

val test11a = 
    tst' "test11a" (fn () => (ArraySlice.copyVec{src=sliceg, dst=g, di= ~1}; false) 
                             handle Subscript => true)

val test11b =
    tst' "test11b" (fn () => (ArraySlice.copyVec{src=sliceg1, dst=g, di=0}; true) 
                             handle _ => false)

val test11c =
    tst' "test11c" (fn () => (ArraySlice.copyVec{src=sliceg, dst=g, di=1}; false) 
                             handle Subscript => true)

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
    tst' "test12a" (fn _ =>
                       foldl cons [1,2] inp = [13,9,7,1,2]
                       andalso (foldl (fn (x, _) => setv x) () inp; !v = 13));

val test12b =
    tst' "test12b" (fn _ =>
                       foldr cons [1,2] inp = [7,9,13,1,2]
                       andalso (foldr (fn (x, _) => setv x) () inp; !v = 7));

val test12c =
    tst' "test12c" (fn _ =>
                       find (fn _ => false) inp = NONE
                       andalso find (fn x => x=7) inp = SOME 7
                       andalso find (fn x => x=9) inp = SOME 9
                       andalso (setv 0; find (fn x => (addv x; x=9)) inp; !v = 7+9));

val test12d = 
    tst' "test12d" (fn _ =>
                       ((setv 0; app addv inp; !v = 7+9+13)
                        andalso (app setv inp; !v = 13)));

val test12f = 
    tst' "test12f" (fn _ => 
                       not (exists (fn i => i>13) inp)
                       andalso exists (fn i => i>12) inp);

val test12g = 
    tst' "test12g" (fn _ => 
                       (setv 117; exists (fn x => (setv x; false)) slice05; !v = 117)
                       andalso (setv 0; exists (fn x => (addv x; false)) inp; !v = 7+9+13)
                       andalso (exists (fn x => (setv x; false)) inp; !v = 13));

val test12h = 
    tst' "test12h" (fn _ => 
                       not (all (fn i => i<13) inp)
                       andalso all (fn i => i<14) inp);

val test12i = 
    tst' "test12i" (fn _ => 
                       (setv 117; all (fn x => (setv x; true)) slice05; !v = 117)
                       andalso (setv 0; all (fn x => (addv x; true)) inp; !v = 7+9+13)
                       andalso (all (fn x => (setv x; true)) inp; !v = 13));


val test13 =
    tst' "test13" (fn _ =>
                      foldli consi [] inp = [(6,13),(5,9),(4,7)]
                      andalso foldri consi [] inp = [(4,7),(5,9),(6,13)]
                      andalso (setv 117; foldli setvif () inp; !v = 6+13)
                      andalso (setv 117; foldri setvif () inp; !v = 4+7));

val test14a =
    tst' "test14a" (fn _ =>
                       findi (fn _ => false) inp = NONE
                       andalso findi (fn (i,x) => x=9) inp = SOME (5,9)
                       andalso findi (fn (i,x) => i=6) inp = SOME (6,13));

val test14b =
    tst' "test14b" (fn _ =>        
                       List.all (fn sli => NONE=findi (fn (j, x) => j*10+1<>x) sli)
                                sliceas)

val test15 = 
    tst' "test15" (fn _ =>
                      ((setvi (0,0); appi addvi inp; !v = 4+7+5+9+6+13)
                       andalso (appi setvi inp; !v = 6+13)));

end

val test17a = 
    tst' "test17a" (fn _ =>
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

val test17b = 
    tst' "test17b" (fn _ =>
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

val _ = tstEnd()
end
