(* File vector.sml: Testing Vector operations.
 * Copyright (c) 1994-2014, Peter Sestoft, Martin Elsman.
 * MIT License.
 *)

open Utest
val _ = tstStart "structure Vector"

local
    open Vector
    infix 9 sub
in

val a = fromList [0,1,2,3,4,5,6]
val b = fromList [44,55,66]
val c = fromList [0,1,2,3,4,5,6]

val test1 = tst' "test1" (fn _ => a<>b)
val test2 = tst' "test2" (fn _ => a=c)
val d = tabulate(100, fn i => i mod 7)

val test3 = tst' "test3" (fn _ => d sub 27 = 6)
val test4a = 
    tst' "test4a" (fn () => (tabulate(maxLen+1, fn i => i); false)
                            handle Size => true)
val test4b = 
    tst' "test4b" (fn () => (tabulate(~1, fn i => i); false)
                            handle Size => true)
val test4c =
    tst' "test4c" (fn _ => length (tabulate(0, fn i => i div 0)) = 0)

val test5 = tst' "test5" (fn _ => length (fromList []) = 0 andalso length a = 7)

val test6a = 
    tst' "test6a" (fn () => (c sub ~1; false) handle Subscript => true)

val test6b =
    tst' "test6b" (fn () => (c sub 7; false) handle Subscript => true)

val test6c =
    tst' "test6c" (fn _ => c sub 0 = 0)

val e = concat [d, b, d]

val test7 = tst' "test7" (fn _ => length e = 203)

val test8 = tst' "test8" (fn _ => length (concat []) = 0)

fun chkiter s iter f vec reslast =
    tst' s (fn _ =>
           let val last = ref ~1
               val res = iter (fn x => (last := x; f x)) vec
           in (res, !last) = reslast end)

fun chkiteri s iter f vec reslast =
    tst' s (fn _ =>
           let val last = ref ~1
               val res = iter (fn (i, x) => (last := i; f x)) vec
           in (res, !last) = reslast end)

val test10a = 
    chkiter "test10a" map (fn x => 2*x) b (fromList [88,110,132], 66)

val test11a = 
    chkiteri "test11a" mapi (fn x => 2*x) (b) (fromList [88,110,132], 2)
end
val _ = tstEnd()
