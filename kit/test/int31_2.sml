(* This test works only for 31-bit implementations! *)

local open Int31
in
val real = fn i => real (toInt i)
val floor = fn r => fromInt(floor r)
val maxint : int31 = 1073741823
val minint = ~maxint -1
infix seq
fun e1 seq e2 = e2;
fun test t s = print (t ^ ": " ^ s ^ "\n")
fun check true = "OK"
  | check false = "ERR"
fun test' t b = test t (check b)
fun test'' t f = test t ((check (f())) handle _ => "EXN")

val test1 = test "test1" ((~minint seq "WRONG") handle Overflow => "OK")

val test2 = test "test2" ((abs minint seq "WRONG") handle Overflow => "OK")
val test3 = test "test3" ((maxint+1   seq "WRONG") handle Overflow => "OK")
val test4 = test "test4" ((minint-1   seq "WRONG") handle Overflow => "OK")

val test5 = test "test5" (if maxint =  0x3fffffff then "OK" else "WRONG")
val test6 = test "test6" (if maxint =  0x3FFFFFFF then "OK" else "WRONG")
val test7 = test "test7" (if minint = ~0x40000000 then "OK" else "WRONG")

val sum = (op+) : int * int -> int
val diff = (op-) : int * int -> int

val test8 = test "test8" ((sum (maxint,1)  seq "WRONG") handle Overflow => "OK")
val test9 = test "test9" ((diff (minint,1) seq "WRONG") handle Overflow => "OK")

val test10 = test "test10" ((minint * ~1 seq  "WRONG") handle Overflow => "OK")

val prod = (op * ) : int * int -> int

val test11 = test "test11" ((prod (minint,~1) seq "WRONG") handle Overflow => "OK")

fun checkDivMod i d =
  let val q = i div d
      val r = i mod d
  in 
(*      printVal i seq TextIO.output(TextIO.stdOut, " "); 
      printVal d seq TextIO.output(TextIO.stdOut, "   "); *)
      if (d * q + r = i) andalso
	  ((0 <= r andalso r < d) orelse (d < r andalso r <= 0))
      then "OK" else "WRONG: problems with div, mod"
  end;

val test12 = test "test12" (checkDivMod 23 10)
val test13 = test "test13" (checkDivMod ~23 10)
val test14 = test "test14" (checkDivMod 23 ~10)
val test15 = test "test15" (checkDivMod ~23 ~10)

val test16 = test "test16" (checkDivMod 100 10)
val test17 = test "test17" (checkDivMod ~100 10)
val test18 = test "test18" (checkDivMod 100 ~10)
val test19 = test "test19" (checkDivMod ~100 ~10)

val test20 = test "test20" (checkDivMod 100 1)
val test21 = test "test21" (checkDivMod 100 ~1)
val test22 = test "test22" (checkDivMod 0 1)
val test23 = test "test23" (checkDivMod 0 ~1)

val test24 = test "test24" ((100 div 0     seq  "WRONG") handle Div => "OK")
val test25 = test "test25" ((100 mod 0     seq  "WRONG") handle Div => "OK")
val test26 = test "test26" ((minint div ~1 seq  "WRONG") handle Overflow => "OK")

val maxri = real maxint
val minri = real minint

val test27 = test "test27" (if floor 3.0 = 3 then "OK" else "WRONG")
val test28 = test "test28" (if floor 3.14 = 3 then "OK" else "WRONG")
val test29 = test "test29" (if floor ~3.0 = ~3 then "OK" else "WRONG")
val test30 = test "test30" (if floor ~3.14 = ~4 then "OK" else "WRONG")
val test31 = test "test31" (if floor(Real.+(maxri, 0.9)) = maxint then "OK" else "WRONG")
val test32 = test "test32" (if floor minri = minint then "OK" else "WRONG")
val test33 = test "test33" ((floor (Real.-(minri, 0.1)) seq  "WRONG") handle Overflow => "OK")
val test34 = test "test34" ((floor (Real.+(maxri, 1.0)) seq  "WRONG") handle Overflow => "OK")

val test35 = test' "test35" (toLarge ~1 = ~1)
val test36 = test' "test36" (toLarge 1 = 1)
val test37 = test' "test37" (toLarge 0 = 0)
val test38 = test' "test38" (toLarge maxint = 1073741823)
val test39 = test' "test39" (toLarge minint = ~1073741824)

val test40 = test'' "test40" (fn _ => fromLarge(toLarge ~1) = ~1)
val test41 = test'' "test41" (fn _ => fromLarge(toLarge maxint) = maxint)
val test42 = test'' "test42" (fn _ => fromLarge(toLarge 0) = 0)
val test42 = test'' "test42" (fn _ => fromLarge(toLarge minint) = minint)

val test43 = test "test43" ((fromLarge(Int32.toLarge(Int32.+(Int32.fromLarge(toLarge maxint), 1))) seq "WRONG") handle Overflow => "OK")
val test44 = test "test44" ((fromLarge(Int32.toLarge(Int32.-(Int32.fromLarge(toLarge minint), 1))) seq "WRONG") handle Overflow => "OK")
val test45 = test "test45" ((fromLarge(Int32.toLarge(valOf Int32.maxInt)) seq "WRONG") handle Overflow => "OK")
val test46 = test "test46" ((fromLarge(Int32.toLarge(valOf Int32.minInt)) seq "WRONG") handle Overflow => "OK")

val test47a = test'' "test47a" (fn _ => valOf (fromString "1") * valOf minInt = valOf minInt)
val test47b = test'' "test47b" (fn _ => valOf minInt * valOf (fromString "1") = valOf minInt)
val test48 = test'' "test48" (fn _ => 1073741820 + 3 = valOf maxInt)
val test49 = test'' "test49" (fn _ => valOf maxInt - 3 = 1073741820)

val test50 = test'' "test50" (fn _ => 1073741820 - ~3 = valOf maxInt)
val test51 = test'' "test51" (fn _ => valOf maxInt + ~3 = 1073741820)

val test52 = test'' "test52" (fn _ => ~1073741821 - 3  = valOf minInt)
val test53 = test'' "test53" (fn _ => valOf minInt + 3 = ~1073741821)

end