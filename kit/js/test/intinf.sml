(* file "test/largeint.sml" PS 1995-09-05, 1998-04-12

   - modified for the MLKit; mael 2005-12-13 *)

fun ptest t s = print(t ^ ": " ^ s ^ "<br>")
fun ptestl t nil = ()
  | ptestl t (s::ss) = (ptest t s; ptestl t ss)
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

val _ = print "<h2>File intinf.sml: Testing structure IntInf...</h2>";

local 
    open IntInf
    fun divmod1 (i, d, q, r)  = 
	check'(fn () => (toInt (fromInt i div fromInt d) = q 
			 andalso toInt(fromInt i mod fromInt d) = r));
    fun quotrem1 (i, d, q, r) = 
	check'(fn () => (toInt (quot(fromInt i, fromInt d)) = q 
			 andalso toInt (rem(fromInt i, fromInt d)) = r));

    fun divmod2 (i, d, q, r)  = 
	check'(fn () => let val (q', r') = divMod(fromInt i, fromInt d) 
			in toInt q' = q andalso toInt r' = r end);
    fun quotrem2 (i, d, q, r) = 
	check'(fn () => let val (q', r') = quotRem(fromInt i, fromInt d) 
			in toInt q' = q andalso toInt r' = r end);

    fun add1(x, y, sum) = 
	check'(fn () => (toInt (fromInt x + fromInt y) = sum));

    fun sub1(x, y, diff) = 
	check'(fn () => (toInt (fromInt x - fromInt y) = diff));

    fun mul1(x, y, prod) = 
	check'(fn () => (toInt (fromInt x * fromInt y) = prod));
	
in	

val test1a = divmod1(10, 3, 3, 1);
val _ = ptest "test1a" test1a
val test1b = divmod1(~10, 3, ~4, 2);
val _ = ptest "test1b" test1b
val test1c = divmod1(~10, ~3, 3, ~1);
val _ = ptest "test1c" test1c
val test1d = divmod1(10, ~3, ~4, ~2);
val _ = ptest "test1d" test1d

val test2a = quotrem1(10, 3, 3, 1);
val _ = ptest "test2a" test2a
val test2b = quotrem1(~10, 3, ~3, ~1);
val _ = ptest "test2b" test2b
val test2c = quotrem1(~10, ~3, 3, ~1);
val _ = ptest "test2c" test2c
val test2d = quotrem1(10, ~3, ~3, 1);
val _ = ptest "test2d" test2d

val test3a = divmod2(10, 3, 3, 1);
val _ = ptest "test3a" test3a
val test3b = divmod2(~10, 3, ~4, 2);
val _ = ptest "test3b" test3b
val test3c = divmod2(~10, ~3, 3, ~1);
val _ = ptest "test3c" test3c
val test3d = divmod2(10, ~3, ~4, ~2);
val _ = ptest "test3d" test3d

val test4a = quotrem2(10, 3, 3, 1);
val _ = ptest "test4a" test4a
val test4b = quotrem2(~10, 3, ~3, ~1);
val _ = ptest "test4b" test4b
val test4c = quotrem2(~10, ~3, 3, ~1);
val _ = ptest "test4c" test4c
val test4d = quotrem2(10, ~3, ~3, 1);
val _ = ptest "test4d" test4d

val test5a = ((fromInt 1 div fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val _ = ptest "test5a" test5a
val test5b = ((fromInt 1 mod fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val _ = ptest "test5b" test5b
val test5c = (quot(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val _ = ptest "test5c" test5c
val test5d = (rem(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val _ = ptest "test5d" test5d
val test5e = (divMod(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val _ = ptest "test5e" test5e
val test5f = (quotRem(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val _ = ptest "test5f" test5f

val test6a = 
    List.map add1 [(12,17,29), (~12,17,5), (12,~17,~5), (~12,~17,~29)];
val _ = ptestl "test6a" test6a
val test6b = 
    List.map sub1 [(12,17,~5), (~12,17,~29), (12,~17,29), (~12,~17,5)];
val _ = ptestl "test6b" test6b
val test6c = 
    List.map mul1 [(12,17,204), (~12,17,~204), (12,~17,~204), (~12,~17,204)];
val _ = ptestl "test6c" test6c

fun chkToString (i, s) = check'(fn _ => toString(fromInt i) = s);

val test12a = 
    List.map chkToString [(0, "0"), 
			  (~1, "~1"), 
			  (12345678, "12345678"),
			  (~12345678, "~12345678")];
val _ = ptestl "test12a" test12a

fun chk f (s, r) = 
    check'(fn _ => 
	   case f s of
	       SOME res => toInt res = r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a = 
    List.map (chk fromString)
             [("10789", 10789),
	      ("+10789", 10789),
	      ("~10789", ~10789),
	      ("-10789", ~10789),
	      (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789),
	      (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789)];
val _ = ptestl "test13a" test13a

val test13b = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "ff"];	    
val _ = ptestl "test13b" test13b

val test14a = 
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
	      ("+10789", 10789),
	      ("~10789", ~10789),
	      ("-10789", ~10789),
	      (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789),
	      (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789)];
val _ = ptestl "test14a" test14a

val test14b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "ff"];	    
val _ = ptestl "test14b" test14b

val test15a = 
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      ("+10010", 18),
	      ("~10010", ~18),
	      ("-10010", ~18),
	      (" \n\t10010crap", 18),
	      (" \n\t+10010crap", 18),
	      (" \n\t~10010crap", ~18),
	      (" \n\t-10010crap", ~18)];
val _ = ptestl "test15a" test15a

val test15b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "2", "8", "ff"];
val _ = ptestl "test15b" test15b

val test16a = 
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
	      ("+2071", 1081),
	      ("~2071", ~1081),
	      ("-2071", ~1081),
	      (" \n\t2071crap", 1081),
	      (" \n\t+2071crap", 1081),
	      (" \n\t~2071crap", ~1081),
	      (" \n\t-2071crap", ~1081)];
val _ = ptestl "test16a" test16a

val test16b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "8", "ff"];
val _ = ptestl "test16b" test16b

val test17a = 
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367),
	      ("+20Af", 8367),
	      ("~20Af", ~8367),
	      ("-20Af", ~8367),
	      (" \n\t20AfGrap", 8367),
	      (" \n\t+20AfGrap", 8367),
	      (" \n\t~20AfGrap", ~8367),
	      (" \n\t-20AfGrap", ~8367)];
val _ = ptestl "test17a" test17a

val test17b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1"];
val _ = ptestl "test17b" test17b

val test18 =
    check'(fn _ => 
	   toInt(pow(fromInt 12, 3)) = 1728
	   andalso toInt(pow(fromInt  0 ,  1)) = 0
	   andalso toInt(pow(fromInt  1 ,  0)) = 1
	   andalso toInt(pow(fromInt  0 ,  0)) = 1
	   andalso toInt(pow(fromInt  1 , ~1)) = 1
	   andalso toInt(pow(fromInt ~1 , ~1)) = ~1
	   andalso toInt(pow(fromInt  2 , ~1)) = 0
	   andalso toInt(pow(fromInt ~2 , ~1)) = 0)
val _ = ptest "test18" test18

fun testbin cvt opr a1 a2 r =
    check(
	   case (StringCvt.scanString (scan cvt) a1, StringCvt.scanString (scan cvt) a2) of 
	       (SOME i1,SOME i2) =>
		   let val i = opr(i1,i2) handle X => (print "Upps\n"; raise X)
		       val s = fmt cvt i
		       (* val _ = print (s ^ "\n")  *)
		   in r = s
		   end
	     | _ => false)

val test19a = testbin StringCvt.BIN (op orb) "01" "10" "11"
val _ = ptest "test19a" test19a

val test19b = testbin StringCvt.BIN (op orb) "001001001001001001001001001001" "100100100100100100100100100100" "101101101101101101101101101101"
val _ = ptest "test19b" test19b

val test19c = testbin StringCvt.HEX (op orb) "ffffffffffff" "0" "ffffffffffff"
val _ = ptest "test19c" test19c

val test19d = testbin StringCvt.HEX (op orb) "afffffaffafa" "ffafffaffffa" "ffffffaffffa"
val _ = ptest "test19d" test19d

val test20a = testbin StringCvt.BIN (op andb) "101" "110" "100"
val _ = ptest "test20a" test20a

val test20b = testbin StringCvt.BIN (op andb) "1001001001001001001001001001001" "1100100100100100100100100100100" "1000000000000000000000000000000"
val _ = ptest "test20b" test20b

val test20c = testbin StringCvt.HEX (op andb) "ffffffffffff" "ffffffffffff" "ffffffffffff"
val _ = ptest "test20c" test20c

val test20d = testbin StringCvt.HEX (op andb) "ffffffffffff" "ffaffffffffa" "ffaffffffffa"
val _ = ptest "test20d" test20d

end

(* overloading resolution *)

val test21a = check ( (4:intinf) div 2 = 2 )
val _ = ptest "test21a" test21a

val test21b = check ( (4:intinf) mod 2 = 0 )
val _ = ptest "test21b" test21b

val _ = print "Test ended."
