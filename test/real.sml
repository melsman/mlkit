(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p =
    let open Int
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds

(* test/real.sml -- PS 1995-03-24, 1996-05-16, 1996-07-02, 1996-09-25 *)

(*KILL 05/11/1997 11:01. tho.:
use "auxil.sml";
*)

val _ = print "Testing structure Real...\n"

local
    open Real
    infix ==
in

val test1 = tst "test1" (sign ~57.0 = ~1 andalso sign 99.0 = 1 andalso sign 0.0 = 0)
val test2 = tst "test2" (sameSign(~255.0, ~256.0) andalso sameSign(255.0, 256.0)
			 andalso sameSign(0.0, 0.0))
val test3 = tst "test3" (sign 1E~300 = 1 andalso sign ~1E~300 = ~1
			 andalso sign 1E300 = 1 andalso sign ~1E300 = ~1)

local
    val args = [0.0, 99.0, ~5.0, 1.1, ~1.1, 1.9, ~1.9, 2.5, ~2.5,
		1000001.4999, ~1000001.4999]
    val smallInt : int = Int.*(~1073741824, 10)   (* smaller than what fits in 32 bits *)
    val bigInt : int = Int.*(1073741823, 10)      (* larger than what fits in 32 bits *)
    val rsmallInt = real smallInt
    val rbigInt = real bigInt
    val minInt : int = valOf Int.minInt
    val maxInt : int = valOf Int.maxInt
    val rminInt = real minInt
    val rmaxInt = real maxInt

(*
    val _ = (print ("minInt = " ^ Int.toString minInt);
	     print ("\nmaxInt = " ^ Int.fmt StringCvt.DEC maxInt);
	     print ("\nrminInt = " ^ Real.toString rminInt);
	     print ("\nrmaxInt = " ^ Real.toString rmaxInt);
	     print ("\n"))
*)
    fun chksmallbig s f min max =
	tst' s (fn _ =>
		List.all (fn r => f r = smallInt) (rsmallInt :: min)
		andalso List.all (fn r => f r = bigInt) (rbigInt :: max));
    fun chkfail s f r =
	tst0 s ((f r; "WRONG")
			handle Overflow => "OK" | _ => "WRONG")

in
val test4a = tst "test4a" (map ceil args
			   = [0, 99, ~5, 2, ~1, 2, ~1, 3, ~2, 1000002, ~1000001])

val test4b = chksmallbig "test4b" ceil [rsmallInt-0.9] [rbigInt-0.1]
val test4c = map (chkfail "test4c" ceil) [rminInt-2000.0, rmaxInt+2000.0]

val test5a = check(map floor args
		   = [0, 99, ~5, 1, ~2, 1, ~2, 2, ~3, 1000001, ~1000002])

val test5b = chksmallbig "test5b" floor [rsmallInt+0.1] [rbigInt+0.9]
val test5c = map (chkfail "test5c" floor) [rminInt-2000.0, rmaxInt+2000.0]

val test6a = tst "test6a" (map trunc args
			   = [0, 99, ~5, 1, ~1, 1, ~1, 2, ~2, 1000001, ~1000001])

val test6b = chksmallbig "test6b" trunc [rsmallInt-0.9] [rbigInt+0.9];
val test6c = map (chkfail "test6c" trunc) [rminInt-1.0, rmaxInt+1.0];

val test7a = tst "test7a" (map round args
			   = [0, 99, ~5, 1, ~1, 2, ~2, 2, ~2, 1000001, ~1000001]);

val test7b = chksmallbig "test7b" round [rsmallInt-0.5] [rbigInt+0.4]
val test7c = map (chkfail "test7c" round) [rminInt-2000.0, rmaxInt+2000.0];

end

val test8 = tst "test8" (0.0 == real 0 andalso 2.0 == real 2 andalso ~2.0 == real ~2);

fun chk(s, r) =
    let val eps = abs r * 1E~10
    in
	tst' "chk" (fn _ =>
		    case fromString s of
		      SOME res => abs(res - r) <= eps
		    | NONE     => false)
    end

val test9a =
    List.map chk[("12.", 12.0),
		 ("12.E", 12.0),
		 ("12.E+", 12.0),
		 ("12.E-", 12.0),
		 ("12.E2", 12.0),
		 ("12.E+2", 12.0),
		 ("12.E-2", 12.0),
		 ("12E+", 12.0),
		 ("12E-", 12.0)];

val test9b =
    List.map chk[("0", 0.0),
		 ("156", 156.0),
		 ("+156", 156.0),
		 ("~156", ~156.0),
		 ("-156", ~156.0),
		 ("156.25", 156.25),
		 ("+156.25", 156.25),
		 ("~156.25", ~156.25),
		 ("-156.25", ~156.25),
		 (".25", 0.25),
		 ("+.25", 0.25),
		 ("~.25", ~0.25),
		 ("-.25", ~0.25)];

val test9c =
    List.map chk[ ("156E024", 156E024),
		  ("+156E024", 156E024),
		  ("~156E024", ~156E024),
		  ("-156E024", ~156E024),
		  ("156.25E024", 156.25E024),
		  ("+156.25E024", 156.25E024),
		  ("~156.25E024", ~156.25E024),
		  ("-156.25E024", ~156.25E024),
		  (".25E024", 0.25E024),
		  ("+.25E024", 0.25E024),
		  ("~.25E024", ~0.25E024),
		  ("-.25E024", ~0.25E024)];

val test9d =
    List.map chk[ ("156E+024", 156E024),
		  ("+156E+024", 156E024),
		  ("~156E+024", ~156E024),
		  ("-156E+024", ~156E024),
		  ("156.25E+024", 156.25E024),
		  ("+156.25E+024", 156.25E024),
		  ("~156.25E+024", ~156.25E024),
		  ("-156.25E+024", ~156.25E024),
		  (".25E+024", 0.25E024),
		  ("+.25E+024", 0.25E024),
		  ("~.25E+024", ~0.25E024),
		  ("-.25E+024", ~0.25E024)];

val test9e =
    List.map chk[ ("156E~024", 156E~024),
		  ("+156E~024", 156E~024),
		  ("~156E~024", ~156E~024),
		  ("-156E~024", ~156E~024),
		  ("156.25E~024", 156.25E~024),
		  ("+156.25E~024", 156.25E~024),
		  ("~156.25E~024", ~156.25E~024),
		  ("-156.25E~024", ~156.25E~024),
		  (".25E~024", 0.25E~024),
		  ("+.25E~024", 0.25E~024),
		  ("~.25E~024", ~0.25E~024),
		  ("-.25E~024", ~0.25E~024)];

val test9f =
    List.map chk[ ("156E-024", 156E~024),
		  ("+156E-024", 156E~024),
		  ("~156E-024", ~156E~024),
		  ("-156E-024", ~156E~024),
		  ("156.25E-024", 156.25E~024),
		  ("+156.25E-024", 156.25E~024),
		  ("~156.25E-024", ~156.25E~024),
		  ("-156.25E-024", ~156.25E~024),
		  (".25E-024", 0.25E~024),
		  ("+.25E-024", 0.25E~024),
		  ("~.25E-024", ~0.25E~024),
		  ("-.25E-024", ~0.25E~024)];

val test9g =
    List.map chk[ ("156e024", 156e024),
		  ("+156e024", 156e024),
		  ("~156e024", ~156e024),
		  ("-156e024", ~156e024),
		  ("156.25e024", 156.25e024),
		  ("+156.25e024", 156.25e024),
		  ("~156.25e024", ~156.25e024),
		  ("-156.25e024", ~156.25e024),
		  (".25e024", 0.25e024),
		  ("+.25e024", 0.25e024),
		  ("~.25e024", ~0.25e024),
		  ("-.25e024", ~0.25e024)];

val test9h =
    List.map chk[ ("156e+024", 156e024),
		  ("+156e+024", 156e024),
		  ("~156e+024", ~156e024),
		  ("-156e+024", ~156e024),
		  ("156.25e+024", 156.25e024),
		  ("+156.25e+024", 156.25e024),
		  ("~156.25e+024", ~156.25e024),
		  ("-156.25e+024", ~156.25e024),
		  (".25e+024", 0.25e024),
		  ("+.25e+024", 0.25e024),
		  ("~.25e+024", ~0.25e024),
		  ("-.25e+024", ~0.25e024)];

val test9i =
    List.map chk[ ("156e~024", 156e~024),
		  ("+156e~024", 156e~024),
		  ("~156e~024", ~156e~024),
		  ("-156e~024", ~156e~024),
		  ("156.25e~024", 156.25e~024),
		  ("+156.25e~024", 156.25e~024),
		  ("~156.25e~024", ~156.25e~024),
		  ("-156.25e~024", ~156.25e~024),
		  (".25e~024", 0.25e~024),
		  ("+.25e~024", 0.25e~024),
		  ("~.25e~024", ~0.25e~024),
		  ("-.25e~024", ~0.25e~024)];

val test9j =
    List.map chk[ ("156e-024", 156e~024),
		  ("+156e-024", 156e~024),
		  ("~156e-024", ~156e~024),
		  ("-156e-024", ~156e~024),
		  ("156.25e-024", 156.25e~024),
		  ("+156.25e-024", 156.25e~024),
		  ("~156.25e-024", ~156.25e~024),
		  ("-156.25e-024", ~156.25e~024),
		  (".25e-024", 0.25e~024),
		  ("+.25e-024", 0.25e~024),
		  ("~.25e-024", ~0.25e~024),
		  ("-.25e-024", ~0.25e~024)];

fun chk2 s = tst0 "chk2" (case fromString s of NONE => "OK" | _ => "WRONG")

val test10 =
    List.map chk2
             ["e10", "E10",
	      "+e10", "+E10",
	      "~e10", "~E10",
	      "-e10", "-E10"];

(* Note: There is some unclarity concerning rounding.  Should 1.45,
rounded to two significant digits, be "1.4" (nearest even digit) or
"1.5" (new greater digit) in case of a tie?  PS 1996-05-16 *)

val test11a =
    tst0 "test11a" ((fmt (StringCvt.FIX (SOME ~1)) 12.3456; "WRONG")
		    handle Size => "OK" | _ => "WRONG")

val test11b =
    tst0 "test11b" ((fmt (StringCvt.FIX (SOME 10000)) 12.3456; "OK")
		    handle Size => "WRONG" | _ => "EXN")

fun chkFIX (s,r, s0, s1, s2, s6) =
    tst ("chkFIX."^s)(fmt (StringCvt.FIX (SOME 0)) r = s0
		      andalso fmt (StringCvt.FIX (SOME 1)) r = s1
		      andalso fmt (StringCvt.FIX (SOME 2)) r = s2
		      andalso fmt (StringCvt.FIX (SOME 6)) r = s6
		      andalso fmt (StringCvt.FIX NONE) r = s6)

fun chkFIX' (s,r, s0, s1, s2, s6) =
    (chkFIX(s,r, s0, s1, s2, s6);
     if r == 0.0 then ()
     else chkFIX(s^"~",~r, "~"^s0, "~"^s1, "~"^s2, "~"^s6))

val test11c =
	   List.app chkFIX'
	   [("a",0.0, "0", "0.0", "0.00", "0.000000"),
	    ("b",1.0, "1", "1.0", "1.00", "1.000000"),
	    ("c",1.4, "1", "1.4", "1.40", "1.400000"),
	    ("d",1.5, "2", "1.5", "1.50", "1.500000"),
(* dubious  ("e",2.5, "2", "2.5", "2.50", "2.500000"), *)
	    ("f",1.6, "2", "1.6", "1.60", "1.600000"),
    	    ("g",1.45, "1", "1.4", "1.45", "1.450000"),
	    ("h",3.141592653589, "3", "3.1", "3.14", "3.141593"),
	    ("j",91827364509182.0, "91827364509182", "91827364509182.0",
	     "91827364509182.00", "91827364509182.000000")]

local val r = 91827364509182.0
      val s0 = "91827364509182"
      val s1 = "91827364509182.0"
      val s2 = "91827364509182.00"
      val s6 = "91827364509182.000000"
in
(*  fun pr s = (print (" " ^ s ^ "\n"); s)
  val fmt = fn a => pr o (fmt a) *)
  val test11d = tst "test11d" (fmt (StringCvt.FIX (SOME 0)) r = s0)
  val test11e = tst "test11e" (fmt (StringCvt.FIX (SOME 1)) r = s1)
  val test11f = tst "test11f" (fmt (StringCvt.FIX (SOME 2)) r = s2)
  val test11g = tst "test11g" (fmt (StringCvt.FIX (SOME 6)) r = s6)
  val test11h = tst "test11h" (fmt (StringCvt.FIX NONE) r = s6)
end

val test12a =
    tst0 "test12a" ((fmt (StringCvt.SCI (SOME ~1)) 12.3456; "WRONG")
		    handle Size => "OK" | _ => "WRONG")

val test12b =
    tst0 "test12b" ((fmt (StringCvt.SCI (SOME 10000)) 12.3456; "OK")
		    handle Size => "WRONG" | _ => "EXN")

fun chkSCI (r, s0, s1, s2, s6) =
    fmt (StringCvt.SCI (SOME 0)) r = s0
    andalso fmt (StringCvt.SCI (SOME 1)) r = s1
    andalso fmt (StringCvt.SCI (SOME 2)) r = s2
    andalso fmt (StringCvt.SCI (SOME 6)) r = s6
    andalso fmt (StringCvt.SCI NONE) r = s6;

fun chkSCI' (r, s0, s1, s2, s6) =
    chkSCI(r, s0, s1, s2, s6)
    andalso (r == 0.0 orelse chkSCI(~r, "~"^s0, "~"^s1, "~"^s2, "~"^s6));

val test12c =
    tst' "test12c" (fn _ =>
	   List.all chkSCI'
	   [(0.0, "0E00", "0.0E00", "0.00E00", "0.000000E00"),
	    (0.0012345678, "1E~03", "1.2E~03", "1.23E~03", "1.234568E~03"),
	    (1.0, "1E00", "1.0E00", "1.00E00", "1.000000E00"),
	    (1.4, "1E00", "1.4E00", "1.40E00", "1.400000E00"),
	    (1.5, "2E00", "1.5E00", "1.50E00", "1.500000E00"),
(* dubious  (2.5, "2E00", "2.5E00", "2.50E00", "2.500000E00"), *)
	    (1.6, "2E00", "1.6E00", "1.60E00", "1.600000E00"),
    	    (1.45, "1E00", "1.4E00", "1.45E00", "1.450000E00"),
	    (3.141592653589, "3E00", "3.1E00", "3.14E00", "3.141593E00"),
	    (91827364509182.0, "9E13", "9.2E13", "9.18E13", "9.182736E13")]);

val test13a =
    tst0 "test13a" ((fmt (StringCvt.GEN (SOME 0)) 12.3456; "WRONG")
		    handle Size => "OK" | _ => "WRONG")

val test13b =
    tst0 "test13b" ((fmt (StringCvt.GEN (SOME 100000)) 12.3456)
		    handle Size => "OK" | _ => "WRONG")

fun chkGEN (r, s1, s2, s6, s12) =
    fmt (StringCvt.GEN (SOME 1)) r = s1
    andalso fmt (StringCvt.GEN (SOME 2)) r = s2
    andalso fmt (StringCvt.GEN (SOME 6)) r = s6
    andalso fmt (StringCvt.GEN (SOME 12)) r = s12
    andalso fmt (StringCvt.GEN NONE) r = s12
    andalso toString r = s12;

fun chkGEN' (r, s1, s2, s6, s12) =
    chkGEN(r, s1, s2, s6, s12)
    andalso (r == 0.0 orelse
	     chkGEN(~r, "~"^s1, "~"^s2, "~"^s6, "~"^s12));

val test13c =
    tst' "test13c" (fn _ =>
	   List.all chkGEN'
	   [(0.0,               "0.0", "0.0",     "0.0", "0.0"),
	    (0.0012345678,    "0.001", "0.0012", "0.00123457",
	     "0.0012345678"),
	    (1.0,              "1.0", "1.0",  "1.0", "1.0"),
	    (1.4,              "1.0", "1.4",  "1.4", "1.4"),
	    (1.5,              "2.0", "1.5",  "1.5", "1.5"),
(* dubious  (2.5,              "2.0", "2.5",  "2.5", "2.5"), *)
	    (1.6,              "2.0", "1.6",  "1.6", "1.6"),
    	    (1.45,             "1.0", "1.4",  "1.45", "1.45"),
	    (3.141592653589,   "3.0", "3.1",  "3.14159", "3.14159265359"),
	    (91827364509182.0, "9E13", "9.2E13",  "9.18274E13", "9.18273645092E13")])

val nan = Math.sqrt ~1.0

val test14 =
    tst "test14" (radix = 2 andalso precision = 53)

val test15 =
    tst "test15" (maxFinite > 100000.0 andalso maxFinite < posInf)

val test16 =
    tst "test16" (minPos > 0.0 andalso minPos < 0.5E~322)

val test17 =
    tst "test17" (minNormalPos > 0.0 andalso minNormalPos < 0.5E~300)

val test18 =
    tst "test18" (toString posInf = "inf")

val test19 =
    tst "test19" (toString negInf = "~inf")

val test20 =
    tst "test20" (toString nan = "nan")

val test21 =
    tst "test21" ( *+(34.0,~23.4,12.2) == 34.0 * ~23.4 + 12.2 )

val test22 =
    tst "test22" ( *-(34.0,~23.4,12.2) == 34.0 * ~23.4 - 12.2 )

val test23 =
    tst "test23" (not(signBit posInf) andalso signBit negInf andalso signBit ~2.1 andalso not (signBit 2.0))

val test24 =
    tst "test24" (copySign(1.2,~300.0) == ~1.2 andalso copySign(~1.2,300.0) == 1.2)

val test25 =
    tst "test25" (compareReal (nan,2.0) = IEEEReal.UNORDERED andalso
                  compareReal (~2.0,nan) = IEEEReal.UNORDERED andalso
                  compareReal (nan,nan) = IEEEReal.UNORDERED andalso
                  compareReal (2.0,posInf) = IEEEReal.LESS)

val test26a =
    tst "test26a" ((compare(nan,2.0); false) handle IEEEReal.Unordered => true)

val test26b =
    tst "test26b" ((compare(nan,nan); false) handle IEEEReal.Unordered => true)

val test26c =
    tst "test26c" ((compare(~2.0,nan); false) handle IEEEReal.Unordered => true)

val test27 =
    tst "test27" (unordered(nan,2.0))

val test28 =
    tst "test28" (isNan nan andalso not(isNan posInf) andalso not(isNan 2.0))

val test29 =
    tst "test29" (isFinite 2.0 andalso not (isFinite posInf) andalso not (isFinite negInf))

fun isoManExp r = fromManExp(toManExp r)

val test30 =
    tst "test30" (2.3 == isoManExp 2.3 andalso posInf == isoManExp posInf andalso isNan(isoManExp nan))

fun whole r = #whole(split r)

val test31 =
    tst "test31" (whole 2.3 == 2.0 andalso whole posInf == posInf andalso isNan(whole nan) andalso whole ~2.4 == ~2.0)

val test32 =
    tst "test32" (realMod 2.5 == 0.5)

val test33 =
    tst "test33" (realMod posInf == 0.0)

val test34 =
    tst "test34" (isNan(realMod nan))

val test35 =
    tst "test35" (nextAfter (2.0, 3.0) > 2.0 andalso nextAfter (2.0, 3.0) < 2.0001)

val test36 =
    tst "test36" (nextAfter (2.0, ~3.0) < 2.0 andalso nextAfter (2.0, ~3.0) > 1.9999)

val test37 =
    tst "test37" (nextAfter (2.0, 2.0) == 2.0)

val test38 =
    tst "test38" (isNan(nextAfter (nan, 2.0)) andalso isNan(nextAfter (2.0,nan)))

val test39 =
    tst "test39" (nextAfter (negInf, 0.0) > negInf andalso nextAfter (posInf, 0.0) < posInf)

val test40 =
    tst "test40" ((checkFloat nan; false) handle General.Div => true)

val test41 =
    tst "test41" ((checkFloat posInf; false) handle General.Overflow => true)

val test42 =
    tst "test42" ((checkFloat 3.4; true) handle _ => false)

fun pr r = Real.fmt (StringCvt.FIX(SOME 0)) r

fun try t s expected =
    let val res = pr (fromLargeInt (valOf (LargeInt.fromString s)))
    in tst t (res = expected)
    end

val test43 = try "test43" "123456789123456789123456789" "123456789123456791337762816"

fun try2 t rm r expected =
    let val res = IntInf.toString (toLargeInt rm r)
    in tst t (res = expected)
    end

val test44 =
    try2 "test44" IEEEReal.TO_POSINF 123456789123456789123456789123456789123456789123456789.0E200
         "12345678912345679076198071096244527781560870137019946199603053023437034345861166751654846484345015072896161860739481140984032537425287193418804458844483168788367356521095598566212295492892610317287470596434332346080596003132423633511547282430231173922816"

val test45 =
    try2 "test45" IEEEReal.TO_ZERO 12345678.6
         "12345678"

fun withRM rm f =
    let val rm' = IEEEReal.getRoundingMode()
        val () = IEEEReal.setRoundingMode rm
        val res = f()
    in IEEEReal.setRoundingMode rm'
     ; res
    end

val test46a =
    tst "test46a" (toInt IEEEReal.TO_NEAREST 2.8 = 3)

val test46b =
    tst "test46b" (toInt IEEEReal.TO_ZERO 2.8 = 2)

end
