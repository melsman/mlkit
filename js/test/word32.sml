(* Auxiliary functions for test cases *)

fun pp_intinf (_IntInf{negative,digits}) =
    let val s = if negative then "~" else ""
    in s ^ String.concatWith "," (map Int31.toString digits)
    end

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int32 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "<br>");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds
fun tstrange' s bounds = (tst' s) o (fn b => fn () => range bounds b)  

(* test/word.sml -- some test cases for Word32, appropriate for a two's
   complement representation with Word.wordSize = 32
   PS 1995-03-19, 1995-07-12, 1995-11-06, 1996-04-01, 1996-10-01 
   ME 2001-04-26
*)

val _ = print "<h2>File word32.sml: Testing structure Word32...</h2>"

local 
  local open Int32
  in
    (* Isn't this disgusting: *)
    val [gt,  lt,  ge,   le] = 
	[op>, op<, op>=, op<=] : (int * int -> bool) list
    val [add, sub, mul, idiv,   imod] = 
	[op+, op-, op*, op div, op mod] : (int * int -> int) list
  end

  open Word32
  val op > = gt and op < = lt and op >= = ge and op <= = le;
  val op + = add and op - = sub and op * = mul 
  and op div = idiv and op mod = imod;
  fun i2w a0 = 
      let val a = Int32.toLarge a0 handle ? => (print ("Error i2w: " ^ exnName ? ^ "<br>"); raise ?) 
(*         val _ = print ("Int32.toLarge(" ^ Int32.toString a0 ^ ") = " ^ pp_intinf a ^ "<br>")*)
          val b = fromLargeInt a (* <-----err------- *)
(*          val _ = print ("Word32.fromLargeInt(" ^ pp_intinf a ^ ") = " ^ Word32.toString b ^ "<br>") *)

      in b
      end
  fun w2i a0 = 
      let val a = toLargeIntX a0 handle ? => (print ("Error w2i(1): " ^ exnName ? ^ "<br>"); raise ?) 
          val a = Int32.fromLarge a handle ? => (print ("Error w2i(2): " ^ exnName ? ^ "; a0 = " ^ toString a0 ^ "<br>"); raise ?) 
      in a
      end

  fun pr_ln s s' = print (s ^ ": " ^ s' ^ "<br>")
in

local
val iL1 = Int32.toLarge(valOf Int32.maxInt)
val iL2 = Int32.toLarge(valOf Int32.minInt)
val iL3 = Int.toLarge(valOf Int.maxInt)
val iL4 = Int.toLarge(valOf Int.minInt)
val iL5 = Int31.toLarge(valOf Int31.maxInt)
val iL6 = Int31.toLarge(valOf Int31.minInt)
in
val test0a = tst' "test0a" (fn() => toLargeInt(fromLargeInt iL1) = iL1)
val test0b = tst' "test0b" (fn() => toLargeIntX(fromLargeInt iL2) = iL2)
val test0c = tst' "test0c" (fn() => toLargeInt(fromLargeInt iL3) = iL3)
val test0d = tst' "test0d" (fn() => toLargeIntX(fromLargeInt iL4) = iL4)
val test0e = tst' "test0e" (fn() => toLargeInt(fromLargeInt iL5) = iL5)
val test0f = tst' "test0f" (fn() => toLargeIntX(fromLargeInt iL6) = iL6)
val test0g = tstrange "test0g" (~100,100) (fn i => let val i = Int.toLarge i
                                                   in toLargeIntX(fromLargeInt i) = i
                                                   end)
val test0h = tst' "test0h" (fn() => Int32.fromLarge(toLargeIntX 0wxFFFFFFFF) = ~1)
val test0i = tst' "test0i" (fn() => Int32.fromLarge(toLargeIntX 0wx1) = 1)
val test0j = tst' "test0j" (fn() => fromLargeInt(toLargeIntX 0wxFFFFFFFF) = 0wxFFFFFFFF)
val test0k = tst' "test0k" (fn() => toLargeIntX 0wxFFFFFFFF = ~1)
end

val test1 = checkrange (0, 125)
    (fn i => i = w2i (i2w i));
val _ = pr_ln "test1" test1

val test3 = tstrange' "test3" (~100, 100) 
    (fn i => i = Int32.fromLarge(toLargeIntX (i2w i)));

val test5a = checkrange (0,15) 
    (fn i => (i+960) div 2 * 2 + 1
             = w2i (orb (i2w i, i2w 961)));
val _ = pr_ln "test5a" test5a
val test5b = checkrange (0,113)
    (fn i => i = w2i (orb (i2w i, i2w i)));
val _ = pr_ln "test5b" test5b
val test6a = checkrange (0,15) 
    (fn i => i div 2 * 2 = w2i (andb (i2w i, i2w ~2)));
val _ = pr_ln "test6a" test6a
val test6b = checkrange (0,113)
    (fn i => i = w2i (andb (i2w i, i2w i)));
val _ = pr_ln "test6b" test6b
val test7a = checkrange (0,15) 
    (fn i => i+960 = w2i (xorb (i2w i, i2w 960)));
val _ = pr_ln "test7a" test7a
val test7b = checkrange (0, 113)
    (fn i => 0 = w2i (xorb (i2w i, i2w i)));
val _ = pr_ln "test7b" test7b
val test8a = check (~1 = w2i (notb (i2w 0)));
val _ = pr_ln "test8a" test8a
val test8b = check (0 = w2i (notb (i2w ~1)));
val _ = pr_ln "test8b" test8b
val maxposint = case Int32.maxInt
		  of SOME m => m
		   | NONE => raise Fail "ERROR"
val maxnegint = case Int32.minInt
		  of SOME m => m
		   | NONE => raise Fail "ERROR"
fun pwr2 0 = 1 
  | pwr2 n = 2 * pwr2 (n-1);
fun rwp i 0 = i
  | rwp i n = rwp i (n-1) div 2;

val test9a = checkrange (0,1)
    (fn k => pwr2 k = w2i (<< (i2w 1, Word.fromLargeInt (Int32.toLarge k))));
val _ = pr_ln "test9a" test9a
val test9b = checkrange (32,65)
    (fn k => 0 = w2i (<< (i2w 1, Word.fromLargeInt (Int32.toLarge k))));
val _ = pr_ln "test9b" test9b
val test9c = check (maxnegint = w2i (<< (i2w 1, Word.fromInt (Int.-(wordSize,1)))));
val _ = pr_ln "test9c" test9c
val test9d = checkrange (0, 125)
    (fn i => 2 * i = w2i (<< (i2w i, Word.fromLargeInt (Int32.toLarge 1))));
val _ = pr_ln "test9d" test9d
val test9e = checkrange (0, 125)
    (fn i => i div 2 = w2i (>> (i2w i, Word.fromLargeInt (Int32.toLarge 1))));
val _ = pr_ln "test9e" test9e
val test9f = checkrange (0,65)
    (fn k => rwp maxposint k = w2i (>> (i2w maxposint, Word.fromLargeInt (Int32.toLarge k))));
val _ = pr_ln "test9f" test9f
val test9g = checkrange (32,65)
    (fn k => 0 = w2i (<< (i2w ~1, Word.fromLargeInt (Int32.toLarge k))));
val _ = pr_ln "test9g" test9g
val test9h = checkrange (1,65)
    (fn k => 0 = w2i (>> (i2w 1, Word.fromLargeInt (Int32.toLarge k))));
val _ = pr_ln "test9h" test9h

val test10a = checkrange (1,65)
    (fn k => 0 = w2i (~>> (i2w 1, Word.fromLargeInt (Int32.toLarge k))));
val _ = pr_ln "test10a" test10a
val test10b = tstrange' "test10b" (1,65)
    (fn k => ~1 = w2i (~>> (i2w ~1, Word.fromLargeInt (Int32.toLarge k))));

val test10c = checkrange (~113, 113)
    (fn i => i div 2 = Int32.fromLarge(toLargeIntX (~>> (i2w i, Word.fromLargeInt (Int32.toLarge 1)))));
val _ = pr_ln "test10c" test10c
val test10d = tstrange' "test10d" (0,65)
    (fn k => rwp maxnegint k = Int32.fromLarge(toLargeIntX (~>> (i2w maxnegint, Word.fromLargeInt (Int32.toLarge k)))));

local 
    open Word32
in
val test11a = check (i2w 256 > i2w 255);
val _ = pr_ln "test11a" test11a
val test11b = check (i2w 0 < i2w ~1);
val _ = pr_ln "test11b" test11b
val test11c = check (i2w maxposint >= i2w maxposint);
val _ = pr_ln "test11c" test11c
val test11d = check (i2w maxnegint >= i2w 127);
val _ = pr_ln "test11d" test11d
val test11e = check (i2w 1 <= i2w 1);
val _ = pr_ln "test11e" test11e
val test11f = check (i2w 0 <= i2w 1);
val _ = pr_ln "test11f" test11f
val test11g = check (i2w 0 < i2w maxposint);
val _ = pr_ln "test11g" test11g
val test11h = check (i2w maxposint < i2w maxnegint);
val _ = pr_ln "test11h" test11h
val test11i = check (i2w maxnegint < i2w ~1);
val _ = pr_ln "test11i" test11i
end;

local 
    open Word32
in
val test12a = checkrange(0, 100) (fn k => w2i (i2w k + i2w 17) = add(k, 17));
val _ = pr_ln "test12a" test12a
val test12b = checkrange(0, 100) (fn k => w2i (i2w k - i2w 17) = sub(k, 17));
val _ = pr_ln "test12b" test12b
val test12c = checkrange(0, 100) (fn k => w2i (i2w k * i2w 17) = mul(k, 17));
val _ = pr_ln "test12c" test12c
val test12d = checkrange(0, 100) 
    (fn k => w2i (i2w k div i2w 17) = idiv(k, 17));
val _ = pr_ln "test12d" test12d
val test12e = checkrange(0, 100) 
    (fn k => w2i (i2w k mod i2w 17) = imod(k, 17));
val _ = pr_ln "test12e" test12e
val test12f = checkrange(0, 100) 
    (fn k => w2i (i2w k + i2w maxnegint) = add(k, maxnegint));
val _ = pr_ln "test12f" test12f
val test12g = checkrange(0, 100) 
    (fn k => w2i (i2w maxnegint - i2w k - i2w 1) = sub(maxposint,k));
val _ = pr_ln "test12g" test12g
val test12h = checkrange(0, 100) 
    (fn k => w2i (i2w k * i2w maxnegint) = mul(imod(k, 2), maxnegint));
val _ = pr_ln "test12h" test12h
val test12i = checkrange(0, 100) 
    (fn k => w2i (i2w k * i2w maxposint + i2w k) = mul(imod(k, 2), maxnegint));
val _ = pr_ln "test12i" test12i
val test12j = checkrange(0, 100) 
    (fn k => w2i (i2w k div i2w ~1) = 0);
val _ = pr_ln "test12j" test12j
val test12k = checkrange(0, 100) 
    (fn k => w2i (i2w k mod i2w ~1) = k);
val _ = pr_ln "test12k" test12k
val test12l = check(w2i (i2w maxposint + i2w 1) = maxnegint);
val _ = pr_ln "test12l" test12l
val test12m = check(w2i (i2w maxnegint - i2w 1) = maxposint);
val _ = pr_ln "test12m" test12m
val test12n = check(w2i (i2w ~1 div i2w 2) = maxposint);
val _ = pr_ln "test12n" test12n
val test12o = check(w2i (i2w ~1 mod i2w 2) = 1);
val _ = pr_ln "test12o" test12o
val test12p = check(w2i (i2w ~1 div i2w 100) = idiv(maxposint, 50));
val _ = pr_ln "test12p" test12p
(*5 on 32bit; 7 on 31bit*)
val test12q = check(w2i (i2w ~1 mod i2w 10) = 5);
val _ = pr_ln "test12q" test12q
val test12r = (i2w 17 div i2w 0 seq "WRONG") 
              handle Div => "OK" | _ => "WRONG";
val _ = pr_ln "test12r" test12r
val test12s = (i2w 17 mod i2w 0 seq "WRONG") 
              handle Div => "OK" | _ => "WRONG";
val _ = pr_ln "test12s" test12s
fun chk f (s, r) = 
    check'(fn _ => 
	   case f s of
	       SOME res => res = i2w r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a = 
    List.map (chk fromString)
             [("20Af", 8367),
	      (" \n\t20AfGrap", 8367),
	      ("0w20Af", 8367),
	      (" \n\t0w20AfGrap", 8367),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 1),
	      ("0wX1", 1),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val _ = pr_ln "test13a" (concat test13a)
val test13b = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+1", "~1", "-1", "GG"];	    
val _ = pr_ln "test13b" (concat test13b)

val test14a = 
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
	      (" \n\t10789crap", 10789),
	      ("0w10789", 10789),
	      (" \n\t0w10789crap", 10789),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 0),
	      ("0wX1", 0),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val _ = pr_ln "test14a" (concat test14a)
val test14b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+1", "~1", "-1", "ff"];	    
val _ = pr_ln "test14b" (concat test14b)
val test15a = 
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      (" \n\t10010crap", 18),
	      ("0w10010", 18),
	      (" \n\t0w10010crap", 18),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 0),
	      ("0wX1", 0),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val _ = pr_ln "test15a" (concat test15a)
val test15b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+1", "~1", "-1", "2", "8", "ff"];
val _ = pr_ln "test15b" (concat test15b)
val test16a = 
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
	      (" \n\t2071crap", 1081),
	      ("0w2071", 1081),
	      (" \n\t0w2071crap", 1081),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 0),
	      ("0wX1", 0),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val _ = pr_ln "test16a" (concat test16a)
val test16b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+1", "~1", "-1", "8", "ff"];
val _ = pr_ln "test16b" (concat test16b)
val test17a = 
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367), (" \n\t20AfGrap", 8367),
	      ("0wx20Af", 8367), (" \n\t0wx20AfGrap", 8367),
	      ("0wX20Af", 8367), (" \n\t0wX20AfGrap", 8367),
	      ("0x20Af", 8367), (" \n\t0x20AfGrap", 8367),
	      ("0X20Af", 8367), (" \n\t0X20AfGrap", 8367),
	      ("0", 0),
	      ("0w", 0),
	      ("0w ", 0),
	      ("0w1", 1),
	      ("0W1", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 1),
	      ("0wX1", 1)];
val _ = pr_ln "test17a" (concat test17a)
val test17b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+1", "~1", "-1"];
val _ = pr_ln "test17b" (concat test17b)
end;

local 
    fun fromToString i = 
	fromString (toString (fromLargeInt (Int32.toLarge i))) = SOME (fromLargeInt (Int32.toLarge i));

    fun scanFmt radix i = 
	let val w = fromLargeInt (Int32.toLarge i)
	    val s = fmt radix w
	in StringCvt.scanString (scan radix) s = SOME w end;

in
val test18 = 
    check'(fn _ => range (0, 200) fromToString);
val _ = pr_ln "test18" test18
val test19 = 
    check'(fn _ => range (0, 200) (scanFmt StringCvt.BIN));
val _ = pr_ln "test19" test19
val test20 = 
    check'(fn _ => range (0, 200) (scanFmt StringCvt.OCT));
val _ = pr_ln "test20" test20
val test21 = 
    check'(fn _ => range (0, 200) (scanFmt StringCvt.DEC));
val _ = pr_ln "test21" test21
val test22 = 
    check'(fn _ => range (0, 200) (scanFmt StringCvt.HEX));
val _ = pr_ln "test22" test22
end

local open Word32
  fun tagging () = Int.precision = SOME 31
in

  val test23a = tst "test23a" ((Word32.toInt 0wxFFFFFFFF seq false)
			       handle Overflow => true)

  val test23b = tst "test23b" (Word32.toIntX 0wxFFFFFFFF = ~1)

  val test23c = tst "test23c" 
    (if tagging() then
       ((Word32.toIntX 0wx7FFFFFFF seq false)
	handle Overflow => true)
     else 
       (SOME(Word32.toIntX 0wx7FFFFFFF) = Int.maxInt))

  val test23d = tst "test23d" 
    (if tagging() then
       ((Word32.toIntX 0wx80000000 seq false)
	handle Overflow => true)
     else 
       (SOME(Word32.toIntX 0wx80000000) = Int.minInt))

  val test23e = tst "test23e" (Word32.toIntX 0wx3FFFFFFF = 1073741823)

  val test23f = tst "test23f" (Word32.toIntX 0wxc0000000 = ~1073741824)

  val test23g = tst "test23g" 
    (if tagging() then
       ((Word32.toIntX 0wxbfffffff seq false)
	handle Overflow => true)
     else 
       (Word32.toIntX 0wxbfffffff = (Int.-(~1073741824, 1))))

  val _ = tst' "test24a" (fn() => i2w ~1 = 0wxFFFFFFFF) 
(*  val _ = print("~1 = " ^ toString (i2w ~1) ^ "<br>") *)
end
val _ = print "End test<br>"
end;
