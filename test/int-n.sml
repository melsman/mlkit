(* Tests of structure Int8 and Int16. *)

infix 1 seq
fun e1 seq e2 = e2
fun check b = if b then "OK" else "WRONG"
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN"
fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n")
fun tst  s b = tst0 s (check  b)
fun tst' s f = tst0 s (check' f)

functor Test(I : INTEGER) : sig end =
struct

fun range (from, to) p =
    let open I
    in case compare (from, to) of
           EQUAL => p from
         | LESS => p from andalso range (from + fromInt 1, to) p
         | GREATER => true
    end
fun checkrange bounds = check o range bounds
fun tstrange s bounds = (tst s) o range bounds

val N = case I.precision of
            SOME n => Int.toString n
          | NONE => "Inf"

val _ = print ("[Testing structure Int" ^ N ^ "...]\n")

open I
infix 7 quot rem
fun divmod s (i, d, q, r)  = tst s (i div d = q andalso i mod d = r)
fun quotrem s (i, d, q, r) = tst s (i quot d = q andalso i rem d = r)

val $ = I.fromInt

val test1a = divmod "test1a" ($ 10, $ 3, $ 3, $ 1)
val test1b = divmod "test1b" ($ ~10, $ 3, $ ~4, $ 2)
val test1c = divmod "test1c" ($ ~10, $ ~3, $ 3, $ ~1)
val test1d = divmod "test1d" ($ 10, $ ~3, $ ~4, $ ~2)

val test2a = quotrem "test2a" ($ 10, $ 3, $ 3, $ 1)
val test2b = quotrem "test2b" ($ ~10, $ 3, $ ~3, $ ~1)
val test2c = quotrem "test2c" ($ ~10, $ ~3, $ 3, $ ~1)
val test2d = quotrem "test2d" ($ 10, $ ~3, $ ~3, $ 1)

val test3 = tst "test3" (max($ ~5, $ 2) = $ 2 andalso max($ 5, $ 2) = $ 5)
val test4 = tst "test4" (min($ ~5, $ 3) = $ ~5 andalso min($ 5, $ 2) = $ 2)

val test5 = tst "test5" (sign ($ ~57) = ~1 andalso sign ($ 99) = 1 andalso sign ($ 0) = 0)
val test6 = tst "test6" (sameSign($ ~127, $ ~126) andalso sameSign($ 125, $ 126)
		         andalso sameSign($ 0, $ 0))

val test12 =
    tst0 "test12" (case (minInt, maxInt) of
		     (SOME mi, SOME ma) => check(sign mi = ~1 andalso sign ma = 1
						 andalso sameSign(mi, $ ~1) andalso sameSign(ma, $ 1))
		   | (NONE, NONE)       => "OK"
		   | _                  => "WRONG")

fun chk f (s, r) =
    tst' "chk" (fn _ =>
	   case f s of
	       SOME res => res = $ r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a =
    List.map (chk fromString)
             [("107", 107),
	      ("+107", 107),
	      ("~107", ~107),
	      ("-107", ~107),
	      (" \n\t107crap", 107),
	      (" \n\t+107crap", 107),
	      (" \n\t~107crap", ~107),
	      (" \n\t-107crap", ~107),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)]

val test13b =
    List.map (fn s => tst0 "test13b" (case fromString s of NONE => "OK" | _ => "WRONG"))
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "ff"]

val test14a =
    List.map (chkScan StringCvt.DEC)
             [("107", 107),
	      ("+107", 107),
	      ("~107", ~107),
	      ("-107", ~107),
	      (" \n\t107crap", 107),
	      (" \n\t+107crap", 107),
	      (" \n\t~107crap", ~107),
	      (" \n\t-107crap", ~107),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)]

val test14b =
    List.map (fn s => tst0 "test14b" (case StringCvt.scanString (scan StringCvt.DEC) s
	              of NONE => "OK" | _ => "WRONG"))
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "ff"]

val test15a =
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      ("+10010", 18),
	      ("~10010", ~18),
	      ("-10010", ~18),
	      (" \n\t10010crap", 18),
	      (" \n\t+10010crap", 18),
	      (" \n\t~10010crap", ~18),
	      (" \n\t-10010crap", ~18),
	      ("0w101", 0),
	      ("0W101", 0),
	      ("0x101", 0),
	      ("0X101", 0),
	      ("0wx101", 0),
	      ("0wX101", 0)]

val test15b =
    List.map (fn s => tst0 "test15b" (case StringCvt.scanString (scan StringCvt.BIN) s
	              of NONE => "OK" | _ => "WRONG"))
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "2", "8", "ff"]

val test16a =
    List.map (chkScan StringCvt.OCT)
             [("153", 107),
	      ("+153", 107),
	      ("~153", ~107),
	      ("-153", ~107),
	      (" \n\t153crap", 107),
	      (" \n\t+153crap", 107),
	      (" \n\t~153crap", ~107),
	      (" \n\t-153crap", ~107),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)]

val test16b =
    List.map (fn s => tst0 "test16b" (case StringCvt.scanString (scan StringCvt.OCT) s
	              of NONE => "OK" | _ => "WRONG"))
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "8", "ff"]

val test17a =
    List.map (chkScan StringCvt.HEX)
             [("2A", 42),
	      ("+2A", 42),
	      ("~2A", ~42),
	      ("-2A", ~42),
	      (" \n\t2AGrap", 42),
	      (" \n\t+2AGrap", 42),
	      (" \n\t~2AGrap", ~42),
	      (" \n\t-2AGrap", ~42),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x", 0),
	      ("0x ", 0),
	      ("0xG", 0),
	      ("0X", 0),
	      ("0XG", 0),
	      ("0x34", 52),
	      ("0X34", 52),
	      ("-0x34", ~52),
	      ("-0X34", ~52),
	      ("~0x34", ~52),
	      ("~0X34", ~52),
	      ("+0x34", 52),
	      ("+0X34", 52),
	      ("0wx34", 0),
	      ("0wX34", 0)]

val test17b =
    List.map (fn s => tst0 "test17b" (case StringCvt.scanString (scan StringCvt.HEX) s
	              of NONE => "OK" | _ => "WRONG"))
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1"]

fun fromToString i =
    fromString (toString i) = SOME i

fun scanFmt radix i =
    StringCvt.scanString (scan radix) (fmt radix i) = SOME i

val test18 =
    tst' "test18" (fn _ => range ($ ~128, $ 127) fromToString);

val test19 =
    tst' "test19" (fn _ => range ($ ~128, $ 127) (scanFmt StringCvt.BIN))

val test20 =
    tst' "test20" (fn _ => range ($ ~128, $ 127) (scanFmt StringCvt.OCT))

val test21 =
    tst' "test21" (fn _ => range ($ ~128, $ 127) (scanFmt StringCvt.DEC))

val test22 =
    tst' "test22" (fn _ => range ($ ~128, $ 127) (scanFmt StringCvt.HEX))

val test23a = tst' "test23a" (fn _ => scanFmt StringCvt.HEX (valOf I.maxInt))
val test23b = tst' "test23b" (fn _ => scanFmt StringCvt.DEC (valOf I.maxInt))
val test23c = tst' "test23c" (fn _ => scanFmt StringCvt.OCT (valOf I.maxInt))
val test23d = tst' "test23d" (fn _ => scanFmt StringCvt.BIN (valOf I.maxInt))

val test24a = tst' "test24a" (fn _ => scanFmt StringCvt.HEX (valOf I.minInt))
val test24b = tst' "test24b" (fn _ => scanFmt StringCvt.DEC (valOf I.minInt))
val test24c = tst' "test24c" (fn _ => scanFmt StringCvt.OCT (valOf I.minInt))
val test24d = tst' "test24d" (fn _ => scanFmt StringCvt.BIN (valOf I.minInt))

val test25a = tst' "test25a" (fn _ => scanFmt StringCvt.HEX (I.+(valOf I.minInt, $ 10)))
val test25b = tst' "test25b" (fn _ => scanFmt StringCvt.DEC (I.+(valOf I.minInt, $ 10)))
val test25c = tst' "test25c" (fn _ => scanFmt StringCvt.OCT (I.+(valOf I.minInt, $ 10)))
val test25d = tst' "test25d" (fn _ => scanFmt StringCvt.BIN (I.+(valOf I.minInt, $ 10)))

fun chk' t f s =
    tst' t (fn _ => ((f s; false) handle Overflow => true))
fun chkScanOvf t fmt = chk' t (StringCvt.scanString (scan fmt))

val (dec_smallbad_string, dec_smallgood_string,
     dec_biggood_string, dec_bigbad_string) =
    case precision of
        SOME 8 => ("~129", "~128", "127", "128")
      | SOME 16 => ("~32769", "~32768", "32767", "32768")
      | SOME 31 => ("~1073741825", "~1073741824", "1073741823", "1073741824")
      | SOME 32 => ("~2147483649", "~2147483648","2147483647","2147483648")
      | SOME 63 => ("~4611686018427387905","~4611686018427387904","4611686018427387903","4611686018427387904")
      | SOME 64 => ("~9223372036854775809","~9223372036854775808","9223372036854775807","9223372036854775808")
      | _ => raise Fail "test precision not supported"

val test26a = chkScanOvf "test26a" StringCvt.HEX "~40000001"
val test26b = chkScanOvf "test26b" StringCvt.DEC dec_smallbad_string
val test26c = chkScanOvf "test26c" StringCvt.OCT "~10000000001"
val test26d = chkScanOvf "test26d" StringCvt.BIN "~1000000000000000000000000000001"

val test27a = chkScanOvf "test27a" StringCvt.HEX "40000000"
val test27b = chkScanOvf "test27b" StringCvt.DEC dec_bigbad_string
val test27c = chkScanOvf "test27c" StringCvt.OCT "10000000000"
val test27d = chkScanOvf "test27d" StringCvt.BIN "1000000000000000000000000000000"

val test28a = tst' "test28a" (fn () => toString (valOf maxInt) = dec_biggood_string)
val test28b = tst' "test28b" (fn () => toString (valOf minInt) = dec_smallgood_string)

val test29a = tst' "test29a" (fn () => fromString dec_biggood_string = maxInt)
val test29b = tst' "test29b" (fn () => fromString dec_smallgood_string = minInt)

val _ = print ("[End of Testing structure Int" ^ N ^ "]\n")

end

structure T8 = Test(Int8)

structure T16 = Test(Int16)
