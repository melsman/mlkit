local
(* Shifts and conversions of 63-bit words and integers with constant
 * operands are folded at compile time; the folding used to be done at
 * 64 bits, so that a signed shift did not replicate the top bit and
 * Int.fromLarge accepted values outside the 63-bit range. *)

fun p s = print (s ^ "\n")
fun w s w = p (s ^ " = 0wx" ^ Word63.toString w)
fun i s f = p (s ^ " = " ^ (Int63.toString (f ()) handle Overflow => "Overflow"))

val () = w "~>> top 62" (Word63.~>> (0wx4000000000000000, 0w62))
val () = w "~>> top 1" (Word63.~>> (0wx4000000000000000, 0w1))
val () = w "~>> top 0" (Word63.~>> (0wx4000000000000000, 0w0))
val () = w "~>> 1 62" (Word63.~>> (0w1, 0w62))
val () = w "~>> allOnes 5" (Word63.~>> (0wx7FFFFFFFFFFFFFFF, 0w5))
val () = w ">> top 62" (Word63.>> (0wx4000000000000000, 0w62))
val () = w "<< 1 62" (Word63.<< (0w1, 0w62))
val () = w "<< 1 63" (Word63.<< (0w1, 0w63))
val () = w "Word63 ~>> top 62" (Word63.fromLarge (Word63.toLarge (Word63.~>> (Word63.fromLarge 0wx4000000000000000, 0w62))))
val () = i "Int.fromLarge 2^62" (fn () => Int63.fromLarge (IntInf.pow (IntInf.fromInt 2, 62)))
val () = i "Int.fromLarge (2^62 - 1)" (fn () => Int63.fromLarge (IntInf.- (IntInf.pow (IntInf.fromInt 2, 62), IntInf.fromInt 1)))
val () = i "Int.fromLarge ~2^62" (fn () => Int63.fromLarge (IntInf.~ (IntInf.pow (IntInf.fromInt 2, 62))))
val () = i "Int.fromLarge (~2^62 - 1)" (fn () => Int63.fromLarge (IntInf.- (IntInf.~ (IntInf.pow (IntInf.fromInt 2, 62)), IntInf.fromInt 1)))
val () = i "Int63.fromLarge 2^62" (fn () => Int63.fromLarge (IntInf.pow (IntInf.fromInt 2, 62)))

in
end
local
(* Comparisons with a NaN are false.  The values are hidden in a ref so
 * that both the compiled code and the constant folder are exercised. *)

fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val r = ref 0.0
val zero = !r
val nan = zero / zero
val one = 1.0 + zero
val () = p ("nan < 1 " ^ b (nan < one) ^ ", nan <= 1 " ^ b (nan <= one)
            ^ ", nan > 1 " ^ b (nan > one) ^ ", nan >= 1 " ^ b (nan >= one))
val () = p ("1 < nan " ^ b (one < nan) ^ ", 1 <= nan " ^ b (one <= nan)
            ^ ", 1 > nan " ^ b (one > nan) ^ ", 1 >= nan " ^ b (one >= nan))
val () = p ("if nan < 1: " ^ (if nan < one then "yes" else "no")
            ^ ", if nan <= 1: " ^ (if nan <= one then "yes" else "no")
            ^ ", if nan > 1: " ^ (if nan > one then "yes" else "no")
            ^ ", if nan >= 1: " ^ (if nan >= one then "yes" else "no"))
val () = p ("1 < 2 " ^ b (one < 2.0) ^ ", 2 < 1 " ^ b (2.0 < one)
            ^ ", 1 <= 1 " ^ b (one <= 1.0) ^ ", 1 > 2 " ^ b (one > 2.0)
            ^ ", 1 >= 1 " ^ b (one >= 1.0) ^ ", 2 >= 1 " ^ b (2.0 >= one))
val () = p ("constants: " ^ b (0.0/0.0 < 1.0) ^ " " ^ b (1.0 < 2.0) ^ " " ^ b (2.0 <= 1.0))
val () = p ("Real.== (nan, nan) " ^ b (Real.== (nan, nan)) ^ ", Real.?= (nan, 1) " ^ b (Real.?= (nan, one)))

in
end
local
(* Negation and abs act on the sign bit alone: ~0.0 is a negative
 * zero, and abs clears the sign of a NaN.  The values are hidden in a
 * ref so that the compiled code is exercised, not the constant folder. *)

fun p s = print (s ^ "\n")
fun b x = if x then "true" else "false"
val r = ref 0.0
val zero = !r
val nan = zero / zero
val one = 1.0 + zero
val () = p ("signBit (~zero) " ^ b (Real.signBit (~zero))
            ^ ", signBit (~(~zero)) " ^ b (Real.signBit (~(~zero)))
            ^ ", signBit (~one) " ^ b (Real.signBit (~one)))
val () = p ("signBit (abs (~nan)) " ^ b (Real.signBit (Real.abs (~nan)))
            ^ ", signBit (abs (~one)) " ^ b (Real.signBit (Real.abs (~one)))
            ^ ", abs (~2.5) = " ^ Real.toString (Real.abs (~2.5 + zero)))
val () = p ("1/(~zero) = " ^ Real.toString (one / (~zero)))
val () = p ("constants: " ^ b (Real.signBit (~0.0)) ^ " " ^ b (Real.signBit (Real.abs (~1.0))) ^ " " ^ Real.toString (~(~2.5)))
val () = p ("unboxed: " ^ (let val a = Array.fromList [zero, one, ~one] in b (Real.signBit (~(Array.sub (a, 0)))) ^ " " ^ Real.toString (Real.abs (Array.sub (a, 2))) ^ " " ^ Real.toString (~(Array.sub (a, 1))) end))

in
end
local
(* Word negation is two's complement negation, which wraps; it used to
 * go through toInt, which raises Overflow on a top bit that is set. *)
fun p s = print (s ^ "\n")
fun t f = (f ()) handle Overflow => "Overflow"
val () = p ("Word63: " ^ t (fn () => Word63.toString (Word63.~ 0wx6C7D9922F47CE479)))
val () = p ("Word63: " ^ t (fn () => Word63.toString (Word63.~ 0w1)))
val () = p ("Word: " ^ t (fn () => Word.toString (Word.~ 0w0)))
val () = p ("Word63: " ^ t (fn () => Word63.toString (Word63.~ 0wx4000000000000001)))
val () = p ("Word31: " ^ t (fn () => Word31.toString (Word31.~ 0wx40000001)))
val () = p ("Word32: " ^ t (fn () => Word32.toString (Word32.~ 0wx80000001)))
val () = p ("Word64: " ^ t (fn () => Word64.toString (Word64.~ 0wx8000000000000001)))
val () = p ("Word8: " ^ t (fn () => Word8.toString (Word8.~ 0wx81)))
val () = p ("Word16: " ^ t (fn () => Word16.toString (Word16.~ (Word16.fromInt 0x8001))))
val () = p ("law: " ^ (if Word.~ 0wx123456789ABCDEF = 0w0 - 0wx123456789ABCDEF then "ok" else "bad"))

in
end
local
(* Scanning a numeral too large for the word raises Overflow. *)
fun p s = print (s ^ "\n")
fun t f = (case f () of SOME s => s | NONE => "NONE") handle Overflow => "Overflow"
val () = p ("Word DEC: " ^ t (fn () => Option.map Word.toString (StringCvt.scanString (Word.scan StringCvt.DEC) (Word.fmt StringCvt.DEC (Word.notb 0w0) ^ "0"))))
val () = p ("Word HEX: " ^ t (fn () => Option.map Word.toString (StringCvt.scanString (Word.scan StringCvt.HEX) (Word.fmt StringCvt.HEX (Word.notb 0w0) ^ "0"))))
val () = p ("Word fromString: " ^ t (fn () => Option.map Word.toString (Word.fromString (Word.toString (Word.notb 0w0) ^ "0"))))
val () = p ("Word max: " ^ t (fn () => Option.map Word63.toString (Word63.fromString (Word63.toString (Word63.notb 0w0)))))
val () = p ("Word max+1 DEC: " ^ t (fn () => Option.map Word63.toString (StringCvt.scanString (Word63.scan StringCvt.DEC) "9223372036854775808")))
val () = p ("Word 2^63-1 DEC: " ^ t (fn () => Option.map Word.toString (StringCvt.scanString (Word.scan StringCvt.DEC) "9223372036854775807")))
val () = p ("Word8: " ^ t (fn () => Option.map Word8.toString (Word8.fromString "100")))
val () = p ("Word8 ok: " ^ t (fn () => Option.map Word8.toString (Word8.fromString "FF")))
val () = p ("Word8 DEC 256: " ^ t (fn () => Option.map Word8.toString (StringCvt.scanString (Word8.scan StringCvt.DEC) "256")))
val () = p ("Word8 DEC 255: " ^ t (fn () => Option.map Word8.toString (StringCvt.scanString (Word8.scan StringCvt.DEC) "255")))
val () = p ("Word31: " ^ t (fn () => Option.map Word31.toString (Word31.fromString "80000000")))
val () = p ("Word31 ok: " ^ t (fn () => Option.map Word31.toString (Word31.fromString "7FFFFFFF")))
val () = p ("Word32: " ^ t (fn () => Option.map Word32.toString (Word32.fromString "100000000")))
val () = p ("Word32 ok: " ^ t (fn () => Option.map Word32.toString (Word32.fromString "FFFFFFFF")))
val () = p ("Word32 DEC: " ^ t (fn () => Option.map Word32.toString (StringCvt.scanString (Word32.scan StringCvt.DEC) "4294967296")))
val () = p ("Word63: " ^ t (fn () => Option.map Word63.toString (Word63.fromString "8000000000000000")))
val () = p ("Word63 ok: " ^ t (fn () => Option.map Word63.toString (Word63.fromString "7FFFFFFFFFFFFFFF")))
val () = p ("Word64: " ^ t (fn () => Option.map Word64.toString (Word64.fromString "10000000000000000")))
val () = p ("Word64 ok: " ^ t (fn () => Option.map Word64.toString (Word64.fromString "FFFFFFFFFFFFFFFF")))
val () = p ("Word64 DEC: " ^ t (fn () => Option.map Word64.toString (StringCvt.scanString (Word64.scan StringCvt.DEC) "18446744073709551616")))
val () = p ("Word64 DEC ok: " ^ t (fn () => Option.map Word64.toString (StringCvt.scanString (Word64.scan StringCvt.DEC) "18446744073709551615")))
val () = p ("Word16: " ^ t (fn () => Option.map Word16.toString (Word16.fromString "10000")))
val () = p ("Word16 ok: " ^ t (fn () => Option.map Word16.toString (Word16.fromString "FFFF")))

in
end
local
(* Word8.toLargeX sign-extends to the whole of LargeWord. *)
fun p s = print (s ^ "\n")
val () = p ("toLargeX E2 = " ^ LargeWord.toString (Word8.toLargeX 0wxE2))
val () = p ("toLargeX 7F = " ^ LargeWord.toString (Word8.toLargeX 0wx7F))
val () = p ("toLargeX FF = " ^ LargeWord.toString (Word8.toLargeX 0wxFF))
val () = p ("toLarge FF = " ^ LargeWord.toString (Word8.toLarge 0wxFF))
val () = p ("toIntX E2 = " ^ Int.toString (Word8.toIntX 0wxE2))
val () = p ("via large: " ^ Int.toString (Int.fromLarge (LargeWord.toLargeIntX (Word8.toLargeX 0wxE2))))
val () = p ("toLargeIntX FF = " ^ LargeInt.toString (Word8.toLargeIntX 0wxFF))

in
end
local
(* quot (minInt, ~1) overflows and must raise Overflow rather than
 * return minInt or trap in the division instruction; rem (minInt, ~1)
 * is 0. *)
fun p s = print (s ^ "\n")
fun t f = (f ()) handle Overflow => "Overflow" | Div => "Div"
val () = p ("Int quot: " ^ t (fn () => Int.toString (Int.quot (valOf Int.minInt, ~1))))
val () = p ("Int rem: " ^ t (fn () => Int.toString (Int.rem (valOf Int.minInt, ~1))))
val () = p ("Int quot 7 ~2: " ^ t (fn () => Int.toString (Int.quot (7, ~2))) ^ " rem " ^ t (fn () => Int.toString (Int.rem (7, ~2))))
val () = p ("Int quot ~7 2: " ^ t (fn () => Int.toString (Int.quot (~7, 2))) ^ " rem " ^ t (fn () => Int.toString (Int.rem (~7, 2))))
val () = p ("Int quot by 0: " ^ t (fn () => Int.toString (Int.quot (1, 0))))
val () = p ("Int31 quot: " ^ t (fn () => Int31.toString (Int31.quot (valOf Int31.minInt, ~1))))
val () = p ("Int31 rem: " ^ t (fn () => Int31.toString (Int31.rem (valOf Int31.minInt, ~1))))
val () = p ("Int32 quot: " ^ t (fn () => Int32.toString (Int32.quot (valOf Int32.minInt, ~1))))
val () = p ("Int32 rem: " ^ t (fn () => Int32.toString (Int32.rem (valOf Int32.minInt, ~1))))
val () = p ("Int63 quot: " ^ t (fn () => Int63.toString (Int63.quot (valOf Int63.minInt, ~1))))
val () = p ("Int63 rem: " ^ t (fn () => Int63.toString (Int63.rem (valOf Int63.minInt, ~1))))
val () = p ("Int64 quot: " ^ t (fn () => Int64.toString (Int64.quot (valOf Int64.minInt, ~1))))
val () = p ("Int64 rem: " ^ t (fn () => Int64.toString (Int64.rem (valOf Int64.minInt, ~1))))
val () = p ("Int64 quot 7 ~2: " ^ t (fn () => Int64.toString (Int64.quot (7, ~2))) ^ " rem " ^ t (fn () => Int64.toString (Int64.rem (~7, 2))))
val () = p ("Int32 quot minInt 1: " ^ t (fn () => Int32.toString (Int32.quot (valOf Int32.minInt, 1))))

in
end
local
(* fmt HEX uses upper-case digits, as for the other integer types. *)
fun p s = print (s ^ "\n")
val () = p (LargeInt.fmt StringCvt.HEX (LargeInt.fromInt 255))
val () = p (LargeInt.fmt StringCvt.HEX (LargeInt.fromInt ~48879))
val () = p (IntInf.fmt StringCvt.HEX (IntInf.pow (IntInf.fromInt 2, 70)))
val () = p (IntInf.fmt StringCvt.HEX (IntInf.fromInt 0))
val () = p (IntInf.fmt StringCvt.OCT (IntInf.fromInt 511) ^ " " ^ IntInf.fmt StringCvt.BIN (IntInf.fromInt 10) ^ " " ^ IntInf.fmt StringCvt.DEC (IntInf.fromInt ~10))
val () = p (Int.fmt StringCvt.HEX 255 ^ " " ^ Word.fmt StringCvt.HEX 0w255)

in
end
