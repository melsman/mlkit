(* Runtime boundary inputs exercise all 126 IntInf and 50 LargeWord forward
 * compositions. Expected values use mathematical ranges/modulo, independently
 * of the conversion implementations. *)
val checks = ref 0
fun pow n = IntInf.<<(1, Word.fromInt n)
val candidates =
    let val bounds = [7,8,30,31,32,62,63,64]
        fun around n = let val p = pow n in [p-1,p,p+1,~p-1,~p,~p+1] end
    in [~1,0,1] @ List.concat (List.map around bounds)
    end
fun check label expected thunk =
    let val actual = (thunk() handle Overflow => "Overflow")
    in checks := !checks + 1;
       if actual = expected then ()
       else raise Fail(label ^ ": expected " ^ expected ^ ", got " ^ actual)
    end
fun integerExpected n v =
    if v < ~(pow(n-1)) orelse v >= pow(n-1) then "Overflow"
    else IntInf.toString v
fun wordExpected n v = IntInf.toString (IntInf.mod(v,pow n))

functor IntIntConversion(structure S : INTEGER
                         structure D : INTEGER) =
struct
  fun run v =
      case (S.fromString(IntInf.toString v) handle Overflow => NONE) of
          NONE => ()
        | SOME x =>
          let val expected = integerExpected (valOf D.precision) v
              val label = "Int" ^ Int.toString(valOf S.precision) ^ " -> Int" ^
                          Int.toString(valOf D.precision) ^ " input " ^ IntInf.toString v
          in check label expected (fn () => D.toString(D.fromLarge(S.toLarge x)));
             check (label ^ " let") expected
               (fn () => let val large = S.toLarge x
                             val alias = large
                         in D.toString(D.fromLarge alias)
                         end)
          end
  val () = List.app run candidates
end
functor IntWordConversion(structure S : INTEGER
                          structure D : WORD) =
struct
  fun run v =
      case (S.fromString(IntInf.toString v) handle Overflow => NONE) of
          NONE => ()
        | SOME x => check ("Int" ^ Int.toString(valOf S.precision) ^ " -> Word" ^
                           Int.toString D.wordSize ^ " input " ^ IntInf.toString v)
                          (wordExpected D.wordSize v)
                          (fn () => D.fmt StringCvt.DEC (D.fromLargeInt(S.toLarge x)))
  val () = List.app run candidates
end
functor WordIntConversion(structure S : WORD
                          structure D : INTEGER) =
struct
  fun run v =
      if v < 0 orelse v >= pow S.wordSize then ()
      else
        let val x = valOf(S.fromString(IntInf.fmt StringCvt.HEX v))
            val signed = if v >= pow(S.wordSize-1) then v-pow S.wordSize else v
            val bits = valOf D.precision
            val label = "Word" ^ Int.toString S.wordSize ^ " -> Int" ^
                        Int.toString bits ^ " input " ^ IntInf.toString v
        in check (label ^ " unsigned") (integerExpected bits v)
                 (fn () => D.toString(D.fromLarge(S.toLargeInt x)));
           check (label ^ " signed") (integerExpected bits signed)
                 (fn () => D.toString(D.fromLarge(S.toLargeIntX x)))
        end
  val () = List.app run candidates
end
functor WordWordConversion(structure S : WORD
                           structure D : WORD) =
struct
  fun run v =
      if v < 0 orelse v >= pow S.wordSize then ()
      else
        let val x = valOf(S.fromString(IntInf.fmt StringCvt.HEX v))
            val signed = if v >= pow(S.wordSize-1) then v-pow S.wordSize else v
            val expected = wordExpected D.wordSize v
            val expectedX = wordExpected D.wordSize signed
            val label = "Word" ^ Int.toString S.wordSize ^ " -> Word" ^
                        Int.toString D.wordSize ^ " input " ^ IntInf.toString v
        in check (label ^ " unsigned via IntInf") expected
                 (fn () => D.fmt StringCvt.DEC(D.fromLargeInt(S.toLargeInt x)));
           check (label ^ " signed via IntInf") expectedX
                 (fn () => D.fmt StringCvt.DEC(D.fromLargeInt(S.toLargeIntX x)));
           check (label ^ " unsigned via LargeWord") expected
                 (fn () => D.fmt StringCvt.DEC(D.fromLarge(S.toLarge x)));
           check (label ^ " signed via LargeWord") expectedX
                 (fn () => D.fmt StringCvt.DEC(D.fromLarge(S.toLargeX x)))
        end
  val () = List.app run candidates
end
structure Int31ToInt31 = IntIntConversion(structure S = Int31
    structure D = Int31)
structure Int31ToInt32 = IntIntConversion(structure S = Int31
    structure D = Int32)
structure Int31ToInt63 = IntIntConversion(structure S = Int31
    structure D = Int63)
structure Int31ToInt64 = IntIntConversion(structure S = Int31
    structure D = Int64)
structure Int32ToInt31 = IntIntConversion(structure S = Int32
    structure D = Int31)
structure Int32ToInt32 = IntIntConversion(structure S = Int32
    structure D = Int32)
structure Int32ToInt63 = IntIntConversion(structure S = Int32
    structure D = Int63)
structure Int32ToInt64 = IntIntConversion(structure S = Int32
    structure D = Int64)
structure Int63ToInt31 = IntIntConversion(structure S = Int63
    structure D = Int31)
structure Int63ToInt32 = IntIntConversion(structure S = Int63
    structure D = Int32)
structure Int63ToInt63 = IntIntConversion(structure S = Int63
    structure D = Int63)
structure Int63ToInt64 = IntIntConversion(structure S = Int63
    structure D = Int64)
structure Int64ToInt31 = IntIntConversion(structure S = Int64
    structure D = Int31)
structure Int64ToInt32 = IntIntConversion(structure S = Int64
    structure D = Int32)
structure Int64ToInt63 = IntIntConversion(structure S = Int64
    structure D = Int63)
structure Int64ToInt64 = IntIntConversion(structure S = Int64
    structure D = Int64)
structure Int31ToWord8 = IntWordConversion(structure S = Int31
    structure D = Word8)
structure Int31ToWord31 = IntWordConversion(structure S = Int31
    structure D = Word31)
structure Int31ToWord32 = IntWordConversion(structure S = Int31
    structure D = Word32)
structure Int31ToWord63 = IntWordConversion(structure S = Int31
    structure D = Word63)
structure Int31ToWord64 = IntWordConversion(structure S = Int31
    structure D = Word64)
structure Int32ToWord8 = IntWordConversion(structure S = Int32
    structure D = Word8)
structure Int32ToWord31 = IntWordConversion(structure S = Int32
    structure D = Word31)
structure Int32ToWord32 = IntWordConversion(structure S = Int32
    structure D = Word32)
structure Int32ToWord63 = IntWordConversion(structure S = Int32
    structure D = Word63)
structure Int32ToWord64 = IntWordConversion(structure S = Int32
    structure D = Word64)
structure Int63ToWord8 = IntWordConversion(structure S = Int63
    structure D = Word8)
structure Int63ToWord31 = IntWordConversion(structure S = Int63
    structure D = Word31)
structure Int63ToWord32 = IntWordConversion(structure S = Int63
    structure D = Word32)
structure Int63ToWord63 = IntWordConversion(structure S = Int63
    structure D = Word63)
structure Int63ToWord64 = IntWordConversion(structure S = Int63
    structure D = Word64)
structure Int64ToWord8 = IntWordConversion(structure S = Int64
    structure D = Word8)
structure Int64ToWord31 = IntWordConversion(structure S = Int64
    structure D = Word31)
structure Int64ToWord32 = IntWordConversion(structure S = Int64
    structure D = Word32)
structure Int64ToWord63 = IntWordConversion(structure S = Int64
    structure D = Word63)
structure Int64ToWord64 = IntWordConversion(structure S = Int64
    structure D = Word64)
structure Word8ToInt31 = WordIntConversion(structure S = Word8
    structure D = Int31)
structure Word8ToInt32 = WordIntConversion(structure S = Word8
    structure D = Int32)
structure Word8ToInt63 = WordIntConversion(structure S = Word8
    structure D = Int63)
structure Word8ToInt64 = WordIntConversion(structure S = Word8
    structure D = Int64)
structure Word31ToInt31 = WordIntConversion(structure S = Word31
    structure D = Int31)
structure Word31ToInt32 = WordIntConversion(structure S = Word31
    structure D = Int32)
structure Word31ToInt63 = WordIntConversion(structure S = Word31
    structure D = Int63)
structure Word31ToInt64 = WordIntConversion(structure S = Word31
    structure D = Int64)
structure Word32ToInt31 = WordIntConversion(structure S = Word32
    structure D = Int31)
structure Word32ToInt32 = WordIntConversion(structure S = Word32
    structure D = Int32)
structure Word32ToInt63 = WordIntConversion(structure S = Word32
    structure D = Int63)
structure Word32ToInt64 = WordIntConversion(structure S = Word32
    structure D = Int64)
structure Word63ToInt31 = WordIntConversion(structure S = Word63
    structure D = Int31)
structure Word63ToInt32 = WordIntConversion(structure S = Word63
    structure D = Int32)
structure Word63ToInt63 = WordIntConversion(structure S = Word63
    structure D = Int63)
structure Word63ToInt64 = WordIntConversion(structure S = Word63
    structure D = Int64)
structure Word64ToInt31 = WordIntConversion(structure S = Word64
    structure D = Int31)
structure Word64ToInt32 = WordIntConversion(structure S = Word64
    structure D = Int32)
structure Word64ToInt63 = WordIntConversion(structure S = Word64
    structure D = Int63)
structure Word64ToInt64 = WordIntConversion(structure S = Word64
    structure D = Int64)
structure Word8ToWord8 = WordWordConversion(structure S = Word8
    structure D = Word8)
structure Word8ToWord31 = WordWordConversion(structure S = Word8
    structure D = Word31)
structure Word8ToWord32 = WordWordConversion(structure S = Word8
    structure D = Word32)
structure Word8ToWord63 = WordWordConversion(structure S = Word8
    structure D = Word63)
structure Word8ToWord64 = WordWordConversion(structure S = Word8
    structure D = Word64)
structure Word31ToWord8 = WordWordConversion(structure S = Word31
    structure D = Word8)
structure Word31ToWord31 = WordWordConversion(structure S = Word31
    structure D = Word31)
structure Word31ToWord32 = WordWordConversion(structure S = Word31
    structure D = Word32)
structure Word31ToWord63 = WordWordConversion(structure S = Word31
    structure D = Word63)
structure Word31ToWord64 = WordWordConversion(structure S = Word31
    structure D = Word64)
structure Word32ToWord8 = WordWordConversion(structure S = Word32
    structure D = Word8)
structure Word32ToWord31 = WordWordConversion(structure S = Word32
    structure D = Word31)
structure Word32ToWord32 = WordWordConversion(structure S = Word32
    structure D = Word32)
structure Word32ToWord63 = WordWordConversion(structure S = Word32
    structure D = Word63)
structure Word32ToWord64 = WordWordConversion(structure S = Word32
    structure D = Word64)
structure Word63ToWord8 = WordWordConversion(structure S = Word63
    structure D = Word8)
structure Word63ToWord31 = WordWordConversion(structure S = Word63
    structure D = Word31)
structure Word63ToWord32 = WordWordConversion(structure S = Word63
    structure D = Word32)
structure Word63ToWord63 = WordWordConversion(structure S = Word63
    structure D = Word63)
structure Word63ToWord64 = WordWordConversion(structure S = Word63
    structure D = Word64)
structure Word64ToWord8 = WordWordConversion(structure S = Word64
    structure D = Word8)
structure Word64ToWord31 = WordWordConversion(structure S = Word64
    structure D = Word31)
structure Word64ToWord32 = WordWordConversion(structure S = Word64
    structure D = Word32)
structure Word64ToWord63 = WordWordConversion(structure S = Word64
    structure D = Word63)
structure Word64ToWord64 = WordWordConversion(structure S = Word64
    structure D = Word64)
structure DefaultIntToWord = IntWordConversion(structure S = Int
                                              structure D = Word)
structure DefaultWordToInt = WordIntConversion(structure S = Word
                                              structure D = Int)
val () = if !checks > 1000 then print "numeric conversions: OK\n"
         else raise Fail "numeric conversion matrix did not run"
