(** SigDoc *)
structure Real32 : REAL =
struct
  type real = real32
  structure Math = Math32
  val radix = 2
  val precision = 24
  val maxFinite : real = 3.40282346638528859811704183484516925e38
  val minPos : real = 1.40129846432481707092372958328991613e~45
  val minNormalPos : real = 1.17549435082228750796873653722224568e~38
  val posInf : real = prim ("id", 0wx7f800000:word)
  val negInf : real = ~posInf
  val op + : real * real -> real = op +
  val op - : real * real -> real = op -
  val op * : real * real -> real = op *
  val op / : real * real -> real = op /
  val ~ : real -> real = ~
  val abs : real -> real = abs
  val op < : real * real -> bool = op <
  val op <= : real * real -> bool = op <=
  val op > : real * real -> bool = op >
  val op >= : real * real -> bool = op >=
  fun op == (x:real, y:real) : bool = prim ("__equal_real32", (x,y))
  fun op != (x:real, y:real) : bool = prim ("__unequal_real32", (x,y))
  fun *+ (a,b,c) = a*b+c
  fun *- (a,b,c) = a*b-c
  fun toLarge (x:real) : LargeReal.real = prim ("__real32_to_real", x)
  fun narrow (x:LargeReal.real) : real = prim ("__real_to_real32", x)
  fun fromInt (x:int) : real = prim ("__int_to_real32", x)
  fun bits (x:real) : word = prim ("id", x)
  fun fromBits (x:word) : real = prim ("id", x)
  fun isNan x = op != (x,x)
  fun signBit x = Word.andb(bits x,0wx80000000) <> 0w0
  fun isFinite x = Word.andb(bits x,0wx7f800000) <> 0wx7f800000
  fun isNormal x =
      let val exponent = Word.andb(bits x,0wx7f800000)
      in exponent <> 0w0 andalso exponent <> 0wx7f800000
      end
  fun sameSign (x,y) = signBit x = signBit y
  fun copySign (x,y) =
      fromBits (Word.orb(Word.andb(bits x,0wx7fffffff),Word.andb(bits y,0wx80000000)))
  fun min (x,y) =
      if isNan x then y
      else if isNan y orelse x < y orelse (op == (x,y) andalso signBit x) then x else y
  fun max (x,y) =
      if isNan x then y
      else if isNan y orelse x > y orelse (op == (x,y) andalso not (signBit x)) then x else y
  fun sign x = if isNan x then raise Domain else if x > 0.0 then 1 else if x < 0.0 then ~1 else 0
  fun unordered (x,y) = isNan x orelse isNan y
  fun op ?= (x,y) = unordered(x,y) orelse op == (x,y)
  fun compare (x,y) =
      if unordered(x,y) then raise IEEEReal.Unordered
      else if x < y then LESS else if x > y then GREATER else EQUAL
  fun compareReal (x,y) =
      if unordered(x,y) then IEEEReal.UNORDERED
      else if x < y then IEEEReal.LESS else if x > y then IEEEReal.GREATER else IEEEReal.EQUAL
  fun class x =
      if isNan x then IEEEReal.NAN
      else if not (isFinite x) then IEEEReal.INF
      else if op == (x,0.0) then IEEEReal.ZERO
      else if isNormal x then IEEEReal.NORMAL else IEEEReal.SUBNORMAL
  fun checkFloat x = if isNan x then raise Div else if isFinite x then x else raise Overflow
  fun rem (x:real,y:real) : real = prim ("@fmodf", (x,y))
  fun nextAfter_ (x:real,y:real) : real = prim ("@nextafterf", (x,y))
  fun nextAfter (x,y) =
      if isNan x then x else if isNan y then y else if isFinite x then nextAfter_(x,y) else x
  fun withMode mode f x =
      let val previous = IEEEReal.getRoundingMode()
          val () = IEEEReal.setRoundingMode mode
          val result = f x handle e => (IEEEReal.setRoundingMode previous; raise e)
      in IEEEReal.setRoundingMode previous; result
      end
  fun fromLarge mode x = withMode mode narrow x
  fun realFloor (x:real) : real = prim ("@floorf", x)
  fun realCeil (x:real) : real = prim ("@ceilf", x)
  fun realTrunc (x:real) : real = prim ("@truncf", x)
  fun realRound x = narrow (withMode IEEEReal.TO_NEAREST Real.realRound (toLarge x))
  fun floor x = Real.floor (toLarge x)
  fun ceil x = Real.ceil (toLarge x)
  fun trunc x = Real.trunc (toLarge x)
  fun round x = Real.round (toLarge x)
  fun toInt mode x = Real.toInt mode (toLarge x)
  fun toLargeInt mode x =
      let val whole = case mode of
                          IEEEReal.TO_NEAREST => realRound x
                        | IEEEReal.TO_NEGINF => realFloor x
                        | IEEEReal.TO_POSINF => realCeil x
                        | IEEEReal.TO_ZERO => realTrunc x
      in Real.toLargeInt IEEEReal.TO_ZERO (toLarge whole)
      end
  fun toManExp x =
      let val {man,exp} = Real.toManExp (toLarge x)
      in {man=narrow man,exp=exp}
      end
  fun ldexp (x:real,e:int) : real = prim ("@ldexpf", (x,e))
  fun fromManExp {man,exp} = ldexp(man,Int.max(~10000,Int.min(10000,exp)))
  fun split x =
      if isNan x then {whole=x,frac=x}
      else let val whole = realTrunc x
           in {whole=whole,frac=copySign(if isFinite x then x-whole else 0.0,x)}
           end
  fun realMod x = #frac (split x)
  val format = {precision=24,exponentBits=8}
  fun rounding mode =
      case mode of
          IEEEReal.TO_NEAREST => RealBits.NEAREST
        | IEEEReal.TO_NEGINF => RealBits.DOWN
        | IEEEReal.TO_POSINF => RealBits.UP
        | IEEEReal.TO_ZERO => RealBits.ZERO
  fun decimal mode s =
      fromBits (Word.fromLargeInt (RealBits.fromStringMode format (rounding mode) s))
  fun fromLargeInt x = decimal (IEEEReal.getRoundingMode()) (IntInf.toString x)
  fun fromDecimalMode mode {class,sign,digits,exp} =
      let fun signed x = if sign then ~x else x
          fun valid d = Int.<= (0,d) andalso Int.<= (d,9)
          fun digit d = String.str (Char.chr (Int.+ (48,d)))
      in case class of
             IEEEReal.NAN => SOME (signed (posInf-posInf))
           | IEEEReal.INF => SOME (signed posInf)
           | IEEEReal.ZERO => SOME (signed 0.0)
           | _ => if List.all valid digits then
                    SOME (decimal mode
                      ((if sign then "~" else "") ^ "0." ^ String.concat (List.map digit digits) ^
                       "e" ^ Int.toString exp))
                  else NONE
      end
  fun fromDecimal d = fromDecimalMode IEEEReal.TO_NEAREST d
  fun scan getc source =
      case IEEEReal.scan getc source of
          NONE => NONE
        | SOME (d,rest) => Option.map (fn r => (r,rest)) (fromDecimalMode (IEEEReal.getRoundingMode()) d)
  fun fromString s = StringCvt.scanString scan s
  fun toDecimal x =
      let val cls = class x
          val sign = signBit x
          fun shortest n =
              let val s = Real.fmt (StringCvt.SCI (SOME n)) (toLarge (abs x))
                  val d = valOf (IEEEReal.fromString s)
                  val candidate = valOf (fromDecimal d)
              in if Int.>= (n,8) orelse op == (candidate,abs x) then
                   let val {digits,exp,...} = d
                   in {class=cls,sign=sign,digits=digits,exp=exp}
                   end
                 else shortest (Int.+ (n,1))
              end
      in case cls of
             IEEEReal.NORMAL => withMode IEEEReal.TO_NEAREST shortest 0
           | IEEEReal.SUBNORMAL => withMode IEEEReal.TO_NEAREST shortest 0
           | _ => {class=cls,sign=sign,digits=[],exp=0}
      end
  fun fmt StringCvt.EXACT = IEEEReal.toString o toDecimal
    | fmt spec =
      let val format = Real.fmt spec
      in fn x => format (toLarge x)
      end
  val toString = fmt (StringCvt.GEN NONE)
end
