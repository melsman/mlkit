(** SigDoc *)
structure Real : REAL =
struct
  open Real
  val radix = 2
  val precision = 53
  val maxFinite : real = 1.7976931348623157e308
  val minPos : real = 4.9406564584124654e~324
  val minNormalPos : real = 2.2250738585072014e~308
  fun signBit (x:real) : bool = prim ("execStmtJS", ("var v=new DataView(new ArrayBuffer(8));v.setFloat64(0,x);return (v.getUint32(0) >>> 31) !== 0;","x",x))
  fun sameSign (x,y) = signBit x = signBit y
  fun copySign (x,y) = if signBit y then ~(abs x) else abs x
  fun op == (x:real,y:real) : bool = prim ("execStmtJS", ("return x === y;","x,y",x,y))
  fun op != (x,y) = not (op == (x,y))
  fun unordered (x,y) = isNan x orelse isNan y
  fun op ?= (x,y) = unordered (x,y) orelse op == (x,y)
  fun isFinite x = not (isNan x) andalso not (op == (abs x,posInf))
  fun isNormal x = isFinite x andalso abs x >= minNormalPos
  fun class x =
      if isNan x then IEEEReal.NAN
      else if not (isFinite x) then IEEEReal.INF
      else if op == (x,0.0) then IEEEReal.ZERO
      else if isNormal x then IEEEReal.NORMAL else IEEEReal.SUBNORMAL
  fun sign x = if isNan x then raise Domain else if x > 0.0 then 1 else if x < 0.0 then ~1 else 0
  fun compareReal (x,y) =
      if unordered (x,y) then IEEEReal.UNORDERED
      else if x < y then IEEEReal.LESS else if x > y then IEEEReal.GREATER else IEEEReal.EQUAL
  fun compare (x,y) =
      if unordered (x,y) then raise IEEEReal.Unordered
      else if x < y then LESS else if x > y then GREATER else EQUAL
  fun min (x,y) = if isNan y orelse x < y then x else y
  fun max (x,y) = if isNan y orelse x > y then x else y
  fun op *+ (x,y,z) = x*y+z
  fun op *- (x,y,z) = x*y-z
  fun checkFloat x =
      if isNan x then raise Div else if not (isFinite x) then raise Overflow else x
  fun realFloor (x:real) : real = prim ("floorFloatWide", x)
  fun realCeil (x:real) : real = prim ("ceilFloatWide", x)
  fun realTrunc (x:real) : real = prim ("truncFloatWide", x)
  fun realRound (x:real) : real = prim ("narrowWideRound", x)
  fun floor x = if isNan x then raise Domain else Real.floor x
  fun ceil x = if isNan x then raise Domain else Real.ceil x
  fun trunc x = if isNan x then raise Domain else Real.trunc x
  fun round x = trunc (realRound x)
  fun rounded mode x =
      case mode of
          IEEEReal.TO_NEAREST => realRound x
        | IEEEReal.TO_NEGINF => realFloor x
        | IEEEReal.TO_POSINF => realCeil x
        | IEEEReal.TO_ZERO => realTrunc x
  fun toInt mode x = if isNan x then raise Domain else trunc (rounded mode x)
  fun toLargeInt mode x =
      if isNan x then raise Domain else if not (isFinite x) then raise Overflow
      else
        let val n = rounded mode x
            (* Decode the exact integer significand; no host decimal rounding or
             * JavaScript BigInt is needed for the default real interface. *)
            val (hi,lo,e) : word * word * int =
                prim ("execStmtJS", ("var v=new DataView(new ArrayBuffer(8));v.setFloat64(0,n);var h=v.getUint32(0),e=(h>>>20)&2047;return [((h&1048575)|(e?1048576:0))>>>0,v.getUint32(4),e?e-1075:-1074];","n",n))
            val m = IntInf.+ (IntInf.<< (Word.toLargeInt hi,0w32),Word.toLargeInt lo)
            val z = if Int.>= (e,0) then IntInf.<< (m,Word.fromInt e)
                    else IntInf.~>> (m,Word.fromInt (Int.~ e))
        in if n < 0.0 then IntInf.~ z else z
        end
  fun number (s:string) : real = prim ("execStmtJS", ("return Number(s.replace(/~/g,'-'));","s",s))
  fun fromLargeInt x = number (IntInf.toString x)
  fun toLarge x = x
  fun fromLarge _ x = x
  fun toManExp x =
      let val (man,exp) : real * int = prim ("narrowWideFrexp", x)
      in {man=man,exp=exp}
      end
  fun fromManExp {man:real,exp:int} : real =
      prim ("execStmtJS", ("return SmlPrims.real64Ldexp(x,e);","x,e",man,exp))
  fun nextAfter (x:real,y:real) : real =
      prim ("execStmtJS", ("return SmlPrims.real64NextAfter(x,y);","x,y",x,y))
  fun split x =
      let val whole = realTrunc x
          val frac = if isFinite x then copySign (x-whole,x) else if isNan x then x else copySign (0.0,x)
      in {whole=whole,frac=frac}
      end
  fun realMod x = #frac (split x)
  fun fromDecimal ({class,sign,digits,exp}:IEEEReal.decimal_approx) =
      let val prefix = if sign then "~" else ""
      in case class of
             IEEEReal.INF => SOME (if sign then negInf else posInf)
           | IEEEReal.NAN => SOME (copySign (posInf-posInf,if sign then ~1.0 else 1.0))
           | IEEEReal.ZERO => SOME (if sign then ~0.0 else 0.0)
           | _ =>
             if List.all (fn d => Int.>= (d,0) andalso Int.<= (d,9)) digits then
               SOME (number (prefix ^ "0." ^ String.concat (List.map Int.toString digits) ^ "e" ^ Int.toString exp))
             else NONE
      end
  fun scan getc source =
      case IEEEReal.scan getc source of
          NONE => NONE
        | SOME (d,rest) => Option.map (fn x => (x,rest)) (fromDecimal d)
  fun fromString s = StringCvt.scanString scan s
  fun toDecimal x =
      let val cls = class x
          val sign = signBit x
      in case cls of
             IEEEReal.NORMAL => finiteDecimal (cls,sign,x)
           | IEEEReal.SUBNORMAL => finiteDecimal (cls,sign,x)
           | _ => {class=cls,sign=sign,digits=[],exp=0}
      end
  and finiteDecimal (cls,sign,x:real) =
      let val s : string = prim ("execStmtJS", ("return Math.abs(x).toString();","x",x))
          val {digits,exp,...} = valOf (IEEEReal.fromString s)
      in {class=cls,sign=sign,digits=digits,exp=exp}
      end
  fun fmt StringCvt.EXACT = IEEEReal.toString o toDecimal
    | fmt spec = Real.fmt spec
  val toString = fmt (StringCvt.GEN NONE)
end
