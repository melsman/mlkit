(* Exact widening of a narrow value needs only a small binary64 adapter.
 * Keep the existing default Real implementation and formatting behavior. *)
structure Real =
struct
  open Real
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
      if isNan x then raise Domain
      else if not (isFinite x) then raise Overflow
      else valOf (IntInf.fromString (prim ("narrowIntegerString", rounded mode x)))
  fun frexp (x:real) : real * int = prim ("narrowWideFrexp", x)
  fun toManExp x =
      let val (man,exp) = frexp x
      in {man=man,exp=exp}
      end
end
