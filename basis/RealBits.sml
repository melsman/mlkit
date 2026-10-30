(* Convert decimal literals directly to IEEE binary formats. Using a host real
 * here would round twice near a midpoint in the destination format. *)
structure RealBits =
struct
  datatype rounding = NEAREST | DOWN | UP | ZERO
  fun pow2 n = IntInf.<< (1, Word.fromInt n)

  fun fromStringMode {precision, exponentBits} mode s =
    let
      val negative = String.sub (s, 0) = #"~"
      val start = if negative then 1 else 0
      val length = String.size s
      fun mantissa (i, n, fractional, afterPoint, digits) =
          if i = length then (n, fractional, digits, 0 : IntInf.int)
          else
            case String.sub (s, i) of
                #"." => mantissa (i+1, n, fractional, true, digits)
              | #"e" => exponent (i+1, n, fractional, digits)
              | #"E" => exponent (i+1, n, fractional, digits)
              | c => mantissa (i+1, n*10 + IntInf.fromInt (Char.ord c - Char.ord #"0"),
                               fractional + (if afterPoint then 1 else 0), afterPoint,
                               if n = 0 andalso c = #"0" then 0 else digits+1)
      and exponent (i, n, fractional, digits) =
          (n, fractional, digits,
           valOf (IntInf.fromString (String.extract (s, i, NONE))))
      val (n, fractional, digits, decimalExponent) = mantissa (start, 0, 0, false, 0)
      val decimalExponent = decimalExponent - IntInf.fromInt fractional
      val order = IntInf.fromInt digits + decimalExponent
      val hidden = pow2 (precision-1)
      val bias = IntInf.toInt (pow2 (exponentBits-1)) - 1
      val minExponent = 1-bias
      val infinity = (pow2 exponentBits - 1) * hidden
      val sign = if negative then pow2 (precision + exponentBits - 1) else 0
      val away = (mode = DOWN andalso negative) orelse
                 (mode = UP andalso not negative)
      val overflow = if mode = NEAREST orelse away then infinity else infinity-1
      fun round (numerator, denominator) =
          let
            val (q, r) = IntInf.divMod (numerator, denominator)
          in
            if (away andalso r <> 0) orelse
               (mode = NEAREST andalso
                (2*r > denominator orelse
                 (2*r = denominator andalso IntInf.mod (q, 2) = 1)))
            then q+1 else q
          end
      fun encode () =
          let
            val k = IntInf.toInt decimalExponent
            val (numerator, denominator) =
                if k >= 0 then (n * IntInf.pow (10, k), 1)
                else (n, IntInf.pow (10, ~k))
            fun exponent (n, d, e) =
                if n < d then exponent (2*n, d, e-1)
                else if n >= 2*d then exponent (n, 2*d, e+1)
                else e
            val e = exponent (numerator, denominator, 0)
            val unitExponent = Int.max (e, minExponent) - (precision-1)
            val significand =
                if unitExponent >= 0 then round (numerator, denominator * pow2 unitExponent)
                else round (numerator * pow2 (~unitExponent), denominator)
            val bits = IntInf.fromInt (Int.max (0, e-minExponent)) * hidden + significand
          in
            if bits >= infinity then overflow else bits
          end
      (* Conservative decimal bounds also avoid constructing powers for enormous
       * exponents. The boundary cases still take the exact rational path. *)
      val bits =
          if n = 0 then 0
          else if order > IntInf.fromInt bias then overflow
          else if order < IntInf.fromInt (minExponent-precision) then (if away then 1 else 0)
          else encode ()
    in
      sign + bits
    end

  fun fromString format s = fromStringMode format NEAREST s
end
