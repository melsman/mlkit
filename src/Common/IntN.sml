(* IntN(I): integers of precision I.precision, represented in I, which
   must be at least that wide (e.g. I = struct open Int64 val precision =
   SOME 63 end).  Operations are carried out in IntInf and their results
   checked against the precision, so that Overflow is raised exactly when
   the true result does not fit in the N-bit type, independently of the
   width of the host compiler's integers. *)

functor IntN(I : INTEGER) :> INTEGER =
  struct
    type int = I.int
    val precision = I.precision
    val bits = case precision of
                   SOME n => n
                 | NONE => raise Fail "IntN: precision must be finite"
    val two = IntInf.fromInt 2
    val minL = IntInf.~ (IntInf.pow (two, Int.-(bits,1)))
    val maxL = IntInf.- (IntInf.pow (two, Int.-(bits,1)), IntInf.fromInt 1)

    fun chk (i:IntInf.int) : IntInf.int =
        if IntInf.<= (minL, i) andalso IntInf.<= (i, maxL) then i
        else raise Overflow

    val fromLarge = I.fromLarge o chk
    val toLarge = I.toLarge
    val minInt = SOME (fromLarge minL)
    val maxInt = SOME (fromLarge maxL)
    val toInt = I.toInt
    val fromInt = fromLarge o Int.toLarge

    fun lift2 f (a,b) = fromLarge (f (toLarge a, toLarge b))
    fun lift1 f a = fromLarge (f (toLarge a))

    val op + = lift2 IntInf.+
    val op - = lift2 IntInf.-
    val op * = lift2 IntInf.*
    val op div = lift2 IntInf.div
    val op mod = lift2 IntInf.mod
    val op quot = lift2 IntInf.quot
    val op rem = lift2 IntInf.rem

    val compare = I.compare
    val op < = I.<
    val op <= = I.<=
    val op > = I.>
    val op >= = I.>=

    val ~ = lift1 IntInf.~
    val abs = lift1 IntInf.abs
    val min = I.min
    val max = I.max
    val sign = I.sign
    val sameSign = I.sameSign

    val fmt = I.fmt
    val toString = I.toString

    fun scan r gc s =
        Option.map (fn (i,r) => (fromLarge (I.toLarge i), r)) (I.scan r gc s)
    val fromString = Option.map (fromLarge o I.toLarge) o I.fromString
  end
