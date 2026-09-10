(* Time -- new basis 1995-02-25, 1995-05-12 *)

(** SigDoc *)
structure Time :> TIME =
  struct
    fun getrealtime () : {sec : int, usec : int} =
      prim("sml_getrealtime", ())

    (* The runtime adds this to the seconds of the current time; it must
       agree with TIMEBASE in Runtime/Time.c. *)
    val timebase = Initial.timebase

    (* A time is sec + usec/1000000 seconds, with 0 <= usec < 1000000 and
       sec of either sign: ~1.5 seconds is {sec = ~2, usec = 500000}.
       Absolute times are intervals since UTC 00:00 on 1 Jan 1970. *)
    type time = {sec : int, usec : int}

    exception Time

    val zeroTime = {sec = 0, usec = 0}

    fun now () =
        let val {sec, usec} = getrealtime ()
        in {sec = sec - timebase, usec = usec}
        end

    val million = 1000000
    val millionL = IntInf.fromInt million

    (* Conversion to and from a number of microseconds; a time that does
       not fit the representation raises Time. *)
    fun fromMicro (us : IntInf.int) : time =
        {sec = LargeInt.toInt (IntInf.div (us, millionL)),
         usec = LargeInt.toInt (IntInf.mod (us, millionL))}
        handle Overflow => raise Time

    fun toMicro ({sec, usec} : time) : IntInf.int =
        IntInf.+ (IntInf.* (LargeInt.fromInt sec, millionL), LargeInt.fromInt usec)

    fun fromSeconds s = fromMicro (IntInf.* (s, millionL))
    fun fromMilliseconds ms = fromMicro (IntInf.* (ms, IntInf.fromInt 1000))
    fun fromMicroseconds us = fromMicro us
    fun fromNanoseconds ns = fromMicro (IntInf.quot (ns, IntInf.fromInt 1000))

    (* "fractions of the time unit are dropped, i.e., the values are
       rounded towards 0" *)
    fun toSeconds t = IntInf.quot (toMicro t, millionL)
    fun toMilliseconds t = IntInf.quot (toMicro t, IntInf.fromInt 1000)
    fun toMicroseconds t = toMicro t
    fun toNanoseconds t = IntInf.* (toMicro t, IntInf.fromInt 1000)

    fun fromReal r =
        if Real.isNan r then raise Time
        else fromMicro (Int.toLarge (Real.round (r * 1000000.0)))
             handle Overflow => raise Time

    fun toReal {sec, usec} =
        real sec + real usec / 1000000.0

    fun pow10 0 = 1
      | pow10 n = 10 * pow10 (n-1)

    (* fmt n t writes t with n decimal digits, rounded to nearest with
       ties to even, as Real.fmt does; the microseconds are exact, so
       the digits are computed from them rather than from toReal. *)
    fun fmt n t =
        if n < 0 then raise Size
        else
          let val us = toMicro t
              val neg = IntInf.< (us, 0)
              val us = IntInf.abs us
              val k = if n > 6 then 6 else n
              val scale = IntInf.fromInt (pow10 (6 - k))
              val q = IntInf.div (us, scale)
              val r2 = IntInf.* (IntInf.mod (us, scale), IntInf.fromInt 2)
              val rounded =
                  if IntInf.> (r2, scale)
                     orelse (r2 = scale andalso IntInf.mod (q, IntInf.fromInt 2) = IntInf.fromInt 1)
                  then IntInf.+ (q, IntInf.fromInt 1)
                  else q
              val unit = IntInf.fromInt (pow10 k)
              val whole = IntInf.toString (IntInf.div (rounded, unit))
              val frac = IntInf.toString (IntInf.mod (rounded, unit))
              fun zeros 0 = ""
                | zeros i = "0" ^ zeros (i-1)
          in (if neg then "~" else "")
             ^ whole
             ^ (if n = 0 then ""
                else "." ^ StringCvt.padLeft #"0" k frac ^ zeros (n - k))
          end

    fun toString t = fmt 3 t

    (* scan reads an optional sign (+, - or ~), then digits with an
       optional point and fraction, or a point and a fraction; the
       fraction is rounded to microseconds *)
    fun scan getc source =
        let fun isDigit c = #"0" <= c andalso c <= #"9"
            fun digits src acc =
                case getc src of
                    SOME (c, rest) => if isDigit c then digits rest (acc ^ String.str c)
                                      else (acc, src)
                  | NONE => (acc, src)
            fun value "" = IntInf.fromInt 0
              | value s = case IntInf.fromString s of SOME v => v
                                                     | NONE => raise Time
            val src = StringCvt.skipWS getc source
            val (neg, src1) =
                case getc src of
                    SOME(#"+", rest) => (false, rest)
                  | SOME(#"-", rest) => (true, rest)
                  | SOME(#"~", rest) => (true, rest)
                  | _ => (false, src)
            val (ip, src2) = digits src1 ""
            val (fp, src3) = case getc src2 of
                                 SOME(#".", rest) =>
                                   let val (fp, src3) = digits rest ""
                                   in (SOME fp, src3) end
                               | _ => (NONE, src2)
        in
          if ip = "" andalso (case fp of NONE => true | SOME fp => fp = "") then NONE
          else
            let val fp = case fp of NONE => "" | SOME fp => fp
                (* seven digits of the fraction: six microsecond digits and
                   one to round by *)
                val fp7 = if size fp >= 7 then String.substring (fp, 0, 7)
                          else StringCvt.padRight #"0" 7 fp
                val micro = IntInf.div (IntInf.+ (value fp7, IntInf.fromInt 5), IntInf.fromInt 10)
                val us = IntInf.+ (IntInf.* (value ip, millionL), micro)
            in SOME (fromMicro (if neg then IntInf.~ us else us), src3)
            end
        end

    fun fromString s = StringCvt.scanString scan s

    val op + = fn ({sec=sec1, usec=usec1} : time, {sec=sec2, usec=usec2}) =>
        let val usecs = usec1 + usec2
        in if usecs >= million then {sec = sec1 + sec2 + 1, usec = usecs - million}
           else {sec = sec1 + sec2, usec = usecs}
        end handle Overflow => raise Time

    and op - = fn ({sec=sec1, usec=usec1} : time, {sec=sec2, usec=usec2}) =>
        let val usecs = usec1 - usec2
        in if usecs < 0 then {sec = sec1 - sec2 - 1, usec = usecs + million}
           else {sec = sec1 - sec2, usec = usecs}
        end handle Overflow => raise Time

    val op <  = fn ({sec=sec1, usec=usec1} : time, {sec=sec2, usec=usec2}) =>
        (sec1 < sec2) orelse (sec1=sec2 andalso usec1 < usec2)
    and op <= = fn ({sec=sec1, usec=usec1} : time, {sec=sec2, usec=usec2}) =>
        (sec1 < sec2) orelse (sec1=sec2 andalso usec1 <= usec2)
    and op >  = fn ({sec=sec1, usec=usec1} : time, {sec=sec2, usec=usec2}) =>
        (sec1 > sec2) orelse (sec1=sec2 andalso usec1 > usec2)
    and op >= = fn ({sec=sec1, usec=usec1} : time, {sec=sec2, usec=usec2}) =>
        (sec1 > sec2) orelse (sec1=sec2 andalso usec1 >= usec2)

    fun compare (x, y: time) =
        if x<y then LESS else if x>y then GREATER else EQUAL

    fun toPair x = x
  end
