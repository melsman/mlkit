(* Much of this implementation is taken from the MLton implementation
of the basis library. *)

structure IEEEReal : IEEE_REAL =
struct

exception Unordered

datatype real_order = LESS | EQUAL | GREATER | UNORDERED

datatype float_class
  = NAN
  | INF
  | ZERO
  | NORMAL
  | SUBNORMAL

datatype rounding_mode
  = TO_NEAREST
  | TO_NEGINF
  | TO_POSINF
  | TO_ZERO

fun int_of_rm rm =
    case rm of
        TO_NEAREST => 0
      | TO_NEGINF => 1
      | TO_POSINF => 2
      | TO_ZERO => 3

fun rm_of_int i =
    case i of
        0 => TO_NEAREST
      | 1 => TO_NEGINF
      | 2 => TO_POSINF
      | 3 => TO_ZERO
      | _ => raise Fail ("IEEEReal.rm_of_int: " ^ Int.toString i)

fun setRoundingMode (rm : rounding_mode) : unit =
    prim("floatSetRoundingMode", int_of_rm rm)

fun getRoundingMode () : rounding_mode =
    rm_of_int (prim("floatGetRoundingMode", ()))

type decimal_approx = {
  class : float_class,
  sign : bool,
  digits : int list,
  exp : int
}

structure DecimalApprox =
  struct
  type t = {class: float_class,
            digits: int list,
            exp: int,
            sign: bool}

  val inf: t = {class = INF,
                digits = [],
                exp = 0,
                sign = false}

  val zero: t = {class = ZERO,
                 digits = [],
                 exp = 0,
                 sign = false}
  end

type decimal_approx = DecimalApprox.t

fun 'a scan reader (state: 'a) =
    let
      val state = StringCvt.skipWS reader state
      fun readc (c, state, f) =
          case reader state of
              NONE => NONE
            | SOME (c', state') =>
              if c = Char.toLower c'
              then f state'
              else NONE
      fun readString (s, state, failure, success) =
          let
            val n = String.size s
            fun loop (i, state) =
                if i = n
                then success state
                else
                  case reader state of
                      NONE => failure ()
                    | SOME (c, state) =>
                      if Char.toLower c = String.sub (s, i)
                      then loop (i + 1, state)
                      else failure ()
          in
            loop (0, state)
          end
      fun charToDigit c = Char.ord c - Char.ord #"0"
      fun digitStar (ds: int list, state) =
          let
            fun done () = (rev ds, state)
          in
            case reader state of
                NONE => done ()
              | SOME (c, state) =>
                if Char.isDigit c
                then digitStar (charToDigit c :: ds, state)
                else done ()
          end
      fun digitPlus (state, failure, success) =
          case reader state of
              NONE => failure ()
            | SOME (c, state) =>
              if Char.isDigit c
              then success (digitStar ([charToDigit c], state))
              else failure ()
      (* [+~-]?[0-9]+ *)
      type exp = {digits: int list, negate: bool}
      fun 'b afterE (state: 'a,
                     failure: unit -> 'b,
                     success: exp * 'a -> 'b) : 'b =
          case reader state of
              NONE => failure ()
            | SOME (c, state) =>
              let
                fun neg () =
                    digitPlus (state, failure,
                               fn (ds, state) =>
                                  success ({digits = ds, negate = true},
                                           state))
              in
                case c of
                    #"+" => digitPlus (state, failure,
                                       fn (ds, state) =>
                                          success ({digits = ds,
                                                    negate = false},
                                                   state))
                  | #"~" => neg ()
                  | #"-" => neg ()
                  | _ =>
                    if Char.isDigit c
                    then
                      let
                        val (ds, state) =
                            digitStar ([charToDigit c], state)
                      in
                        success ({digits = ds, negate = false},
                                 state)
                      end
                    else failure ()
              end
      (* e[+~-]?[0-9]+)? *)
      fun exp (state: 'a, failure, success) =
          case reader state of
              NONE => failure ()
            | SOME (c, state) =>
              case Char.toLower c of
                  #"e" => afterE (state, failure, success)
                | _ => failure ()
      (* (\.[0-9]+)(e[+~-]?[0-9]+)? *)
      fun 'b afterDot (state: 'a,
                       failure: unit -> 'b,
                       success: int list * exp * 'a -> 'b) =
          digitPlus (state, failure,
                     fn (frac, state) =>
                        exp (state,
                             fn () => success (frac,
                                               {digits = [], negate = false},
                                               state),
                             fn (e, state) => success (frac, e, state)))
      fun stripLeadingZeros (ds: int list): int * int list =
          let
            fun loop (i, ds) =
                case ds of
                    [] => (i, [])
                  | d :: ds' =>
                    if d = 0
                    then loop (i + 1, ds')
                    else (i, ds)
          in
            loop (0, ds)
          end
      fun stripTrailingZeros ds =
          case ds of
              [] => []
            | _ =>
              case List.last ds of
                  0 => rev (#2 (stripLeadingZeros (rev ds)))
                | _ => ds
      fun done (whole: int list,
                frac: int list,
                {digits: int list, negate: bool},
                state: 'a) =
          let
            val (_, il) = stripLeadingZeros whole
            val fl = stripTrailingZeros frac
            datatype exp =
                     Int of int
                     | Overflow of DecimalApprox.t
            val exp =
                case (SOME (let
                             val i =
                                 List.foldl (fn (d, n) => n * 10 + d)
                                            0 digits
                           in
                             if negate then Int.~ i else i
                           end)
                      handle General.Overflow => NONE) of
                    NONE => Overflow (if negate
                                      then DecimalApprox.zero
                                      else DecimalApprox.inf)
                  | SOME i => Int i
            val da =
                case il of
                    [] =>
                    (case fl of
                         [] => DecimalApprox.zero
                       | _ =>
                         case exp of
                             Int e =>
                             let
                               val (m, fl) = stripLeadingZeros fl
                             in
                               {class = NORMAL,
                                digits = fl,
                                exp = e - m,
                                sign = false}
                             end
                           | Overflow da => da)
                  | _ =>
                    case exp of
                        Int e =>
                        {class = NORMAL,
                         digits = stripTrailingZeros (il @ fl),
                         exp = e + length il,
                         sign = false}
                      | Overflow da => da
          in
            SOME (da, state)
          end
      fun normal' (c, state) =
          case Char.toLower c of
              #"i" => readc (#"n", state, fn state =>
                                             readc (#"f", state, fn state =>
                                                                    let
                                                                      fun res state =
                                                                          SOME ({class = INF,
                                                                                 digits = [],
                                                                                 exp = 0,
                                                                                 sign = false},
                                                                                state)
                                                                    in
                                                                      readString ("inity", state,
                                                                                  fn () => res state,
                                                                                  res)
                                                                    end))
            | #"n" => readc (#"a", state, fn state =>
                                             readc (#"n", state, fn state =>
                                                                    SOME ({class = NAN,
                                                                           digits = [],
                                                                           exp = 0,
                                                                           sign = false},
                                                                          state)))
            (* (([0-9]+(\.[0-9]+)?)|(\.[0-9]+))(e[+~-]?[0-9]+)? *)
            | #"." => afterDot (state,
                                fn () => NONE,
                                fn (frac, exp, state) =>
                                   done ([], frac, exp, state))
            | _ =>
              if Char.isDigit c
              then
                (* ([0-9]+(\.[0-9]+)?)(e[+~-]?[0-9]+)? *)
                let
                  val (whole, state) =
                      digitStar ([charToDigit c], state)
                  fun no () = done (whole, [],
                                    {digits = [], negate = false},
                                    state)
                in
                  case reader state of
                      NONE => no ()
                    | SOME (c, state) =>
                      case Char.toLower c of
                          #"." =>
                          afterDot (state, no,
                                    fn (frac, e, state) =>
                                       done (whole, frac, e, state))
                        | #"e" =>
                          afterE (state, no,
                                  fn (e, state) =>
                                     done (whole, [], e, state))
                        | _ => no ()
                end
              else NONE
      fun normal state =
          case reader state of
              NONE => NONE
            | SOME z => normal' z
      fun negate state =
               case normal state of
                   NONE => NONE
                 | SOME ({class, digits, exp, ...}, state) =>
                   SOME ({class = class,
                          digits = digits,
                          exp = exp,
                          sign = true},
                         state)
    in
      case reader state of
          NONE => NONE
        | SOME (c, state) =>
          case c of
              #"~" => negate state
            | #"-" => negate state
            | #"+" => normal state
            | _ => normal' (c, state)
    end

fun fromString s = StringCvt.scanString scan s

fun digitToChar (n: int): char = String.sub ("0123456789ABCDEF", n)

fun toString {class, sign, digits, exp}: string =
    let
      fun digitStr () = implode (map digitToChar digits)
      fun norm () =
          let val num = "0." ^ digitStr()
          in if exp = 0
             then num
             else concat [num, "E", Int.toString exp]
          end
      val num =
          case class of
              ZERO => "0.0"
            | NORMAL => norm ()
            | SUBNORMAL => norm ()
            | INF => "inf"
            | NAN => "nan"
    in if sign
       then "~" ^ num
       else num
    end

end

(**

[exception Unordered] Exception that may be raised by Real.compare.

[setRoundingMode m]
[getRoundingMode()]

These set and get the rounding mode of the underlying hardware. The
IEEE standard requires TO_NEAREST as the default rounding mode.

    Implementation note:

    Some platforms do not support all of the rounding modes. An SML
    implementation built on these platforms will necessarily be
    non-conforming with, presumably, setRoundingMode raising an
    exception for the unsupported modes.

[type decimal_approx] This type provides a structured decimal
representation of a real. The class field indicates the real class. If
sign is true, the number is negative. The integers in the digits list
must be digits, i.e., between 0 and 9.  When class is NORMAL or
SUBNORMAL, a value of type decimal_approx with digits = [d(1), d(2),
..., d(n)] corresponds to the real number s * 0.d(1)d(2)...d(n)
10(exp), where s is -1 if sign is true and 1 otherwise. When class is
ZERO or INF, the value corresponds to zero or infinity, respectively,
with its sign determined by sign. When class is NAN, the value
corresponds to an unspecified NaN value.

[toString d] returns a string representation of d. Assuming digits =
[d(1), d(2), ..., d(n)] and ignoring the sign and exp fields, toString
generates the following strings depending on the class field:

    ZERO        "0.0"
    NORMAL      "0.d(1)d(2)...d(n)"
    SUBNORMAL   "0.d(1)d(2)...d(n)"
    INF         "inf"
    NAN         "nan"

If the sign field is true, a #"~" is prepended. If the exp field is
non-zero and the class is NORMAL or SUBNORMAL, the string
"E"^(Integer.toString exp) is appended.  The composition toString o
REAL.toDecimal is equivalent to REAL.fmt StringCvt.EXACT.

[scan getc strm]
[fromString s]

These functions scan a decimal approximation from a prefix of a
character source. Initial whitespace is ignored. The first reads from
the character stream src using the character input function getc. It
returns SOME(d, rest) if the decimal approximation d can be parsed;
rest is the remainder of the character stream. NONE is returned
otherwise.  The second form uses the string s as input. It returns the
decimal approximation on success and NONE otherwise. The fromString
function is equivalent to StringCvt.scanString scan.

The functions accept real numbers with the following format:

    [+~-]?([0-9]+.[0-9]+? | .[0-9]+)(e | E)[+~-]?[0-9]+?

The optional sign determines the value of the sign field, with a
default of false. Initial zeros are stripped from the integer part and
trailing zeros are stripped from the fractional part, yielding two
lists il and fl, respectively, of digits. If il is non-empty, then
class is set to NORMAL, digits is set to il@fl with any trailing zeros
removed and exp is set to the length of il plus the value of the
scanned exponent, if any. If il is empty and so is fl, then class is
set to ZERO, digits = [] and exp = 0. Finally, if il is empty but fl
is not, let m be the number of leading zeros in fl and let fl' be fl
after the leading zeros are removed. Then, class is set to NORMAL,
digits is set to fl' and exp is set to -m plus the value of the
scanned exponent, if any.  They also accept the following string
representations of non-finite values:

    [+~-]?(inf | infinity | nan)

where the alphabetic characters are case-insensitive. The optional
sign determines the value of the sign field, with a default of
false. In the first and second cases, d will have class set to INF. In
the third case, class is set to NAN. In all these cases, d will have
digits = [] and exp = 0.

*)
