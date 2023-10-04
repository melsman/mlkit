(** Types and operations related to an IEEE implementation of reals.

The IEEEReal structure defines types associated with an IEEE
implementation of floating-point numbers. In addition, it provides
control for the floating-point hardware's rounding mode. Refer to the
IEEE standard 754-1985 and the ANSI/IEEE standard 854-1987 for
additional information.

*)

signature IEEE_REAL =
  sig
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

    val setRoundingMode : rounding_mode -> unit
    val getRoundingMode : unit -> rounding_mode

    type decimal_approx = {
      class : float_class,
      sign : bool,
      digits : int list,
      exp : int
    }

    val toString   : decimal_approx -> string
    val scan       : (char, 'a) StringCvt.reader -> (decimal_approx, 'a) StringCvt.reader
    val fromString : string -> decimal_approx option
  end

(**

[exception Unordered]

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
