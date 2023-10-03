structure LargeReal =
struct
type real = real
end

(** Operations on floating point values.

The REAL signature specifies structures that implement floating-point
numbers. The semantics of floating-point numbers should follow the
IEEE standard 754-1985 and the ANSI/IEEE standard 854-1987. In
addition, implementations of the REAL signature are required to use
non-trapping semantics. Additional aspects of the design of the REAL
and MATH signatures were guided by the Floating-Point C Extensions
developed by the X3J11 ANSI committee and the lecture notes by
W. Kahan on the IEEE standard 754.

Although there can be many representations for NaN values, the Library
models them as a single value and currently provides no explicit way
to distinguish among them, ignoring the sign bit. Thus, in the
descriptions below and in the Math structure, we just refer to the NaN
value.
*)

signature REAL = sig
  type real

  structure Math : MATH

  val radix        : int
  val precision    : int
  val maxFinite    : real
  val minPos       : real
  val minNormalPos : real
  val posInf       : real
  val negInf       : real

  val +            : real * real -> real
  val -            : real * real -> real
  val *            : real * real -> real
  val /            : real * real -> real
  val rem          : real * real -> real
  val *+           : real * real * real -> real
  val *-           : real * real * real -> real
  val ~            : real -> real
  val abs          : real -> real

  val min          : real * real -> real
  val max          : real * real -> real

  val sign         : real -> int
  val signBit      : real -> bool

  val sameSign     : real * real -> bool
  val copySign     : real * real -> real

  val compare      : real * real -> order
  val compareReal  : real * real -> IEEEReal.real_order

  val <            : real * real -> bool
  val <=           : real * real -> bool
  val >            : real * real -> bool
  val >=           : real * real -> bool
  val ==           : real * real -> bool
  val !=           : real * real -> bool

  val ?=           : real * real -> bool
  val unordered    : real * real -> bool

  val isFinite     : real -> bool
  val isNan        : real -> bool
  val isNormal     : real -> bool
  val class        : real -> IEEEReal.float_class

  val toManExp     : real -> {man : real, exp : int}
  val fromManExp   : {man : real, exp : int} -> real
  val split        : real -> {whole : real, frac : real}
  val realMod      : real -> real

  val nextAfter    : real * real -> real
  val checkFloat   : real -> real

  val realFloor    : real -> real
  val realCeil     : real -> real
  val realTrunc    : real -> real
  val realRound    : real -> real
  val floor        : real -> int
  val ceil         : real -> int
  val trunc        : real -> int
  val round        : real -> int

  val toInt        : IEEEReal.rounding_mode -> real -> int
  val toLargeInt   : IEEEReal.rounding_mode -> real -> LargeInt.int
  val fromInt      : int -> real
  val fromLargeInt : LargeInt.int -> real

  val toLarge      : real -> LargeReal.real
  val fromLarge    : IEEEReal.rounding_mode -> LargeReal.real -> real

  val fmt          : StringCvt.realfmt -> real -> string
  val toString     : real -> string
  val scan         : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
  val fromString   : string -> real option

(*
  val toDecimal    : real -> IEEEReal.decimal_approx
  val fromDecimal  : IEEEReal.decimal_approx -> real option

  val toDefault    : real -> real
  val fromDefault  : real -> real
*)
end

(**

[type real] The type of reals. Notice that real is not an equality
type.

[structure Math] Mathematical operations on real values.

[radix] The base of the representation, e.g., 2 or 10 for IEEE
floating point.

[precision] The number of digits, each between 0 and radix-1, in the
mantissa. Note that the precision includes the implicit (or hidden)
bit used in the IEEE representation (e.g., the value of
Real64.precision is 53).

[maxFinite]

[minPos]

[minNormalPos]

The maximum finite number, the minimum non-zero positive number, and
the minimum non-zero normalized number, respectively.

[posInf]

[negInf]

Positive and negative infinity values.

[x + y] returns the sum of x and y.

[x - y] returns the difference between x and y.

[x * y] returns the product of x and y.

[x / y] returns the division of x by y.

[rem (x, y)] returns the remainder x - n*y, where n = trunc (x /
y). The result has the same sign as x and has absolute value less than
the absolute value of y.  If x is an infinity or y is 0, rem returns
NaN. If y is an infinity, rem returns x.

[*+ (a, b, c)]
[*- (a, b, c)]

These return a*b + c and a*b - c, respectively. Their behaviors on
infinities follow from the behaviors derived from addition,
subtraction, and multiplication.  The precise semantics of these
operations depend on the language implementation and the underlying
hardware. Specifically, certain architectures provide these operations
as a single instruction, possibly using a single rounding
operation. Thus, the use of these operations may be faster than
performing the individual arithmetic operations sequentially, but may
also cause different rounding behavior.

[~ x] returns the negation of x.

[abs r] returns the absolute value |r| of r.

[min(x, y)] is the smaller of x and y.

[max(x, y)] is the larger of x and y.

[sign x] is ~1, 0, or 1, according to whether x is negative, zero, or
positive. An infinity returns its sign; a zero returns 0 regardless of
its sign. It raises Domain on NaN.

[signBit r] returns true if and only if the sign of r (infinities,
zeros, and NaN, included) is negative.

[sameSign (x, y)] is true iff sign x = sign y.

[copySign (x, y)] returns x with the sign of y, even if y is NaN.

[compare (x, y)]

[compareReal (x, y)]

The function compare returns LESS, EQUAL, or GREATER according to
whether its first argument is less than, equal to, or greater than the
second. It raises IEEEReal.Unordered on unordered arguments.  The
function compareReal behaves similarly except that the values it
returns have the extended type IEEEReal.real_order and it returns
IEEEReal.UNORDERED on unordered arguments.

[x < y] returns true if x is strictly smaller than y. Returns false
otherwise.

[x <= y] returns true if x is smaller than or equal to y. Returns
false otherwise.

[x > y] returns true if x is strictly larger than y. Returns false
otherwise.

[x >= y] returns true if x is larger than or equal to y. Returns false
otherwise.

Note that these operators return false on unordered arguments, i.e.,
if either argument is NaN, so that the usual reversal of comparison
under negation does not hold, e.g., a < b is not the same as not (a >=
b).

[== (x, y)]
[!= (x, y)]

The first returns true if and only if neither y nor x is NaN, and y
and x are equal, ignoring signs on zeros. This is equivalent to the
IEEE = operator.  The second function != is equivalent to not o op ==
and the IEEE ?<> operator.

[?= (x, y)] returns true if either argument is NaN or if the arguments
are bitwise equal, ignoring signs on zeros. It is equivalent to the
IEEE ?= operator.

[unordered (x, y)] returns true if x and y are unordered, i.e., at
least one of x and y is NaN.

[isFinite x] returns true if x is neither NaN nor an infinity.

[isNan x] returns true if x is NaN.

[isNormal x] returns true if x is normal, i.e., neither zero,
subnormal, infinite nor NaN.

[class x] returns the IEEEReal.float_class to which x belongs.

[toManExp r] returns {man, exp}, where man and exp are the mantissa
and exponent of r, respectively. Specifically, we have the relation

    r = man * radix^(exp)

where 1.0 <= man * radix < radix. This function is comparable to frexp
in the C library.  If r is +-0, man is +-0 and exp is +0. If r is
+-infinity, man is +-infinity and exp is unspecified. If r is NaN, man
is NaN and exp is unspecified.

[fromManExp {man, exp}] returns man * radix^(exp). This function is
comparable to ldexp in the C library. Note that, even if man is a
non-zero, finite real value, the result of fromManExp can be zero or
infinity because of underflows and overflows.  If man is +-0, the
result is +-0. If man is +-infinity, the result is +-infinity. If man
is NaN, the result is NaN.

[split r]

[realMod r]

The former returns {whole, frac}, where frac and whole are the
fractional and integral parts of r, respectively. Specifically, whole
is integral, |frac| < 1.0, whole and frac have the same sign as r, and
r = whole + frac. This function is comparable to modf in the C
library.  If r is +-infinity, whole is +-infinity and frac is +-0. If
r is NaN, both whole and frac are NaN.  The realMod function is
equivalent to #frac o split.

[nextAfter (r, t)] returns the next representable real after r in the
direction of t. Thus, if t is less than r, nextAfter returns the
largest representable floating-point number less than r. If r = t then
it returns r. If either argument is NaN, this returns NaN. If r is
+-infinity, it returns +-infinity.

[checkFloat x] raises Overflow if x is an infinity, and raises Div if
x is NaN. Otherwise, it returns its argument.  This can be used to
synthesize trapping arithmetic from the non-trapping operations given
here. Note, however, that infinities can be converted to NaNs by some
operations, so that if accurate exceptions are required, checks must
be done after each operation.

[realFloor r]

[realCeil r]

[realTrunc r]

[realRound r]

These functions convert real values to integer-valued reals. realFloor
produces floor(r), the largest integer not larger than r. realCeil
produces ceil(r), the smallest integer not less than r. realTrunc
rounds r towards zero, and realRound rounds to the integer-values real
value that is nearest to r. If r is NaN or an infinity, these
functions return r.

[floor r] is the largest integer <= r (rounds towards minus infinity).
May raise Overflow.

[ceil r] is the smallest integer >= r (rounds towards plus infinity).
May raise Overflow.

[trunc r] is numerically largest integer between r and zero (rounds
towards zero). May raise Overflow.

[round r] is the integer nearest to r, using the default rounding
mode.  NOTE: This isn't the required behaviour: it should round to
nearest even integer in case of a tie.  May raise Overflow.

[toInt mode x]

[toLargeInt mode x]

These functions convert the argument x to an integral type using the
specified rounding mode. They raise Overflow if the result is not
representable, in particular, if x is an infinity. They raise Domain
if the input real is NaN.

[fromInt i]

[fromLargeInt i]

These functions convert the integer i to a real value. If the absolute
value of i is larger than maxFinite, then the appropriate infinity is
returned. If i cannot be exactly represented as a real value, then the
current rounding mode is used to determine the resulting value. The
top-level function real is an alias for Real.fromInt.

[toLarge r]

[fromLarge r]

These convert between values of type real and type LargeReal.real. If
r is too small or too large to be represented as a real, fromLarge
will convert it to a zero or an infinity.

[fmt spec r] returns a string representing r, in the format specified
by spec.

      spec          description                            C printf
      ---------------------------------------------------------------
      SCI NONE      scientific,   6 digits after point       %e
      SCI (SOME n)  scientific,   n digits after point       %.ne
      FIX NONE      fixed-point,  6 digits after point       %f
      FIX (SOME n)  fixed-point,  n digits after point       %.nf
      GEN NONE      auto choice, 12 significant digits       %.12g
      GEN (SOME n)  auto choice,  n significant digits       %.ng

[toString r] returns a string representing r, with automatic choice of
format according to the magnitude of r. Equivalent to (fmt (GEN NONE)
r).

[scan getc charsrc] attempts to scan a floating-point number from the
character source charsrc, using the accessor getc, and ignoring any
initial whitespace.  If successful, it returns SOME(r, rest) where r
is the number scanned, and rest is the unused part of the character
source. The valid forms of floating-point numerals are described by

	[+~-]?(([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+~-]?[0-9]+)?

[fromString s] returns SOME(r) if a floating-point numeral can be
scanned from a prefix of string s, ignoring any initial whitespace;
returns NONE otherwise.  The valid forms of floating-point numerals
are described by

	[+~-]?(([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+~-]?[0-9]+)?

[toDecimal r]

[fromDecimal d]

These convert between real values and decimal approximations. Decimal
approximations are to be converted using the IEEEReal.TO_NEAREST
rounding mode. toDecimal should produce only as many digits as are
necessary for fromDecimal to convert back to the same number. In
particular, for any normal or subnormal real value r, we have the
bit-wise equality: fromDecimal (toDecimal r) = r.  For toDecimal, when
the r is not normal or subnormal, then the exp field is set to 0 and
the digits field is the empty list. In all cases, the sign and class
field capture the sign and class of r.

For fromDecimal, if class is ZERO or INF, the resulting real is the
appropriate signed zero or infinity. If class is NAN, a signed NaN is
generated. If class is NORMAL or SUBNORMAL, the sign, digits and exp
fields are used to produce a real number whose value is

    s * 0.d(1)d(2)...d(n) 10^(exp)

where digits = [d(1), d(2), ..., d(n)] and where s is -1 if sign is
true and 1 otherwise. Note that the conversion itself should ignore
the class field, so that the resulting value might have class NORMAL,
SUBNORMAL, ZERO, or INF. For example, if digits is empty or a list of
all 0's, the result should be a signed zero. More generally, very
large or small magnitudes are converted to infinities or zeros.  If
the argument to fromDecimal does not have a valid format, i.e., if the
digits field contains integers outside the range [0,9], it returns
NONE.

    Implementation note: Algorithms for accurately and efficiently
    converting between binary and decimal real representations are
    readily available, e.g., see the technical report by Gay.

*)
