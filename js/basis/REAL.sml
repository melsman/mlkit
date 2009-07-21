(**
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
  type real = real

  structure Math : MATH

  val ~    : real -> real
  val +    : real * real -> real
  val -    : real * real -> real
  val *    : real * real -> real
  val /    : real * real -> real
  val abs  : real -> real
  val min  : real * real -> real
  val max  : real * real -> real
  val sign : real -> int
  val compare : real * real -> order

  val sameSign    : real * real -> bool
  val toDefault   : real -> real
  val fromDefault : real -> real
  val fromInt     : int -> real

  val floor : real -> int
  val ceil  : real -> int
  val trunc : real -> int
  val round : real -> int

  val isNan : real -> bool
  val isFinite : real -> bool
  val posInf : real
  val negInf : real

  val >    : real * real -> bool
  val >=   : real * real -> bool
  val <    : real * real -> bool
  val <=   : real * real -> bool
  val ==   : real * real -> bool
  val !=   : real * real -> bool

  val toString   : real -> string
  val fromString : string -> real option
  val scan       : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader
  val fmt        : StringCvt.realfmt -> real -> string
end

(**

[~ x] returns the negation of x.

[x * y] returns the product of x and y.

[x / y] returns the division of x by y.

[x + y] returns the sum of x and y.

[x - y] returns the difference between x and y.

[x > y] returns true if x is strictly larger than y. Returns false
otherwise.

[x >= y] returns true if x is larger than or equal to y. Returns false
otherwise.

[x < y] returns true if x is strictly smaller than y. Returns false
otherwise.

[x <= y] returns true if x is smaller than or equal to y. Returns
false otherwise.

[~ x] returns the absolute value of x.

[min(x, y)] is the smaller of x and y.

[max(x, y)] is the larger of x and y.

[sign x] is ~1, 0, or 1, according to whether x is negative, zero, or
positive.

[compare(x, y)] returns LESS, EQUAL, or GREATER, according as x is
less than, equal to, or greater than y.

[sameSign(x, y)] is true iff sign x = sign y.

[toDefault x] is x.

[fromDefault x] is x.

[fromInt i] is the floating-point number representing integer i.

[floor r] is the largest integer <= r (rounds towards minus infinity).
May raise Overflow.

[ceil r] is the smallest integer >= r (rounds towards plus infinity).
May raise Overflow.

[trunc r] is numerically largest integer between r and zero (rounds
towards zero). May raise Overflow.

[round r] is the integer nearest to r, using the default rounding
mode.  NOTE: This isn't the required behaviour: it should round to
nearest even integer in case of a tie.  May raise Overflow.

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
   
[fromString s] returns SOME(r) if a floating-point numeral can be
scanned from a prefix of string s, ignoring any initial whitespace;
returns NONE otherwise.  The valid forms of floating-point numerals
are described by

	[+~-]?(([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+~-]?[0-9]+)?

[scan getc charsrc] attempts to scan a floating-point number from the
character source charsrc, using the accessor getc, and ignoring any
initial whitespace.  If successful, it returns SOME(r, rest) where r
is the number scanned, and rest is the unused part of the character
source. The valid forms of floating-point numerals are described by

	[+~-]?(([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+~-]?[0-9]+)?

*)
