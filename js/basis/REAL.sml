(*REAL.sml*)

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
end; (*signature REAL*)

(* [~, *, /, +, -, >, >=, <, <=, abs] are the usual operations on reals.

   [min(x, y)] is the smaller of x and y.

   [max(x, y)] is the larger of x and y.

   [sign x] is ~1, 0, or 1, according as x is negative, zero, or positive.

   [compare(x, y)] returns LESS, EQUAL, or GREATER, according 
   as x is less than, equal to, or greater than y.

   [sameSign(x, y)] is true iff sign x = sign y.

   [toDefault x] is x.

   [fromDefault x] is x.

   [fromInt i] is the floating-point number representing integer i.

   [floor r] is the largest integer <= r (rounds towards minus infinity).
   May raise Overflow.

   [ceil r] is the smallest integer >= r (rounds towards plus infinity).
   May raise Overflow.

   [trunc r] is numerically largest integer between r and zero 
   (rounds towards zero). May raise Overflow.

   [round r] is the integer nearest to r, using the default rounding
   mode.  NOTE: This isn't the required behaviour: it should round to
   nearest even integer in case of a tie.  May raise Overflow.

   [fmt spec r] returns a string representing r, in the format
   specified by spec.

      spec          description                            C printf 
      ---------------------------------------------------------------
      SCI NONE      scientific,   6 digits after point       %e
      SCI (SOME n)  scientific,   n digits after point       %.ne
      FIX NONE      fixed-point,  6 digits after point       %f
      FIX (SOME n)  fixed-point,  n digits after point       %.nf
      GEN NONE      auto choice, 12 significant digits       %.12g
      GEN (SOME n)  auto choice,  n significant digits       %.ng

   [toString r] returns a string representing r, with automatic choice
   of format according to the magnitude of r.  
   Equivalent to (fmt (GEN NONE) r).
   
   [fromString s] returns SOME(r) if a floating-point numeral can be
   scanned from a prefix of string s, ignoring any initial whitespace;
   returns NONE otherwise.  The valid forms of floating-point numerals
   are described by:
	[+~-]?(([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+~-]?[0-9]+)?

   [scan getc charsrc] attempts to scan a floating-point number from
   the character source charsrc, using the accessor getc, and ignoring
   any initial whitespace.  If successful, it returns SOME(r, rest)
   where r is the number scanned, and rest is the unused part of the
   character source.  The valid forms of floating-point numerals
   are described by:
	[+~-]?(([0-9]+(\.[0-9]+)?)|(\.[0-9]+))([eE][+~-]?[0-9]+)?
*)
