(** Operations on signed integer values.

Instances of the INTEGER signature provide a type of signed integers
of either a fixed or arbitrary precision, and arithmetic and
conversion operations. For fixed precision implementations, most
arithmetic operations raise the exception Overflow when their result
is not representable.
*)

signature INTEGER =
  sig
      eqtype int

      val toLarge   : int -> intinf
      val fromLarge : intinf -> int
      val toInt   : int -> Initial.int0
      val fromInt : Initial.int0 -> int

      val precision : Initial.int0 option
      val minInt : int option
      val maxInt : int option

      val + : int * int -> int
      val - : int * int -> int
      val * : int * int -> int
      val div : int * int -> int
      val mod : int * int -> int
      val quot : int * int -> int
      val rem : int * int -> int

      val compare : int * int -> order
      val <  : int * int -> bool
      val <= : int * int -> bool
      val >  : int * int -> bool
      val >= : int * int -> bool

      val ~ : int -> int
      val abs : int -> int
      val min : int * int -> int
      val max : int * int -> int
      val sign : int -> Initial.int0
      val sameSign : int * int -> bool

      val fmt      : StringCvt.radix -> int -> string
      val toString : int -> string
      val scan     : StringCvt.radix
	             -> (char, 'a) StringCvt.reader
	             -> (int, 'a) StringCvt.reader
    val fromString : string -> int option
  end

(**
[type int]

[toLarge i] returns the LargeInt.int representation of the value i.

[fromLarge i] returns the int representation of the LargeInt.int value
i. Raises Overflow if the value does not fit. Int<M>.fromLarge o
Int<N>.toLarge converts an integer from type Int<N>.int to Int<M>.int.

[toInt i] converts the value i to the default integer type. Raises
Overflow if the value does not fit.

[fromInt i] returns the int representation of the value i. Raises
Overflow if the value does not fit.

[precision] If SOME(n), this denotes the number n of significant bits
in type int, including the sign bit. If it is NONE, int has arbitrary
precision. The precision need not necessarily be a power of two.

[minInt] is the minimal (most negative) integer, representable by
int. If NONE, int can represent all negative integers, within the
limits of the heap size.  If precision is SOME(n), then we have minInt
= -2(n-1).

[maxInt] is the maximal (most positive) integer, representable by
int. If NONE, int can represent all positive integers, within the
limits of the heap size.  If precision is SOME(n), then we have maxInt
= 2(n-1) - 1.

[a + b] returns the sum of a and b. Raises Overflow when the result is
not representable.

[a - b] returns the difference between a and b. Raises Overflow when
the result is not representable.

[a * b] returns the product of a and b. Raises Overflow when the
result is not representable.

[i div j] returns the greatest integer less than or equal to the
quotient of i by j, i.e., floor(((i / j))). It raises Overflow when
the result is not representable, or Div when j = 0. Note that rounding
is towards negative infinity, not zero.

[i mod j] returns the remainder of the division of i by j. It raises
Div when j = 0. When defined, (i mod j) has the same sign as j, and

        (i div j) * j + (i mod j) = i

[quot (i, j)] returns the truncated quotient of the division of i by
j, i.e., it computes (i / j) and then drops any fractional part of the
quotient. It raises Overflow when the result is not representable, or
Div when j = 0. Note that unlike div, quot rounds towards zero. In
addition, unlike div and mod, neither quot nor rem are infix by
default; an appropriate infix declaration would be infix 7 quot rem.

This is the semantics of most hardware divide instructions, so quot
may be faster than div.

[i rem j] returns the remainder of the division of i by j. It raises
Div when j = 0. (i rem j) has the same sign as i, and it holds that

        (i quot j) * j + (i rem j) = i

This is the semantics of most hardware divide instructions, so rem may
be faster than mod.

[compare (i, j)] returns LESS, EQUAL, or GREATER when i is less than,
equal to, or greater than j, respectively.

[a < b] returns true if a is strictly less than b.

[a <= b] returns true if a is less than or equal to b.

[a > b] returns true if a is strictly greater than b.

[a >= b] returns true if a is greater than or equal to b.

[~ i] returns the negation of i, i.e., (0 - i). It raises Overflow
when the result is not representable. This can happen, for example,
when int is an n-bit 2's-complement integer type, and ~ is applied to
-2 (n-1).

[abs i] returns the absolute value (magnitude) of i. It raises Overflow
when the result is not representable.

[min (a,b)] returns the smaller of a and b.

[max (a,b)] returns the larger of a and b.

[sign i] returns ~1, 0, or 1 when i is less than, equal to, or greater
than 0, respectively.

[sameSign (i, j)] returns true if i and j have the same sign. It is
equivalent to (sign i = sign j).

[fmt radix i] returns a string containing a representation of i with
#"~" used as the sign for negative numbers. The string is formatted
according to radix. The hexadecimal digits 10 through 15 are
represented as #"A" through #"F", respectively. No prefix "0x" is
generated for the hexadecimal representation.

[toString i] returns a string containing a decimal representation of i
with #"~" used as the sign for negative numbers. Equivalent to fmt
StringCvt.DEC i.

[scan radix getc strm] returns SOME(i,rest) if an integer in the
format denoted by radix can be parsed from a prefix of the character
stream strm after skipping initial whitespace, where i is the value of
the integer parsed and rest is the rest of the character stream. NONE
is returned otherwise. Raises Overflow when an integer can be parsed,
but is too large to be represented by type int.

The format that scan accepts depends on the radix argument. Regular
expressions defining these formats are as follows:

	  Radix                 Format
	  StringCvt.BIN         [+~-]?[0-1]+
	  StringCvt.OCT         [+~-]?[0-7]+
	  StringCvt.DEC         [+~-]?[0-9]+
	  StringCvt.HEX         [+~-]?(0x | 0X)?[0-9a-fA-F]+

Note that strings such as "0xg" and "0x 123" are scanned as SOME(0),
even using a hexadecimal radix.

[fromString s] returns SOME(i) if an integer i in the format
[+~-]?[0-9]+ can be parsed from a prefix of the string s, ignoring
initial whitespace; NONE is returned otherwise. Raises Overflow when
an integer can be parsed, but is too large to fit in type int. It is
equivalent to the expression StringCvt.scanString (scan
StringCvt.DEC).

*)
