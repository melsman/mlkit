(*INTEGER.sml*)

signature INTEGER = sig
  eqtype int
  val toLarge : int -> int32 
  val fromLarge : int32 -> int 
  val toInt : int -> Initial.int0 
  val fromInt : Initial.int0 -> int 
  val precision : Initial.int0 option 
  val minInt : int option 
  val maxInt : int option 
  val ~ : int -> int 
  val * : (int * int) -> int 
  val div : (int * int) -> int 
  val mod : (int * int) -> int 
  val quot : (int * int) -> int 
  val rem : (int * int) -> int 
  val + : (int * int) -> int 
  val - : (int * int) -> int 
  val compare : (int * int) -> order 
  val > : (int * int) -> bool 
  val >= : (int * int) -> bool 
  val < : (int * int) -> bool 
  val <= : (int * int) -> bool 
  val abs : int -> int 
  val min : (int * int) -> int 
  val max : (int * int) -> int 
  val sign : int -> Initial.int0 
  val sameSign : (int * int) -> bool 
  val fmt : StringCvt.radix -> int -> string 
  val toString : int -> string 
  val fromString : string -> int option 
  val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> 'a -> (int * 'a) option 
end; (*signature INTEGER*)

(* 
   [precision] is SOME n, where n is the number of significant bits in an
   integer.  In Moscow ML n is 31 in 32-bit architectures and 63 in 64-bit
   architectures.

   [minInt] is SOME n, where n is the most negative integer.

   [maxInt] is SOME n, where n is the most positive integer.

   [~, *, div, mod, +, -, >, >=, <, <=, abs] are the usual operations
   on integers, as prescribed by the Definition.

   [quot(i, d)] is the quotient of i by d, rounding towards zero (instead 
   of rounding towards minus infinity, as done by div).

   [rem(i, d)] is the remainder for quot.  That is, 
   if    q' = quot(i, d)  and  r' = rem(i, d)
   then  d * q' + r' = i  and  0 <= d * q' <= i  or  i <= d * q' <= 0.
   The recommended fixity for quot and rem is  
                        infix 7 quot rem

   [min(x, y)] is the smaller of x and y.

   [max(x, y)] is the larger of x and y.

   [sign x] is ~1, 0, or 1, according as x is negative, zero, or positive.

   [compare(x, y)] returns LESS, EQUAL, or GREATER, according 
   as x is less than, equal to, or greater than y.

   [sameSign(x, y)] is true iff sign x = sign y.

   [toDefault x] is x.

   [fromDefault x] is x.

   [fmt radix i] returns a string representing i, in the radix (base)
   specified by radix.

     radix    description                     output format  
     ------------------------------------------------------
      BIN     signed binary      (base  2)    ~?[01]+
      OCT     signed octal       (base  8)    ~?[0-7]+
      DEC     signed decimal     (base 10)    ~?[0-9]+
      HEX     signed hexadecimal (base 16)    ~?[0-9A-F]+

   [toString i] returns a string representing i in signed decimal format.
   Equivalent to (fmt DEC i).
   
   [fromString s] returns SOME(i) if a decimal integer numeral can be
   scanned from a prefix of string s, ignoring any initial whitespace;
   returns NONE otherwise.  A decimal integer numeral must have form,
   after possible initial whitespace: 
        [+~-]?[0-9]+

   [scan radix getc charsrc] attempts to scan an integer numeral
   from the character source charsrc, using the accessor getc, and
   ignoring any initial whitespace.  The radix argument specifies the base
   of the numeral (BIN, OCT, DEC, HEX).  If successful, it returns
   SOME(i, rest) where i is the value of the number scanned, and rest
   is the unused part of the character source.  A numeral must have
   form, after possible initial whitespace:

     radix    input format 
     ---------------------------
      BIN     [+~-]?[0-1]+
      OCT     [+~-]?[0-7]+
      DEC     [+~-]?[0-9]+
      HEX     [+~-]?[0-9a-fA-F]+
*)
