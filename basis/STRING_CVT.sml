(** Utility operations for converting to and from strings.

The StringCvt structure provides types and functions for handling the
conversion between strings and values of various basic types.
*)

signature STRING_CVT =
  sig
    datatype radix = BIN | OCT | DEC | HEX
    datatype realfmt
      = SCI of int option
      | FIX of int option
      | GEN of int option
      | EXACT
    type ('a,'b) reader = 'b -> ('a * 'b) option
    val padLeft    : char -> int -> string -> string
    val padRight   : char -> int -> string -> string
    val splitl     : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
    val takel      : (char -> bool) -> (char, 'a) reader -> 'a -> string
    val dropl      : (char -> bool) -> (char, 'a) reader -> 'a -> 'a
    val skipWS     : (char, 'a) reader -> 'a -> 'a
    type cs
    val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option
  end

(**

[datatype radix] The values of type radix are used to specify the radix of
a representation of an integer, corresponding to the bases 2, 8, 10,
and 16, respectively.

[datatype realfmt] Values of type realfmt are used to specify the format
of a string representation for a real or floating-point number.

A value (SCI opt) corresponds to scientific representation:

    [~]?[0-9].[0-9]+?E[0-9]+

where there is always one digit before the decimal point, nonzero
if the number is nonzero. The optional integer value specifies the
number of decimal digits to appear after the decimal point, with 6
being the default.  In particular, if 0 is specified, there should
be no fractional part. The exponent is zero if the value is zero.

A value (FIX opt) corresponds to a fixed-point representation:

    [~]?[0-9]+.[0-9]+?

where there is always at least one digit before the decimal
point. The optional integer value specifies the number of decimal
digits to appear after the decimal point, with 6 being the
default. In particular, if 0 is specified, there should be no
fractional part.

A value (GEN opt) allows a formatting function to use either the
scientific or fixed-point notation, whichever is shorter, breaking
ties in favor of fixed-point. The optional integer value specifies the
maximum number of significant digits used, with 12 the default. The
string should display as many significant digits as possible, subject
to this maximum.  There should not be any trailing zeros after the
decimal point. There should not be a decimal point unless a fractional
part is included.

The fourth constructor EXACT specifies that the string should
represent the real using an exact decimal representation. The string
contains enough information in order to reconstruct a semantically
equivalent real value using REAL.fromDecimal o valOf o
IEEEReal.fromString. Refer to the description of IEEEReal.toString for
more precise information concerning this format.

In all cases, positive and negative infinities are converted to "inf"
and "~inf", respectively, and NaN values are converted to the string
"nan".

[type ('a,'b) reader] The type of a reader producing values of type 'a
from a stream of type 'b.  A return value of SOME(a,b) corresponds to
a value a scanned from the stream, plus the remainder b of the
stream. A return value of NONE indicates that no value of the correct
type could be scanned from the prefix of the stream.  The reader type
is designed for use with a stream or functional view of I/O. Scanning
functions using the reader type, such as skipWS, splitl, and Int.scan,
will often use lookahead characters to determine when to stop
scanning. If the character source ('b in an ('a,'b) reader) is
imperative, the lookahead characters will be lost to any subsequent
scanning of the source. One mechanism for combining imperative I/O
with the standard scanning functions is provided by the
TextIO.scanStream function.

[padLeft c i s]

[padRight c i s] These return s padded, on the left or right,
respectively, with i - |s| copies of the character c. If |s| >= i,
they just return the string s. In other words, these functions right
and left-justify s in a field i characters wide, never trimming off
any part of s. Note that if i <= 0, s is returned. These functions
raise Size if the size of the resulting string would be greater than
String.maxSize.

[splitl f rdr src] returns (pref, src') where pref is the longest
prefix (left substring) of src, as produced by the character reader
rdr, all of whose characters satisfy f, and src' is the remainder of
src. Thus, the first character retrievable from src' is the leftmost
character not satisfying f. The function splitl can be used with
scanning functions such as scanString by composing it with SOME; e.g.,
scanString (fn rdr => SOME o (splitl f rdr)).

[takel f rdr src]

[dropl f rdr src] These routines scan the source src for the first
character not satisfying the predicate f.  The function dropl drops
the maximal prefix consisting of characters satisfying the predicate,
returning the rest of the source, while takel returns the maximal
prefix consisting of characters satisfying the predicate. These can be
defined in terms of splitl:

   takel f rdr s = #1(splitl f rdr s)
   dropl f rdr s = #2(splitl f rdr s)

[skipWS rdr src] strips whitespace characters from a stream src using
the reader rdr. It returns the remaining stream. A whitespace
character is one that satisfies the predicate Char.isSpace. It is
equivalent to dropl Char.isSpace.

[type cs] The abstract type of the character stream used by
scanString. A value of this type represents the state of a character
stream. The concrete type is left unspecified to allow implementations
a choice of representations.  Typically, cs will be an integer index
into a string.

[scanString sc s] provides a general framework for converting the
string s into some value. The user supplies a scanning function sc and
the string s. The function scanString converts the string into a
character source (type cs) and applies the scanning function. A
scanning function converts a reader of characters into a reader of
values of the desired type. Typical scanning functions are Bool.scan
and Date.scan.

[Discussion] The SML Basis Library emphasizes a functional view for
scanning values from text. This provides a natural and elegant way to
write simple scanners and parsers, especially as these typically
involve some form of reading ahead and backtracking. The model
involves two types of components: ways to produce character readers
and functions to convert character readers into value readers. For the
latter, most types ty have a corresponding scanning function of type

      (char, 'a) reader -> (ty, 'a) reader

Character readers are provided for the common sources of characters,
either explicitly, such as the SUBSTRING.getc and STREAM_IO.input1
functions, or implicitly, such as the TEXT_IO.scanStream. As an
example, suppose we expect to read a decimal integer followed by a
date from TextIO.stdIn. This could be handled by the following code:

    local
      structure TIO = TextIO
      structure SIO = TextIO.StreamIO
      val scanInt = Int.scan StringCvt.DEC SIO.input1
      val scanDate = Date.scan SIO.input1
    in
      fun scanID () =
            case scanInt (TIO.getInstream TIO.stdIn) of
              NONE => raise Fail "No integer"
            | SOME (intVal, ins') =>
                case scanDate ins' of
                  NONE => raise Fail "No date"
                | SOME (dateVal, _) =>  (intVal,dateVal)
    end

In this example, we used the underlying stream I/O component of
TextIO.stdIn, which is cleaner and more efficient. If, at some later
point, we wish to return to the imperative model and do input directly
using TextIO.stdIn, we need to reset it with the current stream I/O
value using TextIO.setInstream.  Alternatively, we could rewrite the
code using imperative I/O:

    local
      structure TIO = TextIO
      val scanInt = TIO.scanStream (Int.scan StringCvt.DEC)
      val scanDate = TIO.scanStream Date.scan
    in
      fun scanID () =
            case scanInt TIO.stdIn of
              NONE => raise Fail "No integer"
            | SOME intVal =>
                case scanDate TIO.stdIn of
                  NONE => raise Fail "No date"
                | SOME dateVal =>  (intVal,dateVal)
    end

The scanString function was designed specifically to be combined with
the scan function of some type T, producing a function val fromString
: string -> T option for the type. For this reason, scanString only
returns a scanned value, and not some indication of where scanning
stopped in the string. For the user who wants to receive a scanned
value and the unscanned portion of a string, the recommended technique
is to convert the string into a substring and combine scanning
functions with Substring.getc, e.g., Bool.scan Substring.getc. Or, the
user can create an input stream with TextIO.openString using the
string as the source.

When the input source is a list of characters, scanning values can be
accomplished by applying the appropriate scan function to the function
List.getItem. Thus, Bool.scan List.getItem has the type

      (bool, char list) reader

which will scan a boolean value and return that value and the
remainder of the list.

*)
