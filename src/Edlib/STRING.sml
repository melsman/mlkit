signature STRING =
sig

(* ASCII STRINGS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author


DESCRIPTION

   Standard functions on the built-in type "string".

   Functions such as  index  and  search  take an integer
   offset.  This is because it's more efficient to index into
   a string than to take a substring.


SEE ALSO

   AsciiOrdString, LexOrdString, STRING_TYPE, STRING_PARSE,
   STRING_LIST_OPS.


NOTES

   Possibly the integer offset parameters and the revXXX constructs should
   be dropped, relying on the compiler to optimise away intermediate string
   constructions.

   Possibly there should be a dropExtract function to delete a range
   of characters from a string.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:30  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.17  91/03/08  17:02:56  17:02:56  db (Dave Berry)
Renamed existing eq and ne functions to eqMode and neMode; added new
eq and ne functions that match the type given in generic signatures.

Revision 1.16  91/03/06  16:29:49  16:29:49  db (Dave Berry)
Added print function(s).

Revision 1.15  91/02/22  19:03:55  19:03:55  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.

Revision 1.14  91/02/22  14:45:14  14:45:14  db (Dave Berry)
Removed words, words', wordSingles and wordSingles' functions.
These have been replaced by a more general version in STRING_PARSE.sml.

Revision 1.13  91/02/21  18:08:59  18:08:59  db (Dave Berry)
Added Mode datatype, changed search, index and substitution functions (etc.)
to take a mode as the first parameter.

Revision 1.12  91/02/12  14:40:59  14:40:59  db (Dave Berry)
Moved file and fromFile functions to STRING_PARSE.sml.

Revision 1.11  91/02/12  12:19:04  12:19:04  db (Dave Berry)
Changed type to eqtype.

Revision 1.10  91/02/11  19:28:19  19:28:19  db (Dave Berry)
Removed Object sub-structures and the inclusion of STRING_TYPE.
Removed comparison functions.
Moved functions like those provided by the LIST signature to
STRING_LIST_OPS.sml.
This forms part of the major reorganisation of the library.

Revision 1.9  91/01/31  17:48:51  17:48:51  db (Dave Berry)
Added type.

Revision 1.8  91/01/30  18:10:10  18:10:10  db (Dave Berry)
Added skipSpaces function.

Revision 1.7  91/01/25  19:44:20  19:44:20  db (Dave Berry)
Removed dropRepeats' function.

Revision 1.6  91/01/25  19:37:56  19:37:56  db (Dave Berry)
Fixed references to STRING_PARSE and STRING_TYPE, which I'd missed
when converting signature names to all upper case.

Revision 1.5  91/01/25  19:30:47  19:30:47  db (Dave Berry)
Added dependence on OBJECT, fixed include specification.

Revision 1.4  91/01/25  19:10:20  19:10:20  db (Dave Berry)
Added dependence on GeneralTypes and/or InStreamType.

Revision 1.3  91/01/25  16:57:34  16:57:34  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:08:39  17:08:39  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:57:18  16:57:18  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  eqtype string

  val size: string -> int
  exception Ord
  val ord: string -> int
  exception Chr
  val chr: int -> string
  val explode: string -> string list
  val implode: string list -> string
  val ^ : string * string -> string


(* TYPES *)

  eqtype T
    sharing type T = string

  datatype Mode = IgnoreCase | MatchCase
   (* Search and replace functions can either ignore the case of letters
      or match letters only if they have the same case.  If replace functions
      ignore the case when matching, they substitute with the case of the
      matched characters. *)
      

(* CREATORS *)

  exception Size of string * int
   (* Size (fn, i); raised by the create function when it is invoked
      with a negative size. *)

  val create: int -> string -> string
   (* create n s; returns a string composed of n copies of s.
      Raises (Size ("create", n)) if n < 0. *)


(* CONVERTERS *)

  val string: string -> string
   (* string s; returns a version of s with all control characters
      expanded to the SML source form, e.g. if s contains a single
      newline character then (string s) consists of four characters, a
      double quote, a backslash, the letter n and another double quote. *)

  val print: TextIO.outstream -> string -> unit
   (* print os s; sends a version of s to the stream os, with all control
      characters expanded to the SML source form, e.g. if s contains a
      single newline character then (print os s) outputs four characters,
      a double quote, a backslash, the letter n and another double quote. *)


(* OBSERVERS *)

  val exists: Mode -> (string -> bool) -> string -> bool
   (* exists m p s; true if there exists a character in s satisfying p,
      ignoring case if m is IgnoreCase, and matching case if m is MatchCase. *)

  val forAll: Mode -> (string -> bool) -> string -> bool
   (* forAll m p s; true if every character in s satisfies p, ignoring case
      if m is IgnoreCase, and matching case if m is MatchCase. *)

  val eqMode: Mode -> string -> string -> bool
   (* eq m s s'; returns true if (s = s').  Returns false otherwise.
      Ignores case if m is IgnoreCase, and matches case if m is MatchCase. *)

  val neMode: Mode -> string -> string -> bool
   (* ne m s s'; returns true if (s <> s').  Returns false otherwise.
      Ignores case if m is IgnoreCase, and matches case if m is MatchCase. *)

  val eq: string -> string -> bool
   (* eq s s' = eqMode MatchCase.  This function is included to match the
      generic signatures, such as EQ_PRINT. *)

  val ne: string -> string -> bool
   (* ne m s s' = neMode MatchCase.  This function is included to match the
      generic signatures, such as EQ_PRINT. *)

  val fixedWidth: bool
   (* fixedWidth = false *)

  exception Subscript of string * int
   (* Subscript (fn, n); raised if the function named fn is called with
      an out of range argument n. *)

  val prefixes: Mode -> string -> string -> int -> bool
   (* prefixes m s1 s2 n; returns true if s1 is a prefix of the substring of
      s2 starting at (s2 sub n), ignoring case if m is IgnoreCase, and
      matching case if m is MatchCase.   Raises (Subscript ("prefixes", n))
      if not (0 <= n < size s2). *)

  val postfixes: Mode -> string -> string -> int -> bool
   (* postfixes m s1 s2 n; returns true if s1 is a postfix of the substring of
      s2 ending at (s2 sub (n - 1)), ignoring case if m is IgnoreCase, and
      matching case if m is MatchCase.   Raises (Subscript ("postfixes", n))
      if not (0 <= n <= size s2). *)

  val index: Mode -> (string -> bool) -> string -> int -> (int, unit) EdlibGeneral.Result
   (* index m p s n; returns the position in s of the first character
      after (s sub n) (inclusive) that satisfies p ignoring case if m is
      IgnoreCase, and matching case if m is MatchCase.
      Raises (Subscript ("index", n)) if not (0 <= n < size s2). *)

  val revIndex: Mode -> (string -> bool) -> string -> int -> (int, unit) EdlibGeneral.Result
   (* revIndex m p s n; returns the position in s of the last character
      before (s sub n) (exclusive) that satisfies p, ignoring case if m is
      IgnoreCase, and matching case if m is MatchCase.
      Raises (Subscript ("revIndex", n)) if not (0 <= n <= size s2). *)

  val search: Mode -> string -> string -> int -> (int, unit) EdlibGeneral.Result
   (* search m s' s n; find the first occurrence of s' in s after (s sub n)
      (inclusive), ignoring case if m is IgnoreCase, and matching case if m
      is MatchCase.  Returns the index of the first character in that
      substring.  Raises (Subscript ("search", n)) if not (0 <= n < size s). *)

  val revSearch: Mode -> string -> string -> int -> (int, unit) EdlibGeneral.Result
   (* revSearch m s' s n; find the last occurrence of s' in s before (s sub n)
      (exclusive),  ignoring case if m is IgnoreCase, and matching case if m is
      MatchCase.  Returns the index of the first character in that substring.
      Raises (Subscript ("revSearch", n)) if not (0 <= n <= size s). *)

  val occurs: Mode -> string -> string -> int -> bool
   (* occurs m s' s n; return true if s' occurs in s after (s sub n)
      (inclusive), ignoring case if m is IgnoreCase, and matching case if m is
      MatchCase.  Raises (Subscript ("occurs", n)) if not (0 <= n < size s). *)

  val revOccurs: Mode -> string -> string -> int -> bool
   (* revOccurs m s' s n; return true if s' occurs in s before (s sub n)
      (inclusive), ignoring case if m is IgnoreCase, and matching case if m
      is MatchCase.  Raises (Subscript ("revOccurs", n)) if
      not (0 <= n <= size s). *)


(* MANIPULATING STRING TYPE *)

  exception Empty of string
   (* Empty fn; raised if the function called fn is erroneously applied
      to the empty string. *)

  val upper: string -> string
   (* upper s; if the first character in s is a lower case letter,
      returns s with that letter in upper case.  Otherwise leaves s unchanged.
      Raises (Empty "upper") if s is empty. *)

  val lower: string -> string
   (* lower s; if the first character in s is an upper case letter,
      returns s with that letter in lower case.  Otherwise leaves s unchanged.
      Raises (Empty "lower") if s is empty. *)

  val ascii: string -> string
   (* ascii s; if the first character in s is not an ascii character,
      returns s with the top bit stripped from that character.  Otherwise
      leaves s unchanged.  Raises (Empty "ascii") if s is empty. *)

  val control: string -> string
   (* control s; if the first character in s is in the range @ .. _ , returns
      s with that character replaced with the corresponding control character.
      Otherwise leaves s unchanged.  Raises (Empty "ascii") if s is empty. *)


(* SELECTORS *)

  (* infix 9 sub *)
  val sub: string * int -> string
   (* s sub n; return the nth character of s.
      Raises (Subscript ("sub", n)) if not (0 <= n < size s). *)

  val nth: int -> string -> string
   (* nth n s; return the nth character of s.
      Raises (Subscript ("nth", n)) if not (0 <= n < size s). *)

  exception Char of string * string
   (* Char (fn, c); raised if the function called fn is called with a
      string c such that (size c <> 1).  The idea is that the update functions
      replace one character with another, rather than with an arbitrary length
      string.  *)

  exception Extract of int * int

  val extract: int -> int -> string -> string
   (* extract s start finish; 0 <= start,finish <= size (s); returns the
      substring starting with (s sub start) and ending with
      (s sub (finish - 1)).  Returns "" if (start = finish).  Raises
      (Extract (start, finish)) if not (0 <= start <= finish <= size s). *)


(* OTHER MANIPULATORS *)

  val skipSpaces: string -> string
   (* skipSpaces s; returns s with any leading invisible characters removed. *)

  val subst: Mode -> string -> string -> string -> string
   (* subst m c s' s; replaces all occurrences of c in s by s'.  If m is
      MatchCase, then c must exactly match the character to be replaced,
      and s' is substituted literally.  If m is IgnoreCase and c is a letter,
      then both upper case and lower case instances of c will be replaced,
      and all letters in the instances of s' that replace them are converted 
      to the appropriate case.  Raises (Char ("subst", s')) if size c > 0. *)

  val rev: string -> string
   (* rev s; returns the reflection of s. E.g. rev "abc" = "cba". *)

  val showAscii: string -> string
   (* showAscii s; returns s with all characters expanded to the form they
      would have in a string literal.  E.g. showAscii "\n" = "\\n".
      This function differs from the string function only by not
      adding quotes at the beginning and end of the string and in having
      a more intuitive name. *)

  val padL: string -> int -> string -> string
   (* padL c w s; pads the string s with character c on the
      left until its size is w.  Raises (Char ("padL", c)) if (size c <> 1). *)

  val padR: string -> int -> string -> string
   (* padR c w s; pads the string s with character c on the
      right until its size is w.  Raises (Char ("padR", c)) if (size c <> 1). *)

  val padC: string -> int -> string -> string
   (* padC c w s; pads the string s with character c to centre the existing
      satring in a new string of size w.
      Raises (Char ("padC", c)) if (size c <> 1). *)

  val truncL: int -> string -> string
   (* truncL w s; truncates the string s by removing characters on the left
      until its size is w. *)

  val truncR: int -> string -> string
   (* truncR w s; truncates the string s by removing characters on the right
      until its size is w. *)

  val truncC: int -> string -> string
   (* truncC w s; truncates the string s by removing characters equally from
      both ends until its size is w. *)

  val dropL: string -> string -> string
   (* dropL c s; removes occurrence of the character c on the left of s.
      Raises (Char ("dropL", c)) if (size c <> 1). *)

  val dropR: string -> string -> string
   (* dropR c s; removes occurrence of the character c on the right of s.
      Raises (Char ("dropR", c)) if (size c <> 1). *)


(* ITERATORS *)

  val map: (string -> string) -> string -> string
   (* map f s; builds a string by applying f to each character in s. *)

  val apply: (string -> unit) -> string -> unit
   (* apply f s; applies f to each character in s. *)

  val mapAll: (string -> bool) -> (string -> string) -> string -> string
   (* map p f s; builds a string by applying f to each character in s
      that satisfies p. *)

  val applyAll: (string -> bool) -> (string -> unit) -> string -> unit
   (* applyAll p f s; applies f to each character in s that satisfies p. *)
end
