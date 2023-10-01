(** Operations on string values.

The STRING signature specifies the basic operations on a string type,
which is a vector of the underlying character type char as defined in
the structure.

The STRING signature is matched by two structures, the required String
and the optional WideString. The former implements strings based on
the extended ASCII 8-bit characters, and is a companion structure to
the Char structure. The latter provides strings of characters of some
size greater than or equal to 8 bits, and is related to the structure
WideChar. In particular, the type String.char is identical to the type
Char.char and, when WideString is defined, the type WideString.char is
identical to the type WideChar.char. These connections are made
explicit in the Text and WideText structures, which match the TEXT
signature.
*)

signature STRING =
  sig
    eqtype string
    eqtype char
    val maxSize : int
    val size : string -> int
    val sub : string * int -> char
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string
    val ^ : string * string -> string
    val concat : string list -> string
    val concatWith : string -> string list -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix    : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool
    val compare : string * string -> order
    val collate : (char * char -> order) -> string * string -> order
    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool
    val toString : string -> String.string
(*    val scan       : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader *)
    val fromString : String.string -> string option
    val toCString : string -> String.string
    val fromCString : String.string -> string option
  end

(**

[maxSize] contains the longest allowed size of a string.

[size s] returns |s|, the number of characters in string s.

[sub (s, i)] returns the i(th) character of s, counting from
zero. This raises Subscript if i < 0 or |s| <= i.

[extract (s, i, opt)] When opt = NONE, the function returns the
substring of s from the i(th) character to the end of the string,
i.e., the string s[i..|s|-1]. Raises Subscript if i < 0 or |s| <
i. When opt = SOME j, the function returns the substring of size j
starting at index i, i.e., the string s[i..i+j-1]. It raises Subscript
if i < 0 or j < 0 or |s| < i + j. Note that, if defined, extract
returns the empty string when i = |s|.

[substring (s, i, j)] returns the substring s[i..i+j-1], i.e., the
substring of size j starting at index i. Equivalent to extract(s, i,
SOME j). The functions extract and substring perform bounds checking
in such a way that the Overflow exception is not raised.

[s ^ t] is the concatenation of the strings s and t. Raises Size if
|s| + |t| > maxSize.

[concat l] returns the concatenation of all the strings in l. Raises
Size if the sum of all the sizes is greater than maxSize.

[concatWith s l] returns the concatenation of the strings in the list
l using the string s as a separator. Raises Size if the size of
the resulting string would be greater than maxSize.

[str c] returns a string of size one containing the character c.

[implode l] generates the string containing the characters in the list
l. This is equivalent to concat (List.map str l). Raises Size if the
resulting string would have size greater than maxSize.

[explode s] returns the list of characters in the string s.

[map f s] applies f to each element of s from left to right, returning
the resulting string. It is equivalent to implode(List.map f (explode
s)).

[translate f s] returns the string generated from s by mapping each
character in s by f. It is equivalent to concat(List.map f (explode
s)).

[tokens f s] returns a list of tokens derived from s from left to
right. A token is a non-empty maximal substring of s not containing
any delimiter. A delimiter is a character satisfying the predicate
f. Two tokens may be separated by more than one delimiter. For
example, if the only delimiter is the character #"|", then the string
"|abc||def" contains two tokens "abc" and "def".

[fields f s] returns a list of fields derived from s from left to
right. A field is a (possibly empty) maximal substring of s not
containing any delimiter. A delimiter is a character satisfying the
predicate f. Two fields are separated by exactly one delimiter. For
example, if the only delimiter is the character #"|", then the string
"|abc||def" contains the four fields "", "abc", "" and "def".

[isPrefix s1 s2] returns true if the string s1 is a prefix of the
string s2. Note that the empty string is a prefix of any string and
that a string is a prefix of itself.

[isSubstring s1 s2] returns true if the string s1 is a substring of
the string s2. Note that the empty string is a substring of any string
and that a string is a substring of itself.

[isSuffix s1 s2] returns true if the string s1 is a suffix of the
string s2. Note that the empty string is a suffix of any string and
that a string is a suffix of itself.

[compare (s, t)] does a lexicographic comparison of the two strings
using the ordering Char.compare on the characters. It returns LESS,
EQUAL, or GREATER, if s is less than, equal to, or greater than t,
respectively.

[collate f (s, t)] performs lexicographic comparison of the two
strings using the given ordering f on characters.

[a < b] returns true if the string a is lexicographically, strictly
less than b. Returns false otherwise.

[a <= b] returns true if the string a is lexicographically less than
or equal to b. Returns false otherwise.

[a > b] returns true if the string a is lexicographically, strictly
greater than b. Returns false otherwise.

[a >= b] returns true if the string a is lexicographically greater than
or equal to b. Returns false otherwise.

[toString s] returns a string corresponding to s, with non-printable
characters replaced by SML escape sequences. This is equivalent to
(translate Char.toString s).

[scan getc strm] This function scan its character source as a sequence
of printable characters, converting SML escape sequences into the
appropriate characters. The function does not skip leading
whitespace. It returns as many characters as can successfully be
scanned, stopping when they reach the end of the source or a
non-printing character (i.e., one not satisfying isPrint), or if it
encounters an improper escape sequence. The function returns the
remaining characters as the rest of the stream. If no conversion is
possible, e.g., if the first character is non-printable or begins an
illegal escape sequence, NONE is returned. For more information on the
allowed escape sequences, see the entry for CHAR.fromString. SML
source also allows escaped formatting sequences, which are ignored
during conversion. The rule is that if any prefix of the input is
successfully scanned, including an escaped formatting sequence, the
function returns some string. It only returns NONE in the case where
the prefix of the input cannot be scanned at all.

[fromString s] Equivalent to (StringCvt.scanString scan s). Because of
the special cases, such as fromString "" = SOME "", fromString "\\
\\\^D" = SOME "", and fromString "\^D" = NONE, the functions
fromString and scan cannot be implemented as a simple iterative
application of CHAR.scan.

[toCString s] returns a string corresponding to s, with non-printable
characters replaced by C escape sequences. This is equivalent to
(translate Char.toCString s).

[fromCString s] scans the string s as a string in the C language,
converting C escape sequences into the appropriate characters. The
semantics are identical to fromString above, except that C escape
sequences are used (see ISO C standard ISO/IEC 9899:1990). For more
information on the allowed escape sequences, see the entry for
CHAR.fromCString. Note that fromCString accepts an unescaped single
quote character, but does not accept an unescaped double quote
character.

*)

(** SigDoc *)
structure String : STRING = String
