(** Operations on substrings.

The SUBSTRING signature specifies manipulations on an abstract
representation of a sequence of contiguous characters in a string. A
substring value can be modeled as a triple (s, i, n), where s is the
underlying string, i is the starting index, and n is the size of the
substring, with the constraint that 0 <= i <= i + n <= |s|.

The substring type and its attendant functions provide a convenient
abstraction for performing a variety of common analyses of strings,
such as finding the leftmost occurrence, if any, of a character in a
string. In addition, using the substring functions avoids much of the
copying and bounds checking that occur if similar operations are
implemented solely in terms of strings.

The SUBSTRING signature is matched by two structures, the required
Substring and the optional WideSubstring. The former is a companion
structure to the Char and String structures, which are based on the
extended ASCII 8-bit character set. The structure WideSubstring is
related in the same way to the structures WideChar and WideString,
which are based on characters of some size greater than or equal to 8
bits. In particular, the types Substring.string and Substring.char are
identical to those types in the structure String and, when
WideSubstring is defined, the types WideSubstring.string and
WideSubstring.char are identical to those types in the structure
WideString.

All of these connections are made explicit in the Text and WideText
structures, which match the TEXT signature. In the exposition below,
references to a String structure refers to the substructure of that
name defined in either the Text or the WideText structure, which ever
is appropriate.
*)
signature SUBSTRING =
  sig
    type substring
    eqtype char
    eqtype string

    val sub         : substring * int -> char
    val size        : substring -> int
    val base        : substring -> string * int * int
    val extract     : string * int * int option -> substring
    val substring   : string * int * int -> substring
    val full        : string -> substring
    val string      : substring -> string
    val isEmpty     : substring -> bool
    val getc        : substring -> (char * substring) option
    val first       : substring -> char option
    val triml       : int -> substring -> substring
    val trimr       : int -> substring -> substring
    val slice       : substring * int * int option -> substring
    val concat      : substring list -> string
    val concatWith  : string -> substring list -> string
    val explode     : substring -> char list
    val isPrefix    : string -> substring -> bool
    val isSubstring : string -> substring -> bool
    val isSuffix    : string -> substring -> bool
    val compare     : substring * substring -> order
    val collate     : (char * char -> order) -> substring * substring -> order
    val splitl      : (char -> bool) -> substring -> substring * substring
    val splitr      : (char -> bool) -> substring -> substring * substring
    val splitAt     : substring * int -> substring * substring
    val dropl       : (char -> bool) -> substring -> substring
    val dropr       : (char -> bool) -> substring -> substring
    val takel       : (char -> bool) -> substring -> substring
    val taker       : (char -> bool) -> substring -> substring
    val position    : string -> substring -> substring * substring
    val span        : substring * substring -> substring
    val translate   : (char -> string) -> substring -> string
    val tokens      : (char -> bool) -> substring -> substring list
    val fields      : (char -> bool) -> substring -> substring list
    val app         : (char -> unit) -> substring -> unit
    val foldl       : (char * 'a -> 'a) -> 'a -> substring -> 'a
    val foldr       : (char * 'a -> 'a) -> 'a -> substring -> 'a 
  end

(**

[sub (s, i)] returns the i(th) character in the substring, counting
from the beginning of s. It is equivalent to String.sub(string s,
i). The exception Subscript is raised unless 0 <= i < |s|.

[size s] returns the size of s. Equivalent to #3 o base and
String.size o string.

[base ss] returns a triple (s, i, n) giving a concrete representation
of the substring. Here s is the underlying string, i is the starting
index, and n is the size of the substring. It holds that 0 <= i <= i +
n <= |s|.

[extract (s, i, opt)] If opt=NONE, the function returns the substring
of s from the i(th) character to the end of the string, i.e., the
string s[i..|s|-1].  If opt=SOME j, the function returns the substring
of size j starting at index i, i.e., the string s[i..i+j-1].  Raises
Subscript unless 0 <= i <= |s|. The function must perform bounds
checking in such a way that the Overflow exception is not raised.

[substring (s, i, j)] returns the substring s[i..i+j-1], i.e., the
substring of size j starting at index i. This is equivalent to
extract(s, i, SOME j). We require that base o substring be the
identity function on valid arguments. An implementation must perform
bounds checking in such a way that the Overflow exception is not
raised.

[full s] creates a substring representing the entire string s. It is
equivalent to the expression substring(s, 0, String.size s).

[string s] creates a string value corresponding to the substring. It
is equivalent to String.substring o base for the corresponding String
structure.

[isEmpty s] returns true if s has size 0. Returns false otherwise.

[getc s] returns the first character in s and the rest of the
substring, or NONE if s is empty.

[first s] returns the first character in s, or NONE if s is empty.

[triml k s] removes k characters from the left of the substring s. If
k is greater than the size of the substring, an empty substring is
returned. Specifically, for substring ss = substring(s, i, j) and k <=
j, we have (triml k ss = substring(s, i+k, j-k)). The exception
Subscript is raised if k < 0. This exception is raised when (triml k)
is evaluated.

[trimr k s] removes k characters from the right of the substring s. If
k is greater than the size of the substring, an empty substring is
returned. Specifically, for substring ss = substring(s, i, j) and k <=
j, we have (trimr k ss = substring(s, i, j-k)). The exception
Subscript is raised if k < 0. This exception is raised when (trimr k)
is evaluated.

[slice (s, i, opt)] returns a substring of s starting at the i(th)
character. If opt=SOME m, the size of the resulting substring is
m. Otherwise, the size is |s| - i. To be valid, the arguments in the
first case must satisfy 0 <= i, 0 <= m and i + m <= |s|. In the second
case, the arguments must satisfy 0 <= i <= |s|. If the arguments are
not valid, the exception Subscript is raised.

[concat l] returns a string that is the concatenation of the
substrings in l. This is equivalent to String.concat o (List.map
string). This raises Size if the sum of all the sizes is greater than
the corresponding maxSize for the string type.

[concatWith s l] returns the concatenation of the substrings in the
list l using the string s as a separator. Raises Size if the size of
the resulting string would be greater than maxSize for the string
type.

[explode s] returns the list of characters composing the
substring. This is equivalent to String.explode (string s).

[isPrefix s ss] returns true if the string s is a prefix of the
substring ss. Equivalent to String.isPrefix s (string ss).

[isSubstring s ss] returns true if the string s is a substring of the
substring ss. Equivalent to String.isSubstring s (string ss).

[isSuffix s ss] returns true if the string s is a suffix of the
substring ss. Equivalent to String.isSuffix s (string ss).

[compare (s, t)] compares the two substrings lexicographically using
the default character comparison function. Equivalent to
(String.compare (string s, string t)).

[collate f (s, t)] compares the two substrings lexicographically using
the character comparison function f. Equivalent to (String.collate f
(string s, string t)).

[splitl f s] scans s from left to right looking for the first
character that does not satisfy the predicate f. It returns the pair
(ls, rs) giving the split of the substring into the span up to that
character and the rest. ls is the left side of the split, and rs is
the right side.

[splitr f s] scans s from right to left looking for the first
character that does not satisfy the predicate f. It returns the pair
(ls, rs) giving the split of the substring into the span up to that
character and the rest. ls is the left side of the split, and rs is
the right side.

[splitAt (s, i)] returns the pair of substring (ss, ss'), where ss
contains the first i characters of s and ss' contains the rest,
assuming 0 <= i <= size s. Otherwise, it raises Subscript.

[dropl p s] scans the substring s, left to right, for the first
character not satisfying the predicate p. The function drops the
maximal substring consisting of characters satisfying the
predicate. Equivalent to #2(splitl p s).

[dropr f s] scans the substring s, right to left, for the first
character not satisfying the predicate p. The function drops the
maximal substring consisting of characters satisfying the
predicate. Equivalent to #2(splitr p s).

[takel f s] scans the substring s, left to right, for the first
character not satisfying the predicate p. The function returns the
maximal substring consisting of characters satisfying the
predicate. Equivalent to #1(splitl p s).

[taker f s] scans the substring s, right to left, for the first
character not satisfying the predicate p. The function returns the
maximal substring consisting of characters satisfying the
predicate. Equivalent to #1(splitr p s).

[position s ss] splits the substring ss into a pair (pref, suff) of
substrings, where suff is the longest suffix of ss that has s as a
prefix and pref is the prefix of ss preceding suff. More precisely,
let m be the size of s and let ss correspond to the substring (s', i,
n). If there is a least index k >= i such that s = s'[k..k+m-1], then
suff corresponds to (s', k, n+i-k) and pref corresponds to (s', i,
k-i). If there is no such k, then suff is the empty substring
corresponding to (s', i+n, 0) and pref corresponds to (s', i, n),
i.e., all of ss.

[span (ss, ss')] produces a substring composed of a prefix ss, suffix
ss', plus all intermediate characters in the underlying string. It
raises Span if ss and ss' are not substrings of the same underlying
string or if the start of ss is to the right of the end of ss'. More
precisely, if we have ((s, i, n) = base ss) and ((s', i', n') = base
ss'), then span returns substring(s, i, (i'+n')-i) unless s <> s' or
i'+n' < i, in which case it raises Span. Note that this does not
preclude ss' from beginning to the left of ss, or ss from ending to
the right of ss'.  This function allows one to scan for a substring
using multiple pieces and then coalescing the pieces. When applied to
substrings derived from the identical base string, the string equality
test should be constant time. This can be achieved by first doing a
pointer test and, only if that fails, then checking the strings
character by character.

[translate f s] applies f to every character of s, from left to right,
and returns the concatenation of the results. This is equivalent to
String.concat(List.map f (explode s)).

[tokens f s] decomposes a substring into a list of tokens from left to
right. A token is a non-empty maximal substring not containing any
delimiter. A delimiter is a character satisfying predicate f.  Two
tokens may be separated by more than one delimiter. For example, if
the only delimiter is the character #"|", then the substring
"|abc||def" contains two tokens "abc" and "def".

[fields f s] decomposes a substring into a list of fields from left to
right. A field is a (possibly empty) maximal substring of s not
containing any delimiter. A delimiter is a character satisfying
predicate f. Two fields are separated by exactly one delimiter. For
example, if the only delimiter is the character #"|", then the
substring "|abc||def" contains the four fields "", "abc", "" and
"def".

[app f s] applies f to each character of s from left to
right. Equivalent to List.app f (explode s).

[foldl f a s] fold the function f over the substring s, starting with
the value a, from left to right. Equivalent to List.foldl f a (explode
s).

[foldr f a s] fold the function f over the substring s, starting with
the value a, from right to left. Equivalent to List.foldr f a (explode
s).

*)

