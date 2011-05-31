signature CHAR =
  sig
    eqtype char
    eqtype string

    val minChar : char
    val maxChar : char
    val maxOrd : int

    val ord : char -> int
    val chr : int -> char
    val succ : char -> char
    val pred : char -> char

    val compare : char * char -> order
    val <  : char * char -> bool
    val <= : char * char -> bool
    val >  : char * char -> bool
    val >= : char * char -> bool

    val contains : string -> char -> bool
    val notContains : string -> char -> bool

    val isAscii : char -> bool
    val toLower : char -> char
    val toUpper : char -> char
    val isAlpha : char -> bool
    val isAlphaNum : char -> bool
    val isCntrl : char -> bool
    val isDigit : char -> bool
    val isGraph : char -> bool
    val isHexDigit : char -> bool
    val isLower : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isUpper : char -> bool

    val toString : char -> String.string
    val scan       : (Char.char, 'a) StringCvt.reader
                       -> (char, 'a) StringCvt.reader
    val fromString : String.string -> char option
    val toCString : char -> String.string
    val fromCString : String.string -> char option
  end

(*
Description

val minChar : char

    The least character in the ordering. It always equals chr 0.

val maxChar : char

    The greatest character in the ordering; it equals chr maxOrd.

val maxOrd : int

    The greatest character code; it equals ord maxChar.

ord c

    returns the (non-negative) integer code of the character c.

chr i

    returns the character whose code is i; raises Chr if i < 0 or i >
    maxOrd.

succ c

    returns the character immediately following c in the ordering, or
    raises Chr if c = maxChar. When defined, succ c is equivalent to
    chr(ord c + 1).

pred c

    returns the character immediately preceding c, or raises Chr if c
    = minChar. When defined, pred c is equivalent to chr(ord c - 1).

compare (c, d)

    returns LESS, EQUAL, or GREATER, depending on whether c precedes,
    equals, or follows d in the character ordering.

val < : char * char -> bool
val <= : char * char -> bool
val > : char * char -> bool
val >= : char * char -> bool

    These compare characters in the character ordering. Note that the
    functions ord and chr preserve orderings. For example, if we have
    x < y for characters x and y, then it is also true that ord x <
    ord y.

contains s c

    returns true if character c occurs in the string s; otherwise it
    returns false.

notContains s c

    returns true if character c does not occur in the string s; it
    returns false otherwise. It is equivalent to not(contains s c).

isAscii c

    returns true if c is a (seven-bit) ASCII character, i.e., 0 <= ord
    c <= 127. Note that this function is independent of locale.

toLower c
toUpper c

    These return the lowercase (respectively, uppercase) letter
    corresponding to c if c is a letter; otherwise it returns c.

isAlpha c

    returns true if c is a letter (lowercase or uppercase).

isAlphaNum c

    returns true if c is alphanumeric (a letter or a decimal digit).

isCntrl c

    returns true if c is a control character.

isDigit c

    returns true if c is a decimal digit [0-9].

isGraph c

    returns true if c is a graphical character, that is, it is
    printable and not a whitespace character.

isHexDigit c

    returns true if c is a hexadecimal digit [0-9a-fA-F].

isLower c

    returns true if c is a lowercase letter.

isPrint c

    returns true if c is a printable character (space or visible),
    i.e., not a control character.

isSpace c

    returns true if c is a whitespace character (space, newline, tab,
    carriage return, vertical tab, formfeed).

isPunct c

    returns true if c is a punctuation character: graphical but not
    alphanumeric.

isUpper c

    returns true if c is an uppercase letter.

toString c

    returns a printable string representation of the character, using,
    if necessary, SML escape sequences. Printable characters, except
    for #"\\" and #"\"", are left unchanged. Backslash #"\\" becomes
    "\\\\"; double quote #"\"" becomes "\\\"". The common control
    characters are converted to two-character escape sequences:

    Alert (ASCII 0x07)                 "\\a"
    Backspace (ASCII 0x08)             "\\b"
    Horizontal tab (ASCII 0x09)        "\\t"
    Linefeed or newline (ASCII 0x0A)   "\\n"
    Vertical tab (ASCII 0x0B)          "\\v"
    Form feed (ASCII 0x0C)             "\\f"
    Carriage return (ASCII 0x0D)       "\\r"

    The remaining characters whose codes are less than 32 are
    represented by three-character strings in ``control character''
    notation, e.g., #"\000" maps to "\\^@", #"\001" maps to "\\^A",
    etc. For characters whose codes are greater than 999, the
    character is mapped to a six-character string of the form
    "\\uxxxx", where xxxx are the four hexadecimal digits
    corresponding to a character's code. All other characters (i.e.,
    those whose codes are greater than 126 but less than 1000) are
    mapped to four-character strings of the form "\\ddd", where ddd
    are the three decimal digits corresponding to a character's code.

    To convert a character to a length-one string containing the
    character, use the function String.str.

scan getc strm
fromString s

    These scan a character (including possibly a space) or an SML
    escape sequence representing a character from the prefix of a
    character stream or a string of printable characters, as allowed
    in an SML program. After a successful conversion, scan returns the
    remainder of the stream along with the character, whereas
    fromString ignores any additional characters in s and just returns
    the character. If the first character is non-printable (i.e., not
    in the ASCII range [0x20,0x7E]) or starts an illegal escape
    sequence (e.g., "\q"), no conversion is possible and NONE is
    returned. The function fromString is equivalent to
    StringCvt.scanString scan.

    The allowable escape sequences are:
    \a 	Alert (ASCII 0x07)
    \b 	Backspace (ASCII 0x08)
    \t 	Horizontal tab (ASCII 0x09)
    \n 	Linefeed or newline (ASCII 0x0A)
    \v 	Vertical tab (ASCII 0x0B)
    \f 	Form feed (ASCII 0x0C)
    \r 	Carriage return (ASCII 0x0D)
    \\ 	Backslash
    \" 	Double quote
    \^c A control character whose encoding is ord c - 64, with the
    	character c having ord c in the range [64,95]. For example,
    	\^H (control-H) is the same as \b (backspace).
    \ddd The character whose encoding is the number ddd, three decimal
    	digits denoting an integer in the range [0,255].
    \uxxxx The character whose encoding is the number xxxx, four
    	hexadecimal digits denoting an integer in the ordinal range of
    	the alphabet.
    \f...f\ This sequence is ignored, where f...f stands for a
    	sequence of one or more formatting (space, newline, tab, etc.)
    	characters.

    In the escape sequences involving decimal or hexadecimal digits,
    if the resulting value cannot be represented in the character set,
    NONE is returned. As the table indicates, escaped formatting
    sequences (\f...f\) are passed over during scanning. Such
    sequences are successfully scanned, so that the remaining stream
    returned by scan will never have a valid escaped formatting
    sequence as its prefix.

    Here are some sample conversions:

      Input string s 	fromString s
      "\\q"               NONE
      "a\^D"              SOME #"a"
      "a\\ \\\q"          SOME #"a"
      "\\ \\"             NONE
      ""                  NONE
      "\\ \\\^D"          NONE
      "\\ a"              NONE

toCString c

    returns a printable string corresponding to c, with non-printable
    characters replaced by C escape sequences. Specifically, printable
    characters, except for #"\\", #"\"", #"?", and #"'" are left
    unchanged.  Backslash (#"\\") becomes "\\\\"; double quote (#"\"")
    becomes "\\\"", question mark (#"?") becomes "\\?", and single
    quote (#"'") becomes "\\'".  The common control characters are
    converted to two-character escape sequences: Alert (ASCII 0x07)
    "\\a" Backspace (ASCII 0x08) "\\b" Horizontal tab (ASCII 0x09)
    "\\t" Linefeed or newline (ASCII 0x0A) "\\n" Vertical tab (ASCII
    0x0B) "\\v" Form feed (ASCII 0x0C) "\\f" Carriage return (ASCII
    0x0D) "\\r"

    All other characters are represented by three octal digits,
    corresponding to a character's code, preceded by a backslash.

fromCString s

    scans a character (including possibly a space) or a C escape
    sequence representing a character from the prefix of a
    string. After a successful conversion, fromCString ignores any
    additional characters in s. If no conversion is possible, e.g., if
    the first character is non-printable (i.e., not in the ASCII range
    [0x20-0x7E] or starts an illegal escape sequence, NONE is
    returned.

    The allowable escape sequences are given below (cf. Section
    6.1.3.4 of the

    ISO C standard ISO/IEC 9899:1990[CITE]).
    \a 	Alert (ASCII 0x07)
    \b 	Backspace (ASCII 0x08)
    \t 	Horizontal tab (ASCII 0x09)
    \n 	Linefeed or newline (ASCII 0x0A)
    \v 	Vertical tab (ASCII 0x0B)
    \f 	Form feed (ASCII 0x0C)
    \r 	Carriage return (ASCII 0x0D)
    \? 	Question mark
    \\ 	Backslash
    \" 	Double quote
    \' 	Single quote
    \^c A control character whose encoding is ord c - 64, with the
    	character c having ord c in the range [64,95]. For example,
    	\^H (control-H) is the same as \b (backspace).
    \ooo The character whose encoding is the number ooo, where ooo
    	consists of one to three octal digits
    \xhh The character whose encoding is the number hh, where hh is a
    	sequence of hexadecimal digits.

    Note that fromCString accepts an unescaped single quote character,
    but does not accept an unescaped double quote character.

    In the escape sequences involving octal or hexadecimal digits, the
    sequence of digits is taken to be the longest sequence of such
    characters. If the resulting value cannot be represented in the
    character set, NONE is returned.
*)
