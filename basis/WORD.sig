signature WORD =
  sig
    eqtype word

    val wordSize : int

    val toLarge      : word -> word64
    val toLargeX     : word -> word64
    val toLargeWord  : word -> word64
    val toLargeWordX : word -> word64
    val fromLarge     : word64 -> word
    val fromLargeWord : word64 -> word
    val toLargeInt  : word -> intinf
    val toLargeIntX : word -> intinf
    val fromLargeInt : intinf -> word
    val toInt  : word -> int
    val toIntX : word -> int
    val fromInt : int -> word

    val andb : word * word -> word
    val orb  : word * word -> word
    val xorb : word * word -> word
    val notb : word -> word
    val << : word * Initial.word0 -> word
    val >> : word * Initial.word0 -> word
    val ~>> : word * Initial.word0 -> word

    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word

    val compare : word * word -> order
    val <  : word * word -> bool
    val <= : word * word -> bool
    val >  : word * word -> bool
    val >= : word * word -> bool

    val ~ : word -> word
    val min : word * word -> word
    val max : word * word -> word

    val fmt      : StringCvt.radix -> word -> string
    val toString : word -> string
    val scan     : StringCvt.radix
	-> (char, 'a) StringCvt.reader
	-> (word, 'a) StringCvt.reader
    val fromString : string -> word option
end

(*
Description

val wordSize : int

    The number of bits in type word. wordSize need not be a power of
    two. Note that word has a fixed, finite precision.

toLarge w
toLargeX w

    These convert w to a value of type LargeWord.word. In the first
    case, w is converted to its equivalent LargeWord.word value in the
    range [0,2(wordSize)-1]. In the second case, w is
    ``sign-extended,'' i.e., the wordSize low-order bits of w and
    toLargeX w are the same, and the remaining bits of toLargeX w are
    all equal to the most significant bit of w.

    toLargeWord and toLargeWordX are respective synonyms of the first
    two, and are deprecated.

fromLarge w
fromLargeWord w

    These functions convert w to the value w(mod (2(wordSize))) of
    type word. This has the effect of taking the low-order wordSize
    bits of the 2's complement representation of w.

    fromLargeWord is a deprecated synonym for fromLarge.

toLargeInt w
toLargeIntX w

    These convert w to a value of type LargeInt.int. In the former
    case, w is viewed as an integer value in the range
    [0,2(wordSize)-1]. In the latter case, w is treated as a 2's
    complement signed integer with wordSize precision, thereby having
    a value in the range [-2(wordSize-1),2(wordSize-1)-1]. toLargeInt
    raises Overflow if the target integer value cannot be represented
    as a LargeInt.int. Since the precision of LargeInt.int is always
    at least wordSize (see the discussion below), toLargeIntX will
    never raise an exception.

fromLargeInt i

    converts i of type LargeInt.int to a value of type word. This has
    the effect of taking the low-order wordSize bits of the 2's
    complement representation of i.

toInt w
toIntX w

    These convert w to a value of default integer type. In the former
    case, w is viewed as an integer value in the range
    [0,2(wordSize)-1]. In the latter case, w is treated as a 2's
    complement signed integer with wordSize precision, thereby having
    a value in the range [-2(wordSize-1),2(wordSize-1)-1]. They raise
    Overflow if the target integer value cannot be represented as an
    Int.int.

fromInt i

    converts i of the default integer type to a value of type
    word. This has the effect of taking the low-order wordSize bits of
    the 2's complement representation of i. If the precision of
    Int.int is less than wordSize, then i is sign-extended to wordSize
    bits.

val andb : word * word -> word
val orb : word * word -> word
val xorb : word * word -> word

    These functions return the bit-wise AND, OR, and exclusive OR,
    respectively, of their arguments.

notb i

    returns the bit-wise complement (NOT) of i.

<< (i, n)

    shifts i to the left by n bit positions, filling in zeros from the
    right. When i and n are interpreted as unsigned binary numbers,
    this returns (i* 2(n))(mod (2 (wordSize))). In particular,
    shifting by greater than or equal to wordSize results in 0. This
    operation is similar to the ``(logical) shift left'' instruction
    in many processors.

>> (i, n)

    shifts i to the right by n bit positions, filling in zeros from
    the left. When i and n are interpreted as unsigned binary numbers,
    it returns floor((i / 2(n))). In particular, shifting by greater
    than or equal to wordSize results in 0. This operation is similar
    to the ``logical shift right'' instruction in many processors.

~>> (i, n)

    shifts i to the right by n bit positions. The value of the
    leftmost bit of i remains the same; in a 2's-complement
    interpretation, this corresponds to sign extension. When i is
    interpreted as a wordSize-bit 2's-complement integer and n is
    interpreted as an unsigned binary number, it returns floor((i /
    2(n))). In particular, shifting by greater than or equal to
    wordSize results in either 0 or all 1's. This operation is similar
    to the ``arithmetic shift right'' instruction in many processors.

i + j

    returns (i+j)(mod (2 (wordSize))) when i and j are interpreted as
    unsigned binary numbers. It does not raise Overflow.

i - j

    returns the difference of i and j modulo (2(wordSize)):

        (2(wordSize) + i - j)(mod (2(wordSize)))

    when i and j are interpreted as unsigned binary numbers. It does
    not raise Overflow.

i * j

    returns the product (i*j)(mod (2(wordSize))) when i and j are
    interpreted as unsigned binary numbers. It does not raise
    Overflow.

i div j

    returns the truncated quotient of i and j, floor((i / j)), when i
    and j are interpreted as unsigned binary numbers. It raises Div
    when j = 0.

i mod j

    returns the remainder of the division of i by j:

        i - j * floor((i / j))

    when i and j are interpreted as unsigned binary numbers. It raises
    Div when j = 0.

compare (i, j)

    returns LESS, EQUAL, or GREATER if and only if i is less than,
    equal to, or greater than j, respectively, considered as unsigned
    binary numbers.

val < : word * word -> bool
val <= : word * word -> bool
val > : word * word -> bool
val >= : word * word -> bool

    These return true if and only if the input arguments satisfy the
    given relation when interpreted as unsigned binary numbers.

~ i

    returns the 2's complement of i.

val min : word * word -> word
val max : word * word -> word

    These return the smaller (respectively, larger) of the arguments.

fmt radix i
toString i

    These return a string containing a numeric representation of i. No
    prefix "Ow", "OwX", etc. is generated. The version using fmt
    creates a representation specified the given radix. The
    hexadecimal digits in the range [10,15] are represented by the
    characters #"A" through #"F". The version using toString is
    equivalent to fmt StringCvt.HEX i.

scan radix getc strm
fromString s

    These functions scan a word from a character source. In the first
    version, if an unsigned number in the format denoted by radix can
    be parsed from a prefix of the character strm strm using the
    character input function getc, the expression evaluates to
    SOME(w,rest), where w is the value of the number parsed and rest
    is the remainder of the character stream. Initial whitespace is
    ignored. NONE is returned otherwise. It raises Overflow when a
    number can be parsed, but is too large to fit in type word.

    The format that scan accepts depends on the radix
    argument. Regular expressions defining these formats are as
    follows:

               Radix            Format
	       StringCvt.BIN    (0w)?[0-1]+
	       StringCvt.OCT    (0w)?[0-7]+
	       StringCvt.DEC    (0w)?[0-9]+
	       StringCvt.HEX    (0wx | 0wX | 0x | 0X)?[0-9a-fA-F]+

    The fromString version returns SOME(w) if an unsigned hexadecimal
    number in the format (0wx | 0wX | 0x | 0X)?[0-9a-fA-F]+ can be
    parsed from a prefix of string s, ignoring initial whitespace,
    where w is the value of the number parsed. NONE is returned
    otherwise. This function raises Overflow when a hexadecimal
    numeral can be parsed, but is too large to be represented by type
    word. It is equivalent to

	    StringCvt.scanString (scan StringCvt.HEX)
*)
