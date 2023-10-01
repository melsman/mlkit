(** Basic operations on boolean values.

The Bool structure provides some basic operations on boolean values.
*)

signature BOOL =
  sig
    datatype bool = datatype bool
    val not : bool -> bool
    val toString   : bool -> string
    val scan       : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
    val fromString : string -> bool option
  end

(**

[type bool] is considered a primitive type and is defined in the
top-level environment. It is rebound here for consistency.

[not b] returns the logical negation of the boolean value b.

[toString b] returns the string representation of b, either "true" or
"false".

[scan getc strm] Scans a character source for a boolean value. Takes a
character stream reader getc and a stream strm. Ignoring case and
initial whitespace, the sequences "true" and "false" are converted to
the corresponding boolean values. On successful scanning of a boolean
value, scan returns SOME(b, rest), where b is the scanned value and
rest is the remaining character stream.

[fromString s] Scans a boolean from a string s. It returns SOME(b) for
a scanned value b; otherwise it returns NONE. Equivalent to
StringCvt.scanString scan.

[Discussion] In addition to the not function presented here, the
language defines the special operators andalso and orelse, which
provide short-circuit evaluation of the AND and OR of two boolean
expressions. The semantics of strict AND and OR operators, which would
evaluate both expressions before applying the operator, are rarely
needed and can easily be obtained using the andalso and orelse
operators.

*)
