(*$BASIC_IO*)
signature BASIC_IO =
  sig
    val dot: unit -> unit
    val print: string -> unit
    val println: string -> unit
    val print': TextIO.outstream -> string -> unit
    val println': TextIO.outstream -> string -> unit

    val withSpace: ('a -> unit) -> ('a -> unit)
    val withNewline: ('a -> unit) -> ('a -> unit)

    val withDot: ('a -> 'b) -> 'a -> 'b

    val open_in: string -> TextIO.instream
    val input: TextIO.instream * int -> string
    val close_in: TextIO.instream -> unit
  end;
