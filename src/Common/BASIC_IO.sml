(*$BASIC_IO*)
signature BASIC_IO =
  sig
    val dot: unit -> unit
    val print: string -> unit
    val println: string -> unit
    val print': outstream -> string -> unit
    val println': outstream -> string -> unit

    val withSpace: ('a -> unit) -> ('a -> unit)
    val withNewline: ('a -> unit) -> ('a -> unit)

    val withDot: ('a -> 'b) -> 'a -> 'b

    val open_in: string -> instream
    val input: instream * int -> string
    val close_in: instream -> unit
  end;
