(* This is runtime stuff: I/O streams. We represent streams as numbers, and
   let the prelude abstract them into the I/O stream type. Input and output
   streams are distinct (i.e. they might use the same numbers); 0 means
   both std_in and std_out. *)

(*$IO_STREAMS*)
signature IO_STREAMS =
  sig
   (* A set of current streams (rather than having implicit state in the
      package, although that's unavoidable because of the "real" I/O
      streams open in the host package. *)

    type Streams
    val initialStreams: Streams

   (* Create and register a new stream - note the failure function: *)

    val openIn: Streams -> (string * (unit -> 'a)) -> int * Streams
    val openOut: Streams -> (string * (unit -> 'a)) -> int * Streams

   (* Get the "real" stream associated with a stream number; needed for
      reading, writing, and closing. . *)

    val inputStream: Streams -> int -> instream
    val outputStream: Streams -> int -> outstream
  end;
