signature OLD_IO = 
 sig
  type instream
  type outstream
  exception Io of string

  val std_in: instream
  val open_in: string -> instream
  val input: instream * int -> string
  val lookahead: instream -> string
  val close_in: instream -> unit
  val end_of_stream: instream -> bool

  val std_out: outstream
  val open_out: string -> outstream
  val output: outstream * string -> unit
  val close_out: outstream -> unit
 end

structure OldIO : OLD_IO =
 struct

   open TextIO
   exception Io of string
   fun hand f x = (f x) handle IO.Io {name,function,cause} => raise (Io (name ^ ": " ^ function)) 

   val std_in = stdIn
   val open_in = openIn
   val input = fn x => hand inputN x
   fun lookahead _ = raise (Io "lookahead not implemented.. edlib..")  
   val close_in = closeIn
   val end_of_stream = endOfStream

   val std_out = stdOut
   val open_out = fn x => hand openOut x
   val output = fn x => hand output x
   val close_out = closeOut
 end;
