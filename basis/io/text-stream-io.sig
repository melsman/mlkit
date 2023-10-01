(** Operations on text streams. *)

signature TEXT_STREAM_IO =
   sig
      include STREAM_IO
         where type elem = Char.char
         where type vector = CharVector.vector

      val inputLine    : instream -> (string * instream) option
      val outputSubstr : outstream * substring -> unit
   end

(**

[inputLine is] reads from the input stream is andreturns an optional
result of a string and a continuation input stream.

[outputSubstr os ss] outputs the substring ss on the outstream os.

*)
