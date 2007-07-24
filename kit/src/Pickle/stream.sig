(* Stream module
 * Copyright, Martin Elsman 2003-01-07 
 * GPL Licence
 *)

signature STREAM =
  sig
    type instream
    type outstream
    type loc = word

    val getLocIn  : instream -> loc 
    val getLocOut : outstream -> loc 	

    val out    : char * outstream -> outstream
    val get    : instream -> char * instream

    val outw1    : word8 * outstream -> outstream
    val getw1    : instream -> word8 * instream

    val outw   : word * outstream -> outstream
    val getw   : instream -> word * instream

    val outcw  : word * outstream -> outstream
    val getcw  : instream -> word * instream

    val outcw2  : word * outstream -> outstream
    val getcw2  : instream -> word * instream

    val outw32  : Word32.word * outstream -> outstream
    val getw32  : instream -> Word32.word * instream

    val outcw32 : Word32.word * outstream -> outstream
    val getcw32 : instream -> Word32.word * instream

    val toString : outstream -> string
    val openOut  : unit -> outstream
    val openIn   : string -> instream
  end

(* [outw1(w,os)] output the one bit w&0w1 to the stream os.

   [getw1(w,o)] read one bit from stream is.
*)
