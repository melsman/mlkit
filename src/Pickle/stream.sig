(* Stream module
 * Copyright, Martin Elsman 2003-01-07 
 *)

signature STREAM =
  sig
    type IN and OUT
    type 'k stream
    type loc = word

    val getLoc : 'k stream -> loc 
    val out    : char * OUT stream -> OUT stream
    val get    : IN stream -> char * IN stream

    val outw   : Word32.word * OUT stream -> OUT stream
    val getw   : IN stream -> Word32.word * IN stream

    val outcw  : Word32.word * OUT stream -> OUT stream
    val getcw  : IN stream -> Word32.word * IN stream

    val toString : OUT stream -> string
    val openOut  : unit -> OUT stream
    val openIn   : string -> IN stream
  end
