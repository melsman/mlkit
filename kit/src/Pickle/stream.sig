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

    val outw   : word * OUT stream -> OUT stream
    val getw   : IN stream -> word * IN stream

    val outcw  : word * OUT stream -> OUT stream
    val getcw  : IN stream -> word * IN stream

    val toString : OUT stream -> string
    val openOut  : unit -> OUT stream
    val openIn   : string -> IN stream
  end
