(* Old.sml 1995-02-24, 1996-04-09 -- compatibility with the Definition *)

signature SML90 =
  sig
    val sqrt : real -> real             (* Math *)
    val sin : real -> real
    val cos : real -> real
    val arctan : real -> real
    val exp : real -> real
    val ln : real -> real

    val chr : int -> string             (* Strings *)
    val ord : string -> int
    val explode : string -> string list
    val implode : string list -> string

    exception Abs and Diff and Exp and Floor and Neg and Prod and Sum
      and Mod and Quot and Sqrt and Ln and Ord and Io of string and
      Interrupt

    type instream and outstream         (* Input/output *)

    val std_in : instream
    val open_in : string -> instream
    val input : instream * int -> string
    val lookahead : instream -> string
    val close_in : instream -> unit
    val end_of_stream : instream -> bool
    val std_out : outstream;
    val open_out : string -> outstream
    val output : outstream * string -> unit
    val close_out : outstream -> unit
  end

structure SML90 : SML90 =
  struct

    exception Abs   = Overflow
          and Diff  = Overflow
          and Exp   = Overflow
          and Floor = Overflow
          and Neg   = Overflow
          and Prod  = Overflow
          and Sum   = Overflow
          and Quot  = Overflow
          and Mod   = Div
    exception Interrupt
    exception Sqrt
    exception Ord
    exception Ln
    exception Io of string

    fun sqrt r = if r < 0.0 then raise Sqrt else Math.sqrt r
    fun sin r = Math.sin r
    fun cos r = Math.cos r
    fun arctan r = Math.atan r
    fun exp r = 
      let val r' = Math.exp r
      in if Real.==(Real.posInf, r') orelse Real.isNan r' then raise Exp
	 else r'
      end
    fun ln r = if r <= 0.0 then raise Ln else Math.ln r

    fun chr i = Char.toString(Char.chr i)
    fun ord s = Char.ord(String.sub(s,0)) handle _ => raise Ord
    fun implode ss = String.concat ss
    fun explode s = map Char.toString (String.explode s)

    local 
      val exn_open_in = Io "open_in : cannot open file"
      val exn_input = Io "input: stream is closed"
      val exn_open_out = Io "open_out : cannot open file"
      val exn_output = Io "output: stream is closed"
    in
      type instream = TextIO.instream 
      and outstream = TextIO.outstream

      val std_in = TextIO.stdIn

      fun open_in s = TextIO.openIn s handle _ => raise exn_open_in
      fun input(is, n) = TextIO.inputN (is, n) handle _ => raise exn_input
      fun lookahead is = 
	case TextIO.lookahead is
	  of NONE => ""
	   | SOME c => Char.toString c 
      fun close_in is = TextIO.closeIn is
      fun end_of_stream is = TextIO.endOfStream is
      val std_out = TextIO.stdOut
      fun open_out s = TextIO.openOut s handle _ => raise exn_open_out
      fun output(os, s) = TextIO.output(os, s) handle _ => raise exn_output
      fun close_out os = TextIO.closeOut os
    end
  end
