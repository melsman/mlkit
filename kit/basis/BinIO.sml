(* BinIO -- SML Basis Library *)

structure BinIO : BIN_IO =
  struct

    exception CannotOpen
    type elem   = Word8.word
    type vector = Word8Vector.vector

    (* The only way BinIO.instream and BinIO.outstream differ from
     * TextIO.instream and TextIO.outstream is in the way they were
     * opened.  Hence we call on the TextIO functions to implement most of
     * the BinIO functions too (except openIn, openOut, openAppend, of
     * course.  Some `conversion' functions:
     *)

    fun fromString (s: string) : vector = Byte.stringToBytes s
    fun toString (v : vector) : string = Byte.bytesToString v

    fun fromChar (c : char) : elem = Byte.charToByte c
    fun toChar (e : elem) : char = Byte.byteToChar e

    (* Binary input: *)

    type instream = TextIO.instream (*={ic: int, name : string}*)

    fun raiseIo fcn nam exn = 
      raise IO.Io {function = fcn, name = nam, cause = exn}

    fun openIn (f: string) : instream = 
      {ic=prim ("openInBinStream", (f, CannotOpen)), 
       name=f} handle exn as CannotOpen => raiseIo "openIn" f exn

    fun closeIn (is : instream) : unit = 
      TextIO.closeIn is

    fun input (is : instream) : vector = 
      fromString (TextIO.input is)

    fun inputAll (is : instream) : vector =
      fromString (TextIO.inputAll is)

    fun inputNoBlock (is : instream) : vector option =
      raise Fail "not implemented"

    fun input1 (is : instream) : elem option =
      case TextIO.input1 is of
	NONE   => NONE
      | SOME c => SOME (fromChar c)

    fun inputN (is : instream, n : int) : vector =
      fromString (TextIO.inputN (is, n))

    fun endOfStream (is : instream) : bool = TextIO.endOfStream is

    fun lookahead (is : instream) : elem option =
      case TextIO.lookahead is of
	NONE   => NONE
      | SOME c => SOME (fromChar c);
    

    (* Binary output: *)

    type outstream = TextIO.outstream (* = {oc: int, name : string} *)

    fun openOut(f: string): outstream = 
      {oc=prim ("openOutBinStream", (f, CannotOpen)), name=f} 
      handle exn as CannotOpen => raiseIo "openOut" f exn

    fun openAppend(f: string): outstream =
      {oc=prim ("openAppendBinStream", (f, CannotOpen)), name=f} 
      handle exn as CannotOpen => raiseIo "openAppend" f exn

    fun closeOut (os : outstream) : unit = TextIO.closeOut os

    local
      fun raiseIo fcn nam exn = 
	raise IO.Io {function = fcn^"", name = nam^"", cause = exn} 
      fun output0(os as {oc,name},str:vector,function):unit =
	(prim ("outputBinStream", (oc, str, IO.ClosedStream));
	 if os = TextIO.stdErr then TextIO.flushOut os else ())
	handle exn as IO.ClosedStream => raiseIo function name exn
    in
      fun output(os : outstream, vec : vector) : unit =
	output0(os,vec,"output")
    end

    fun output1(os : outstream, w : elem) : unit =
      TextIO.output1(os, toChar w)

    fun flushOut(os : outstream) : unit = TextIO.flushOut os

  end