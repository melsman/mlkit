(*$Instream: INSTREAM General *)

structure Instream: INSTREAM =

(* INPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


SEE ALSO

   Outstream, StreamPair


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:10  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.6  91/02/11  21:24:45  21:24:45  db (Dave Berry)
Now uses the pervasive implementations of streams.  I now believe that
any problems with end_of_stream on interactive streams, or error
messages, are the responsibility of the compiler writer.

Revision 1.5  91/02/05  11:07:49  11:07:49  db (Dave Berry)
Changed input, lookahead and eof so that and end of stream indication
is treated like a character on the stream.  So lookahead recognises it
but doesn't consume it, input recognises it and consumes it, and eof
is defined to be (lookahead i = "") as in the Definition of SML.
This behaviour fits that described in the Definition, and still allows
parsing of polymorphic vectors terminated by end_of_stream.

Revision 1.4  91/02/04  16:59:25  16:59:25  db (Dave Berry)
Renamed InStream to instream, and similarly for structure and signature ids.
Renamed input' to read.
Added Io exception.

Revision 1.3  91/01/25  20:17:16  20:17:16  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:21:15  17:21:15  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:53:37  14:53:37  db (Dave Berry)
Initial revision


*)

struct

  open OldIO
  open OldString
  open EdlibGeneral

(* TYPES *)

  type instream = TextIO.instream


(* SYSTEM *)

  exception NotImplemented of string

  fun openString s = raise NotImplemented "openString"
  fun canInput i n = raise NotImplemented "canInput"
  fun reset i = raise NotImplemented "reset"
  fun interactive i = raise NotImplemented "interactive"
  fun read i n = raise NotImplemented "read"


(* MANIPULATORS *)

  exception Io = Io

  val stdIn = std_in
  val std_in = stdIn

  val openIn = open_in
  val open_in = openIn

  val closeIn = close_in
  val close_in = closeIn

  val input = input

  fun input1 i = input (i, 1)

  val lookahead = lookahead

  val end_of_stream = end_of_stream
  val eof = end_of_stream

  local
    fun readString' _ [] = OK ()
    |   readString' i (h::t) =
	  case input1 i
	  of "" => Fail ""
	  |  c  =>
	      if h = c then 
		case readString' i t of
		  OK () => OK ()
		| Fail s => Fail (c ^ s)
	      else
		Fail c
  in
    fun readString i s = readString' i (explode s)
  end

  fun skip p i =
	case lookahead i
	of "" => ()
	|  s  => if p s then (input1 i; skip p i) else ()

  local
    fun line i s =
        case input (i, 1) of
           ""  => s
        | "\n" => s ^ "\n"
        |   c  => line i (s ^ c)
  in
    fun inputLine i = line i ""
  end
end
