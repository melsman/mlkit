(*$INSTREAM: GeneralTypes *)

signature INSTREAM =
sig

(* INPUT STREAMS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        12 Nov 1989

Maintenance:	Author


DESCRIPTION

   Functions on input streams.


SEE ALSO

   OUTSTREAM, STREAM_PAIR


NOTES

   The pervasive values std_in, open_in, close_in and end_of_stream have
   been given synonyms that fit the library conventions.  The original names
   are still available.

   The input1 function is included to handle the common case of reading a
   single character; this function will remain valid even if input is
   changed to be curried.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:08  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.5  91/02/04  16:42:06  16:42:06  db (Dave Berry)
Renamed InStream to instream, and similarly for structure and signature ids.
Renamed input' to read.

Revision 1.4  91/01/26  13:41:46  13:41:46  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.3  91/01/25  19:02:21  19:02:21  db (Dave Berry)
Added dependence on instreamType and/or GeneralTypes.

Revision 1.2  91/01/25  16:55:08  16:55:08  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.1  90/12/17  16:48:15  16:48:15  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  type instream = TextIO.instream

  exception Io of string

  val std_in: instream
  val open_in: string -> instream
  val close_in: instream -> unit
  val input: instream * int -> string
  val lookahead: instream -> string
  val end_of_stream: instream -> bool


(* SYSTEM *)

  val openString: string -> instream
   (* openString s; returns an instream.  The characters read from this
      stream will be those in s, in order, with the end of s being read as
      an end of file. *)

  val canInput: instream -> int -> bool
   (* canInput i n; returns true if n characters can be read from i
      without blocking. *)

  val reset: instream -> bool
   (* reset i; if i can be reset to the beginning, in some sense,
     this is done and true is returned.  Otherwise false is returned. *)

  val interactive: instream -> bool
   (* interactive i; returns true if i is associated with an interactive
      device. *)


(* MANIPULATORS *)


  val stdIn: instream
   (* stdIn = std_in *)

  val openIn: string -> instream
   (* openIn = open_in *)

  val closeIn: instream -> unit
   (* closeIn = close_in *)

  val eof: instream -> bool
   (* eof = end_of_stream *)

  val read: instream -> int -> string
   (* read i n; returns the empty string if there are no characters
      remaining in i before an end of file.  Otherwise it returns a string
      of at least 1 and not more than n characters, whatever input is
      available from i. *)

  val input1: instream -> string
   (* input1 i; returns the first character on the instream i, or the empty
      string if an end of file is read.  Blocks if there is no character
      or end of file to be read. *)

  val readString: instream -> string -> (unit, string) EdlibGeneral.Result
   (* readString i s; returns OK () if reading from i gives the characters
      in s.  Returns (Fail s') as soon as the end of file is reached or a
      character is read that doesn't match the corresponding one in s, where
      s' is the characters read so far.  In either case all characters read
      from i are lost.  *)

  val skip: (string -> bool) -> instream -> unit
   (* skip p i; reads all characters from i that satisfy p.  Leaves the first
      character that doesn't satisfy p to be read.  *)

  val inputLine : instream -> string
   (* inputLine i; returns a strings consisting of characters read from i
      up to and including the next end of line character.  If the end of the
      file is reached first, all characters up to the end of file are returned
      (without a new line character). *)
end

