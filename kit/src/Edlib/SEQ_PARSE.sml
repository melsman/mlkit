(*$SEQ_PARSE: GeneralTypes InstreamType *)

signature SEQ_PARSE =

(* PARSE AND READ FUNCTIONS FOR TYPES WITH ONE PARAMETER

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        7 Feb 1991

Maintenance:	Author


DESCRIPTION

   These functions form a general interface for reading and parsing
   one-dimensional constant sequences, such as lists and vectors.

   The conversion functions include the simple parse, read and string
   functions of the PARSE signature.  In addition there are versions
   that allow you to specify the start, finish and separating symbols.
   This lets yopu parse simple text layouts.  To parse something that is
   too complicated to be handled by the functions given here, try using
   ML-YACC and ML-LEX.

   The start and finish symbols must be single non-blank characters or
   empty strings.  If they are not empty, then the input must contain
   the appropriate characters.  If they are empty they are ignored.
   
   The separating symbol may be a non-blank character, an empty string,
   or a blank.  If it is a blank, then the elements of the sequence may
   be separated by zero or more formatting characters.  If it is a single
   non-blank character, then elements must be separated by that character
   plus an arbitrary amount of whitespace.  If the separating symbol is
   the empty string, then nothing seperates elements - for example, if
   the elements are bytes then formatting characters will be read as the
   appropriate byte values.

   Other versions of the functions let you specify the length of the
   sequence to be read.  This is especially useful when there is no
   finish symbol or when the finish symbol is the same as the separating
   symbol.

   
NOTES

   These functions were originally in the main SEQUENCE signature.


SEE ALSO

   SEQUENCE, PARSE, MONO_SEQ_PARSE.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:28  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.3  91/02/22  19:06:05  19:06:05  db (Dave Berry)
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.2  91/02/20  16:00:34  16:00:34  db (Dave Berry)
Added file and fromFile functions.

Revision 1.1  91/02/11  19:25:28  19:25:28  db (Dave Berry)
Initial revision


*)

sig

(* TYPES *)

  type 'a T


(* CONVERTORS *)

  exception Sep of string * string * string * string
   (* Sep (fn, start, finish, sep); raised if the function named fn is called
      with arguments start, finish and sep and one or more of these does not
      meet the requirements given above. *)

  exception Size of string * int
   (* Size (fn, i); raised when the function fn is invoked
      with a negative size. *)

  val parseSepN: string -> string -> string ->
		 (string -> ('a * string, 'b) EdlibGeneral.Result) -> int ->
		 string -> ('a T * string, 'a T EdlibGeneral.Option * string) EdlibGeneral.Result
   (* parseSepN start finish sep p n s; reads a sequence of 'a of length n
      that begins with start, ends with finish, in which the elements are
      separated by sep, and which forms a prefix of s, if there is one.
      Raises (Sep ("parseSep", start, finish, sep)) unless start, finish and
      sep fit the requirements listed above. *)

  val parseSep: string -> string -> string ->
                (string -> ('a * string, 'b) EdlibGeneral.Result) ->
                string -> ('a T * string, 'a T EdlibGeneral.Option * string) EdlibGeneral.Result
   (* parseSep start finish sep p s; reads a sequence of 'a that begins with
      start, ends with finish, in which the elements are separated by sep,
      and which forms a prefix of s, if there is one.
      Raises (Sep ("parseSep'", start, finish, sep))
      unless start, finish and sep fit the requirements listed above. *)

  val parse: (string -> ('a * string, 'b) EdlibGeneral.Result) ->
             string -> ('a T * string, 'a T EdlibGeneral.Option * string) EdlibGeneral.Result
   (* parse p s; reads a list of 'a that forms a prefix of s, if there
      is one, using default start, finish and separation symbols. *)

  val parseN: (string -> ('a * string, 'b) EdlibGeneral.Result) ->
              int -> string -> ('a T * string, 'a T EdlibGeneral.Option * string) EdlibGeneral.Result
   (* parse p n s; reads a list of 'a of length n that forms a prefix of s,
      if there is one, using default start, finish and separation symbols. *)

  val readSep: string -> string -> string ->
               (TextIO.instream -> ('a, 'b) EdlibGeneral.Result) -> TextIO.instream ->
               ('a T, 'a T EdlibGeneral.Option) EdlibGeneral.Result
   (* readSep start finish sep p i; reads a sequence of 'a that begins with
      start, ends with finish, in which the elements are separated by sep,
      from i, if it begins with one.
      Raises (Sep ("readSep", start, finish, sep))
      unless start, finish and sep fit the requirements listed above. *)

  val readSepN: string -> string -> string ->
               (TextIO.instream -> ('a, 'b) EdlibGeneral.Result) -> int -> TextIO.instream ->
               ('a T, 'a T EdlibGeneral.Option) EdlibGeneral.Result
   (* readSepN start finish sep p i; reads a sequence of 'a of length n that
      begins with start, ends with finish, in which the elements are separated
      by sep, from i, if it begins with one.
      Raises (Sep ("readSep", start, finish, sep))
      unless start, finish and sep fit the requirements listed above. *)

  val read: (TextIO.instream -> ('a, 'b) EdlibGeneral.Result) -> TextIO.instream ->
            ('a T, 'a T EdlibGeneral.Option) EdlibGeneral.Result
   (* read p i; reads a list of 'a from i, if it begins with one. *)

  val readN: (TextIO.instream -> ('a, 'b) EdlibGeneral.Result) -> int -> TextIO.instream ->
             ('a T, 'a T EdlibGeneral.Option) EdlibGeneral.Result
   (* read p n i; reads a list of 'a of length n from i, if it begins
      with one. *)

  val fromFile: (TextIO.instream -> ('a, 'b) EdlibGeneral.Result) -> string -> 'a T
   (* fromFile p name; read the contents of the file called name into a
      sequence.  Stops reading from the file as soon as p returns Fail.
      Raises Instream.Io if something goes wrong. *)

  val file: ('a -> string) -> 'a T -> string -> unit
   (* file p v name; write the contents of v to the new file called name.
      Raises Outstream.Io if something goes wrong. *)
end
