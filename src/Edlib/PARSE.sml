(*$PARSE: InstreamType GeneralTypes *)

signature PARSE =
sig

(* PARSE AND READ FUNCTIONS FOR SIMPLE TYPES

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        4 Feb 1991

Maintenance:	Author


DESCRIPTION

   This signature defines a type T and functions for reading and parsing
   values of that type.


SEE ALSO

   OBJECT, SEQ_PARSE, MONO_SEQ_PARSE.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:25  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/02/11  19:20:15  19:20:15  db (Dave Berry)
Initial revision


*)


(* TYPES *)

  type T


(* CONVERTERS *)

  val parse:  string -> (T * string, T EdlibGeneral.Option * string) EdlibGeneral.Result
   (* parse s; parses value of type T from the beginning of s. *)

  val read: TextIO.instream -> (T, T EdlibGeneral.Option) EdlibGeneral.Result
   (* read i; reads value of type T from i. *)


(* OBSERVERS *)

  val fixedWidth: bool
   (* fixedWidth; is true if the usual string representation of type T uses
      a fixed number of characters for all values. *)

end
