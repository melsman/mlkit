(*$PAIR_PARSE: InstreamType GeneralTypes *)

signature PAIR_PARSE =
sig

(* PARSE FUNCTIONS FOR PAIRS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		8 Feb 1991

Maintenance:	Author


DESCRIPTION

   Parse and read functions on the built-in type ('a * 'b).


NOTES

   These functions were originally in the main PAIR signature.


SEE ALSO

   PAIR.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:24  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/02/11  19:18:36  19:18:36  db (Dave Berry)
Initial revision



*)



(* CONVERTORS *)

  val parse: (string -> ('a * string, 'c EdlibGeneral.Option * string) EdlibGeneral.Result) ->
  	     (string -> ('b * string, 'd EdlibGeneral.Option * string) EdlibGeneral.Result) ->
	     string -> (('a * 'b) * string, 'e EdlibGeneral.Option * string) EdlibGeneral.Result
   (* parse p1 p2 s; parses a pair from the beginning of s, using p1 and p2
      to parse the two elements. *)

  val read: (TextIO.instream -> ('a, 'c) EdlibGeneral.Result) ->
	    (TextIO.instream -> ('b, 'd) EdlibGeneral.Result) ->
	    TextIO.instream -> ('a * 'b, unit) EdlibGeneral.Result
   (* read p1 p2 i; reads a pair from i, using p1 and p2 to parse the
      two elements. *)

end
