(*$PAIR *)

signature PAIR =
sig

(* PAIRS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		4 Oct 1989

Maintenance:	Author


DESCRIPTION

   Functions on the built-in type ('a * 'b).

   The usual comparison operators are omitted, because they're all the
   same as binary (or binaryPair).  For example,
     fun lt Int.lt (x, y) (x', y') = binary Int.lt (x, y) (x', y').


SEE ALSO

   LIST_PAIR, PAIR_PARSE.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:24  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.9  91/03/06  16:29:38  16:29:38  db (Dave Berry)
Added print function(s).

Revision 1.8  91/02/11  19:17:23  19:17:23  db (Dave Berry)
Moved read and parse functions to PAIR_PARSE.sml as part of the major
reorganisation of the library.

Revision 1.7  91/02/04  15:38:48  15:38:48  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.6  91/01/30  18:07:40  18:07:40  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.5  91/01/26  13:43:52  13:43:52  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.4  91/01/25  19:02:51  19:02:51  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.3  91/01/25  16:55:31  16:55:31  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:08:12  17:08:12  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:54:26  16:54:26  db (Dave Berry)
Initial revision


*)


(* CREATORS *)

  val create: 'a -> 'b -> ('a * 'b)
   (* create x y; yields (x, y). *)


(* CONVERTORS *)

  val string: ('a -> string) -> ('b -> string) -> ('a * 'b) -> string

  val print: TextIO.outstream -> (TextIO.outstream -> 'a -> unit) ->
	     (TextIO.outstream -> 'b -> unit) -> ('a * 'b) -> unit


(* MANIPULATORS *)

  val swap: ('a * 'b) -> ('b * 'a)
   (* swap (x, y); returns (y, x). *)

  val apply: ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
   (* apply f (x,y); yields (f x, f y). *)

  val applyPair: ('a -> 'b) * ('c -> 'd) -> ('a * 'c) -> ('b * 'd)
   (* applyPair (f, g) (x, y); yields (f x, g y). *)

  val binary: ('a -> 'b -> 'c) -> ('a * 'a) -> ('b * 'b) -> ('c * 'c)
   (* binary f (x, y) (x', y'); yields (f x x', f y y'). *)

  val binaryPair: ('a -> 'b -> 'c) * ('d -> 'e -> 'f) ->
		  ('a * 'd) -> ('b * 'e) -> ('c * 'f)
   (* binaryPair (f, g) (x, y) (x', y'); yields (f x x', g y y'). *)

  val tee: ('a -> 'b) * ('a -> 'c) -> 'a -> ('b * 'c)
   (* tee f (x, y); yields (f x, f y). *)

end
