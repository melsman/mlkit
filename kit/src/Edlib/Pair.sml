(*$Pair : PAIR *)

structure Pair: PAIR =


(* FUNCTIONS ON PAIRS.

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk

Date:		1 Nov 1989

Maintenance:	Author


DESCRIPTION

   Functions on the built-in type 'a * 'b.


SEE ALSO

   ListPair, PairParse.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:26  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.9  91/03/06  16:38:31  16:38:31  db (Dave Berry)
Added print function(s).

Revision 1.8  91/02/11  20:32:08  20:32:08  db (Dave Berry)
Moved the read and parse functions to PairParse.sml, as part of the major
reorganisation of the library.

Revision 1.7  91/02/05  11:06:12  11:06:12  db (Dave Berry)
Changed read functions slightly to use new definition of Instream.eof.

Revision 1.6  91/02/04  15:10:51  15:10:51  db (Dave Berry)
InStream and OutSream renamed to Instream and OutStream, as part of  the
reorganisation of the stream entries.

Revision 1.5  91/01/30  17:43:03  17:43:03  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed parse' functions.

Revision 1.4  91/01/25  20:19:24  20:19:24  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/25  15:44:06  db
Used functions defined in CoreUtils structure where appropriate.

Revision 1.2  91/01/24  17:25:19  17:25:19  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:02:48  15:02:48  db (Dave Berry)
Initial revision


*)

struct


(* CREATORS *)

  fun create x y = (x, y)


(* CONVERTERS *)

  fun string p1 p2 (x, y) = "(" ^ p1 x ^ ", " ^ p2 y ^ ")"

  fun print os p1 p2 (x, y) =
      ( TextIO.output (os, "(");
	p1 os x;
	TextIO.output (os, ", ");
	p2 os y;
	TextIO.output (os, ")")
      );


(* MANIPULATORS *)

  fun swap (x, y) = (y, x)

  fun apply f (x, y) = (f x, f y)

  fun applyPair (f, g) (x, y) = (f x, g y)

  fun tee (f, g) x = (f x, g x)

  fun binary f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

  fun binaryPair (f, g) (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

end

