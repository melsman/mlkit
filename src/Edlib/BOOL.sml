signature BOOL =
sig

(* BOOLEANS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

DESCRIPTION

   Standard functions on the built-in type "bool".


NOTES

   Possibly there should be read and write functions for binary form
   as well as ascii.  Portability issues would have to be addresses if
   this were so.

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:00:56  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.11  91/03/06  16:28:42  16:28:42  db (Dave Berry)
Added print function(s).

Revision 1.10  91/02/12  12:17:51  12:17:51  db (Dave Berry)
Changed type to eqtype.

Revision 1.9  91/02/11  18:14:29  18:14:29  db (Dave Berry)
Removed read, parse and comparison functions, and the Object substructure,
as part of the major reorganisation of the library.

Revision 1.8  91/02/04  15:38:26  15:38:26  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.7  91/01/31  17:48:43  17:48:43  db (Dave Berry)
Added type.

Revision 1.6  91/01/30  18:07:21  18:07:21  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.5  91/01/25  19:30:10  19:30:10  db (Dave Berry)
Added dependence on OBJECT, fixed include specification.

Revision 1.4  91/01/25  19:01:52  19:01:52  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.3  91/01/25  16:54:18  16:54:18  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:05:42  17:05:42  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:45:31  16:45:31  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  eqtype bool

  val not: bool -> bool


(* TYPES *)

  eqtype T
    sharing type T = bool


(* CONVERTERS *)

  val string: bool -> string

  val print: TextIO.outstream -> bool -> unit


(* OBSERVERS *)

  val eq: bool -> bool -> bool

  val ne: bool -> bool -> bool

  val fixedWidth: bool
   (* fixedWidth = false. *)


(* MANIPULATORS *)

  (* infix 1 or *)
  val or: bool * bool -> bool
   (* or (x, y); the standard logic function. *)

  (* infix 2 & *)
  val & : bool * bool -> bool
   (* x & y; the standard logic function. *)

  (* infix 3 implies *)
  val implies : bool * bool -> bool
   (* x implies y; the standard logic function. *)

end
