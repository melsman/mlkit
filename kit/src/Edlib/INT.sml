signature INT =
sig

(* INTEGERS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author


DESCRIPTION

   Standard functions on the built-in type "int".


NOTES

   The arithmetic exceptions are defined to raise Overflow for all
   overflow operations and Div for attempts to divide by zero.

   Possibly there should be functions dv, md and dvMd (with exceptions Dv
   and Md) that select the fastest of rem, mod, etc. for positive numbers.

   Possibly the function -- should raise an exception if start > finish + 1.

   Possibly there should be functions to read and write integers in binary
   as well as ascii.  Their specification would have say something about
   portability.

   Possibly there should be functions to read and write integers (to/from
   ascii) in different bases.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:09  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.13  91/09/13  16:41:33  16:41:33  db (Dave Berry)
Added dependency on GeneralTypes.

Revision 1.12  91/03/06  16:28:57  16:28:57  db (Dave Berry)
Added print function(s).

Revision 1.11  91/02/22  16:45:00  16:45:00  db (Dave Berry)
Renamed **! exception to Power.

Revision 1.10  91/02/12  12:18:02  12:18:02  db (Dave Berry)
Changed type to eqtype.

Revision 1.9  91/02/11  18:38:46  18:38:46  db (Dave Berry)
Removed read and parse functions, and the Object sub-structure, as part
of the major reorganisation of the library.

Revision 1.8  91/02/04  15:38:36  15:38:36  db (Dave Berry)
Renamed InStream and OutStream to Instream/instream and OutStream/outstream,
as part of the reorganisation of the stream entries.

Revision 1.7  91/01/31  17:48:45  17:48:45  db (Dave Berry)
Added type.

Revision 1.6  91/01/30  18:07:26  18:07:26  db (Dave Berry)
Changed parse functions to return the unread part of the string.
Removed the parse' functions.

Revision 1.5  91/01/25  19:30:44  19:30:44  db (Dave Berry)
Added dependence on OBJECT, fixed include specification.

Revision 1.4  91/01/25  19:02:24  19:02:24  db (Dave Berry)
Added dependence on InStreamType and/or GeneralTypes.

Revision 1.3  91/01/25  16:55:05  16:55:05  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:19  17:06:19  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:48:35  16:48:35  db (Dave Berry)
Initial revision


*)


(* PERVASIVES *)

  eqtype int

  exception Overflow
  and Div

  val + : int * int -> int
  val - : int * int -> int
  val * : int * int -> int
  val div: int * int -> int
  val mod: int * int -> int
  val ~ : int -> int
  val abs: int -> int
  val real: int -> real


(* SYSTEM *)

  val minInt: int option
   (* minInt; the smallest integer that can be stored on the system, or
      None if the system supports arbitrary length integers. *)

  val maxInt: int option
   (* maxInt; the largest integer that can be stored on the system, or
      None if the system supports arbitrary length integers. *)


(* TYPES *)

  eqtype T
    sharing type T = int


(* CONVERTERS *)

  val string: int -> string

  val print: TextIO.outstream -> int -> unit


(* OBSERVERS *)

  val eq: int -> int -> bool

  val ne: int -> int -> bool

  val lt: int -> int -> bool

  val le: int -> int -> bool

  val gt: int -> int -> bool

  val ge: int -> int -> bool

  val fixedWidth: bool
   (* fixedWidth = false. *)


(* MANIPULATORS *)

  (* infix 7 divMod *)
  val divMod: int * int -> int * int
   (* x divMod y = (x div y, x mod y). *)

  (* infix 7 quot rem quotRem *)

  val quot: int * int -> int
   (* x quot y; like x div y but rounding toward zero. *)

  val rem: int * int -> int
   (* x rem y; like x mod y but rounding toward zero. *)

  val quotRem: int * int -> int * int
   (* x quotRem y = (x quot y, x rem y). *)

  val max: int -> int -> int
   (* max x y; returns the greater of x and y.  *)

  val min: int -> int -> int
   (* min x y; returns the lesser of x and y.  *)

  val maxMin: int -> int -> int * int
   (* maxMin x y = (max (x, y), min (x, y)) .  *)

  (* infix 5 -- *)
  val -- : int * int -> int list
   (* x -- y; returns the list of integers between x and y inclusive.
      Returns nil if x > y.  *)

  (* infix 8 ** *)
  exception Power of int * int
  val ** : int * int -> int
   (* x ** y; x raised to the power y. *)

end
