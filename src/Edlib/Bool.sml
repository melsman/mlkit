structure Bool: BOOL =

(* BOOLEANS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		22 Sep 1989

Maintenance:	Author

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:00:58  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.7  91/03/06  16:38:06  16:38:06  db (Dave Berry)
Added print function(s).

Revision 1.6  91/02/11  19:52:23  19:52:23  db (Dave Berry)
Removed Object sub-structure.  Added type synonym T, string function and
equality functions.  This forms part of the major reorganisation of the
library.

Revision 1.5  91/01/31  17:47:24  17:47:24  db (Dave Berry)
Added type.

Revision 1.4  91/01/30  19:01:04  19:01:04  db (Dave Berry)
Renamed loadFun and loadStr to loadEntry.

Revision 1.3  91/01/25  20:16:09  20:16:09  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:20:40  17:20:40  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:48:25  14:48:25  db (Dave Berry)
Initial revision


*)

struct


(* PERVASIVES *)

  type bool = bool

  val not = not


(* TYPES *)

  type T = bool


(* CONVERTERS *)

  fun string true = "true"
  |   string false = "false"

  fun print os x = TextIO.output (os, string x)


(* OBSERVERS *)

  fun eq x y = (x = y)

  fun ne x y = (x <> y)

  val fixedWidth = false


(* MANIPULATORS *)

  infix 1 or
  fun x or y = x orelse y

  infix 2 &
  fun x & y = x andalso y

  infix 3 implies
  fun x implies y = not x orelse y

end
