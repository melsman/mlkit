signature CORE_VECTOR =

(* CORE VECTOR FUNCTIONS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           24 Jan 1991

Maintenance:    Author


DESCRIPTION

   This is the implementation of vectors agreed between the implementors
   of SML/NJ, Poly/ML and Poplog ML in Autumn 1990.  The main library
   adds more functionality.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:00  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/01/25  11:29:48  11:29:48  db (Dave Berry)
Initial revision


*)

sig

  eqtype 'a vector

  exception Size

  exception Subscript

  val empty : 'a vector  (* 09/09/1996-Martin *)

  val vector: 'a list -> 'a vector

  val tabulate: int * (int -> 'a) -> 'a vector

  val sub: 'a vector * int -> 'a

  val length: 'a vector -> int
end

