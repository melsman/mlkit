(*$LIST_SORT *)

signature LIST_SORT =
sig

(* FUNCTIONS TO SORT AND PERMUTE LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		6 Feb 1991

Maintenance:	Author


DESCRIPTION

   Functions to sort and permute elements of a list.


NOTES

   These functions were originally in the main list structure.


SEE ALSO

   LIST.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:14  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/02/11  18:43:28  18:43:28  db (Dave Berry)
Initial revision



*)


(* MANIPULATORS *)

  val perms: 'a list -> 'a list list
   (* perms l; returns a list whose elements are all the permutations of l*)

  val sort: ('a -> 'a -> bool) -> 'a list -> 'a list
   (* sort p l; returns l sorted by p. *)


end

