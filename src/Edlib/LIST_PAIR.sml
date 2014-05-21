(*$LIST_PAIR *)

signature LIST_PAIR =
sig

(* PAIRS OF LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author


DESCRIPTION

   Functions on the built-in type ('a list * 'b list).


SEE ALSO

   LIST, PAIR.

RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:13  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.4  91/01/26  13:42:37  13:42:37  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.3  91/01/25  16:55:19  16:55:19  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.2  91/01/24  17:06:24  17:06:24  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/17  16:49:21  16:49:21  db (Dave Berry)
Initial revision


*)

(* MANIPULATORS *)

   exception Zip
   val zip: 'a list * 'b list -> ('a * 'b) list
    (* zip (l1, l2); transforms two lists into a list of pairs. It
       raises Zip if the lists are of different length. *)

   val unzip: ('a * 'b) list -> ('a list * 'b list)
    (* unzip l; transforms a list of pairs (l) into a pair of lists. *)

   val unravel: 'a list -> ('a list * 'a list)
    (* unravel l; yields a pair of lists. The elements are taken from
       l alternating one element for the first list and the following
       element for the second. *)

   val interleave: 'a list * 'a list -> 'a list
    (* interleave l1 l2; yields a list created  alternating the elements
       of l1 with the elements of l2. *)

   val merge: ('a -> 'a -> bool) -> ('a list * 'a list) -> 'a list
    (* merge p l1 l2; As interleave, merge yields a list created
       alternating the elements of l1 with the elements of l2.
       The order of insertion of a particular pair is determined
       by the predicate p. *)
end
