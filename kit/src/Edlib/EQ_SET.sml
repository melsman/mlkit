(*$EQ_SET *)

signature EQ_SET =
sig

(* SETS OVER EQUALITY TYPES

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Jan 1991

Maintenance:    Author


DESCRIPTION

   This is the simplest of the three structures that implement sets.
   It provides polymorphic functions that are restricted to equality types.


NOTES

   An alternative name would be EQTYPE_SET, but that might be more appropriate
   for the case where 'a Set was itself an equality type.

SEE ALSO

   MONO_SET, SET.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:04  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.3  1991/10/22  18:22:35  db
Added map, apply, fold and fold' functions.

Revision 1.2  91/01/24  17:31:28  17:31:28  db (Dave Berry)
Removed version value.

Revision 1.1  91/01/22  18:53:15  18:53:15  db (Dave Berry)
Initial revision


*)

(* TYPES *)

  type 'a Set


(* CONSTANTS *)

  val empty: 'a Set
   (* empty; the empty set. *)


(* CREATORS *)

  val singleton: 'a -> 'a Set
   (* singleton x; returns the set containing only x. *)


(* CONVERTERS *)

  val list: 'a Set -> 'a list
   (* list s; return a list of the elements of s. *)

  val fromList: ''a list -> ''a Set
   (* fromList l; return the set of elements of l, removing duplicates. *)


(* OBSERVERS *)

  val size: 'a Set -> int
   (* size s; the number of elements in s. *)

  val isEmpty: 'a Set -> bool
   (* isEmpty s; returns true if s is empty, false otherwise. *)

  val member: ''a -> ''a Set -> bool
   (* member x s; returns true is x is in s, false otherwise. *)

  val eq: ''a Set -> ''a Set -> bool
   (* eq s s'; returns true if s and s' have the same elements. *)


(* SELECTORS *)

  exception Empty of string
   (* Empty fn; raised if the function named fn is erronously applied to
      the empty set. *)

  val select: 'a Set -> ('a * 'a Set)
   (* select s; returns a pair consiting of an element of s and the set
      of the remaining elements. *)


(* MANIPULATORS *)

  val difference: ''a Set -> ''a Set -> ''a Set
   (* difference s s'; returns the set of those elements of s that aren't
      also in s'.  *)

  val insert: ''a -> ''a Set -> ''a Set
   (* insert x s; returns the union of s and {x}. *)

  val intersect: ''a Set -> ''a Set -> ''a Set
   (* intersect s s'; returns the set of those elements that are in
      both s and s'. *)

  val remove: ''a -> ''a Set -> ''a Set
   (* remove x s; returns the set of the elements of s with x removed. *)

  val partition: (''a -> bool) -> ''a Set -> (''a Set * ''a Set)
   (* partition p s; returns a pair of sets; the first containing the elements
      of s for which the predicate p is true, the second the elements of s
      for which p is false. *)

  val union: ''a Set -> ''a Set -> ''a Set
   (* union s s'; returns the set of elements that are in either or both s
      and s'. *)

  val closure: (''a -> ''a Set) -> ''a Set -> ''a Set
   (* closure f s; repeatedly applies f to elements of s and the elements
      of the results of such applications, until no further elements are
      generated. *)


(* ITERATORS *)

  val map: ('a -> 'b) -> 'a Set -> 'b Set
   (* map f s; builds a set by applying f to each element of s. *)

  val apply: ('a -> unit) -> 'a Set -> unit
   (* apply f s; applies f to each element of s. *)


(* REDUCERS *)

  val fold: ('a -> 'b -> 'b) -> 'b -> 'a Set -> 'b
   (* fold f s base; folds using f over the base element. *)

  val fold': ('a -> 'a -> 'a) -> 'a Set -> 'a
   (* fold' f s; folds using f over an arbitrary element of s.
      Raises (Empty "fold'") if s is empty. *)

end;
