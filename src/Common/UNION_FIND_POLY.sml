(*
 *			
 *	File:     UNION_FIND_POLY.sml
 *	Author:   Lars Birkedal (birkedal@diku.dk)
 *	Created:  Wed May 12 15:26:35 MET DST 1993
 *	Modified: Fri Apr 29 13:37:13 MET DST 1994
 *      Modified: mael 2004-08-18: avoid problems with non-canonical 
 *                elements by making it impossible to distinguish between
 *                elements that are unioned. A find-operation is still
 *                present however for memory usage purposes.
 *
 *	Contents: Polymorphic Union Find 
 *	          Tarjan, ``Data Structures and Network Algorithms'', 1983
 *
 *)

signature UNION_FIND_POLY =
sig
  type 'info Element  (* a reference *)
  val eq_Elements : 'info Element * 'info Element -> bool

  val mkElement : '_info -> '_info Element 
  val find  : 'info Element -> 'info Element 
  val union : ('info * 'info -> 'info) ->  
              ('info Element * 'info Element) -> 'info Element
    (* The second argument to the info-combine function is the info of
     * the new element.
     *)

  val set_info  : 'info Element -> 'info -> unit
  val find_info : 'info Element -> 'info
  val find_rep_and_info : 'info Element -> 'info Element * 'info

  val pu : 'a -> 'a Pickle.pu -> 'a Element Pickle.pu
      (* the first argument is a dummy value to make the pickler work for
       * cyclic data structures. *)
end;
