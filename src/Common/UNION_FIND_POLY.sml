(*
 *			
 *	File:     UNION_FIND_POLY.sml
 *	Author:   Lars Birkedal (birkedal@diku.dk)
 *	Created:  Wed May 12 15:26:35 MET DST 1993
 *	Modified: Fri Apr 29 13:37:13 MET DST 1994
 *
 *	Contents: Polymorphic Union Find 
 *	          Tarjan, ``Data Structures and Network Algorithms'', 1983
 *
 *)

(*$UNION_FIND_POLY: *)
signature UNION_FIND_POLY =
sig
  type 'info Element  (* a reference *)
  val eq_Elements : 'info Element * 'info Element -> bool

  val mkElement : '_info -> '_info Element 
  exception UnionFind
  val find  : 'info Element -> 'info Element 
  val union : ('info * 'info -> 'info) ->  
              ('info Element * 'info Element) -> 'info Element
    (* The second argument to the info-combine function is the info of
     * the new canonical element. The elements must be canonical.
     *)

  and get_info  : 'info Element -> 'info          (* The element must be canonical *)
  and set_info  : 'info Element -> 'info -> unit  (* The element must be canonical *)
  val find_info : 'info Element -> 'info          (* The element need not be canonical *)
  val find_rep_and_info : 'info Element -> 'info Element * 'info  (* The argument element need 
                                                     not be canonical *)

  val pu : 'a -> 'a Pickle.pu -> 'a Element Pickle.pu
      (* the first argument is a dummy value to make the pickler work for
       * cyclic data structures. *)
end;
