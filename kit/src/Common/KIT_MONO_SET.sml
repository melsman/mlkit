
(* Modified version of the MONO_SET signature given in the Edinburgh
   library. *)

signature KIT_MONO_SET =
  sig
    type elt
    type Set

    val empty : Set
    val singleton : elt -> Set

    val size : Set -> int
    val isEmpty : Set -> bool
    val member : elt -> Set -> bool
    val eq : Set -> Set -> bool

    val list : Set -> elt list
    val fromList : elt list -> Set 
    val addList : elt list -> Set -> Set
      (* addList l s : Add elements in list l to s. *)

    val insert : elt -> Set -> Set
    val remove : elt -> Set -> Set
    val difference : Set -> Set -> Set
    val intersect : Set -> Set -> Set
    val union : Set -> Set -> Set
    val partition : (elt -> bool) -> Set -> Set * Set

    val subst : elt * elt -> Set -> Set
      (* subst (a,b) s : Substitute element b in s with element a. *)

    val fold : (elt -> 'b -> 'b) -> 'b -> Set -> 'b
      (* fold f base s; folds using f over the base element. *)

    val map : (elt -> elt) -> Set -> Set
      (* map f s; builds a new set by applying f to each element in s *)

    val apply : (elt -> unit) -> Set -> unit
      (* apply f s; applies f to each element of s (in order) *)

    type StringTree

    val layoutSet : {start: string, sep: string, finish: string} ->
      (elt -> StringTree) -> Set -> StringTree
  end;
