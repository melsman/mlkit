(*$LIST_HACKS*)
(* LIST_HACKS provides operations to treat lists as sets. This is temporary,
   since the elaboration phases haven't been ported to use the SML Library's
   sets yet. *)

signature LIST_HACKS =
  sig
    val union: ''a list * ''a list -> ''a list
    val intersect: ''a list * ''a list -> ''a list
    val minus: ''a list * ''a list -> ''a list
    val eqSet: ''a list * ''a list -> bool
  end;
