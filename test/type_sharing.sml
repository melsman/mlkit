
(* The following two signatures should elaborate *)

signature S1 =
  sig 
    type t
    sharing type t = t
  end where type t = int

signature S2 =
  sig 
    type t
    sharing type t = t
    sharing type t = t
  end
