(* An imported consumer fixes the ordinary representation of this datatype.
   A separate private group in the client can still specialise it. *)
datatype boundary_box = BoundaryBox of int ref * int
fun consume__noinline (BoundaryBox(r,_)) : int = prim("!", r)

datatype boundary_owner = BoundaryOwner of boundary_box
fun consumeOwner__noinline (BoundaryOwner x) = consume__noinline x
