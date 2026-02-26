(* invalid order of region and effect variables in type declarations *)

type `[r1 e1 r2] t = int #e1 -> string`r1 * string`r2
