(* invalid order of region and effect variables in type declarations *)

type t `[r1 e1 r2] = int #e1 -> string`r1 * string`r2
