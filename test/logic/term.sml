(* term.sml *)

structure Term =
struct
  datatype term
    = STR of string * term list
    | INT of int31
    | CON of string
    | REF of term option ref

  exception BadArg of string
end;
