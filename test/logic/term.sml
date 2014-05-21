(* term.sml *)

structure Term = 
struct
  datatype term
    = STR of string * term list
    | INT of int
    | CON of string
    | REF of term option ref
	
  exception BadArg of string
end;

