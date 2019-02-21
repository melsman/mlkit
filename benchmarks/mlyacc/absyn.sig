(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1  2001/02/14 16:28:07  mael
 * added mlyacc to test
 *
 * Revision 1.1.1.1  1996/01/31  16:01:44  george
 * Version 109
 * 
 *)

signature ABSYN =
    sig
       datatype exp = EVAR of string
                    | EAPP of exp * exp
                    | ETUPLE of exp list
                    | EINT of int
                    | FN of pat * exp
                    | LET of decl list * exp
                    | UNIT
                    | SEQ of exp * exp
                    | CODE of string
       and      pat = PVAR of string
                    | PAPP of string * pat
                    | PTUPLE of pat list
                    | PLIST of pat list
                    | PINT of int
                    | WILD
                    | AS of pat * pat
       and     decl = VB of pat * exp
       and     rule = RULE of pat * exp
       val printRule : ((string -> unit) * (string -> unit)) -> rule -> unit
    end
