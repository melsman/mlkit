(* Functor identifiers - Definition v3 page 10 *)

(*$FUNID*)

signature FUNID =
  sig
    eqtype funid

    val mk_FunId: string -> funid	(* NEW PARSER *)
    and pr_FunId: funid -> string

    val < : funid * funid -> bool	(* Used for top-level printout *)
  end;
