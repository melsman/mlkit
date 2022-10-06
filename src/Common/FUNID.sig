(* Functor identifiers *)

signature FUNID =
  sig
    eqtype funid

    val mk_FunId: string -> funid	(* NEW PARSER *)
    and pr_FunId: funid -> string

    val < : funid * funid -> bool	(* Used for top-level printout *)

    val pu : funid Pickle.pu

    structure Map : MONO_FINMAP where type dom = funid
  end
