(* Signature identifiers - Definition v3 page 10 *)

(*$SIGID*)
signature SIGID =
  sig
    eqtype sigid

    val mk_SigId: string -> sigid	(* NEW PARSER *)
    and pr_SigId : sigid -> string

    val < : sigid * sigid -> bool	(* Used for top-level printout *)
  end;
