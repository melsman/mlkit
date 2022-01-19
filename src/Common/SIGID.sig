(* Signature identifiers *)

signature SIGID =
  sig
    eqtype sigid

    val mk_SigId: string -> sigid	(* NEW PARSER *)
    and pr_SigId : sigid -> string

    val < : sigid * sigid -> bool	(* Used for top-level printout *)

    val pu : sigid Pickle.pu

    structure Map : MONO_FINMAP where type dom = sigid

  end
