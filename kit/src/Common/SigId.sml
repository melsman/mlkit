(* Signature identifiers - Definition v3 page 10 *)

(*$SigId: SIGID*)
functor SigId(): SIGID =
  struct
    datatype sigid = SIGID of string

    val mk_SigId = SIGID
    fun pr_SigId(SIGID str) = str

    val op < = fn (SIGID str1, SIGID str2) => str1 < str2
  end;
