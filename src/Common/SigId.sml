(* Signature identifiers - Definition v3 page 10 *)

(*$SigId: SIGID*)
functor SigId(): SIGID =
  struct
    datatype sigid = SIGID of string

    val mk_SigId = SIGID
    fun pr_SigId(SIGID str) = str

    fun (SIGID str1) < (SIGID str2) = AsciiOrdString.lt str1 str2
  end;
