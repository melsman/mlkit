(* Functor identifiers - Definition v3 page 10 *)

(*$FunId: FUNID*)
functor FunId(): FUNID =
  struct
    datatype funid = FUNID of string

    val mk_FunId = FUNID
    fun pr_FunId(FUNID str) = str

    val op < = fn (FUNID str1, FUNID str2) => str1 < str2
  end;
