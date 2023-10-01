(* Functor identifiers *)

structure FunId :> FUNID =
  struct
    type funid = string

    fun mk_FunId x = x
    fun pr_FunId x = x

    val op < = fn (str1:string, str2) => str1 < str2

    val pu = Pickle.string

    structure Map = StringFinMap
  end
