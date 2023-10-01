(* Signature identifiers *)

structure SigId :> SIGID =
  struct
    type sigid = string

    fun mk_SigId x = x
    fun pr_SigId x = x

    val op < = fn (str1:string, str2) => str1 < str2

    val pu = Pickle.string

    structure Map = StringFinMap
  end
