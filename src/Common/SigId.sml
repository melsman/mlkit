(* Signature identifiers *)

structure SigId: SIGID =
  struct
    datatype sigid = SIGID of string

    val mk_SigId = SIGID
    fun pr_SigId(SIGID str) = str

    val op < = fn (SIGID str1, SIGID str2) => str1 < str2

    val pu = Pickle.convert (SIGID, fn SIGID s => s) Pickle.string

    structure Map = OrderFinMap(struct type t = sigid
				       val lt = op<
				end)

  end
