(*vpprob.sml  27/08/1997 13:06. tho.*)

(*gives Impossible: StatObject.TyVar.equality; ought only to give the warning
 ``Free type variables are not allowed ...''.  For instance, if everyting
 is put in a let, there is no problem.*)

    fun i x = x
      
    val revonto = i i
    val mk_nextgen_fn = revonto
