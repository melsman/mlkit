val print_id = ScsFormVar.wrapPanic ScsError.panic (ScsFormVar.wrapIntAsString ScsFormVar.getNatErr) "print_id"
val (target_url,errs) = ScsFormVar.wrapMaybe ScsFormVar.getStringErr ("target_url", "target url",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val _ = ScsDb.toggleP "scs_print_log" "print_id" "deleted_p" print_id
val _ = if (target_url <> "") 
	  then Ns.returnRedirect target_url
	else Ns.returnRedirect "log.sml"

