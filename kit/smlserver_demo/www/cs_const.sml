val sql_insert = "insert into cs (id, text) values (" ^ Db.seqNextvalExp("cs_seq") ^ ", 'æøåÅØÆ dejligt')"
val _ = Db.dml sql_insert

val _ = Ns.returnRedirect "cs_form.sml"
	
