val sql_insert = "insert into cs (id, text) values (cs_seq.nextval, 'æøåÅØÆ dejligt')"
val _ = Ns.Db.dml sql_insert

val _ = Ns.returnRedirect "cs_form.sml"
	
