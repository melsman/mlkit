val s = Option.valOf (FormVar.getString "text")

val sql_insert = "insert into cs (text, id) values ('" ^ (Db.qq s) ^ "'," ^ (Db.seqNextvalExp "cs_seq") ^ ")"
val _ = Db.dml sql_insert

val _ = Ns.returnRedirect "cs_form.sml"
