
val s = Option.valOf (FormVar.getString "text")
val id = Option.valOf (FormVar.getString "num")

val sql_upd = "update cs set text = '" ^  (Ns.Db.qq s) ^ "' where id='" ^ id ^ "'"

val _ = Ns.Db.dml sql_upd

val _ = Ns.returnRedirect "cs_form.sml"

