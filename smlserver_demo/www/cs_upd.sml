
val s = Option.valOf (FormVar.getString "text")
val id = Option.valOf (FormVar.getString "num")

val _ = Db.dml `update cs 
                set text = ^(Db.qq' s) 
                where id= ^id`

val _ = Ns.returnRedirect "cs_form.sml"

