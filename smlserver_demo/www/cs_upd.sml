
val s = Option.valOf (FormVar.wrapOpt FormVar.getStringErr "text")
val id = Option.valOf (FormVar.wrapOpt FormVar.getStringErr "num")

val _ = Db.dml `update cs 
                set text = ^(Db.qq' s) 
                where id= ^id`

val _ = Ns.returnRedirect "cs_form.sml"

