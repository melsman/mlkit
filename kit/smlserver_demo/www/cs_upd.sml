
val s = Option.valOf (ScsFormVar.wrapOpt ScsFormVar.getStringErr "text")
val id = Option.valOf (ScsFormVar.wrapOpt ScsFormVar.getStringErr "num")

val _ = Db.dml `update cs 
                set text = ^(Db.qq' s) 
                where id= ^id`

val _ = Ns.returnRedirect "cs_form.sml"

