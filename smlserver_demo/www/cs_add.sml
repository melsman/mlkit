val s = Option.valOf (FormVar.wrapOpt FormVar.getStringErr "text")

val _ = Db.dml `insert into cs (text, id) 
                values (^(Db.qq' s), 
                        ^(Db.seqNextvalExp "cs_seq"))`

val _ = Ns.returnRedirect "cs_form.sml"
