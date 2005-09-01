val rs = FormVar.emptyErr
val (n,rs) = FormVar.getStringErr("n", "Name", rs)
val (c,rs) = FormVar.getStringErr("c", "Comment", rs)
val (e,rs) = FormVar.getEmailErr("e", "Email", rs)
val _ = FormVar.anyErrors rs

val _ = Db.dml 
  `insert into guest (name,email,comments)
   values (^(Db.qqq n),^(Db.qqq e),^(Db.qqq c))`

val _ = Web.returnRedirect "guest.sml"
               
