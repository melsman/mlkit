val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val (source,errs) = ScsUserImp.getExtSourceErr ("source",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val _ =
  ScsError.wrapPanic
  Db.dml `delete from scs_user_imports
           where on_what_table = ^(Db.qqq source)`

val _ = Ns.returnRedirect "imp_form.sml"