val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val (user_imp_id,errs) = ScsUserImp.getUserImpIdErr ("user_imp_id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors

val _ =
    Db.dml `delete from scs_user_imports
             where scs_user_imports.user_imp_id = '^(Int.toString user_imp_id)'`

val _ = Ns.returnRedirect "imp_form.sml"
