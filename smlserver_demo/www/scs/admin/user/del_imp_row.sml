val user_id = ScsLogin.auth_roles ["SiteAdm"]

val d = ScsDict.d ScsLang.en "scs/admin/user" "del_imp_row.sml"
val dl = ScsDict.dl ScsLang.en "scs/admin/user" "del_imp_row.sml"

val (user_imp_id,errs) = ScsFormVar.getIntErr("user_imp_id",d"User import id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors

val _ =
    Db.dml `delete from scs_user_imports
             where scs_user_imports.user_imp_id = '^(Int.toString user_imp_id)'`

val _ = Ns.returnRedirect "imp_form.sml"
