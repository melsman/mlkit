val user_id = ScsLogin.auth_roles ["SiteAdm"]

val d = ScsDict.d ScsLang.en "scs/admin/user" "imp_row.sml"
val dl = ScsDict.dl ScsLang.en "scs/admin/user" "imp_row.sml"

val (person_id,errs) = 
  ScsFormVar.wrapMaybe_nh 0 ScsFormVar.getIntErr("person_id",d"Person id",ScsFormVar.emptyErr)
val (user_imp_id,errs) = ScsFormVar.getIntErr("user_imp_id",d"User import id",errs)
val _ = ScsFormVar.anyErrors

val _ =
  if person_id = 0 then
    (* Create new user *)
    Db.execSp [`scs_user.imp_row(user_imp_id => ^(Int.toString user_imp_id),
                                 always_p => 't')`]
  else
    Db.execSp [`scs_user.imp_row_into_user(user_imp_id => ^(Int.toString user_imp_id),
                                           user_id => ^(Int.toString person_id))`]

val _ = Ns.returnRedirect "imp_form.sml"
