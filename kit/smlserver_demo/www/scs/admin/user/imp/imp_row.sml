val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val (person_id,errs) = 
  ScsFormVar.wrapMaybe_nh 0 ScsFormVar.getIntErr("person_id","Person Id",ScsFormVar.emptyErr)
val (user_imp_id,errs) = ScsUserImp.getUserImpIdErr("user_imp_id",errs)
val _ = ScsFormVar.anyErrors errs

val _ =
  if person_id = 0 then
    (* Create new user *)
    ScsError.wrapPanic
    Db.execSp [`scs_user.imp_row(user_imp_id => ^(Int.toString user_imp_id),
                                 always_p => 't')`]
  else
    ScsError.wrapPanic
    Db.execSp [`scs_user.imp_row_into_user(user_imp_id => ^(Int.toString user_imp_id),
                                           user_id => ^(Int.toString person_id))`]

val _ = Ns.returnRedirect "imp_form.sml"
