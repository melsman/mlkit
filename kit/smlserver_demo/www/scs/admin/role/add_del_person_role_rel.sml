val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (person_id,errs) = ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val (role_id,errs) = ScsFormVar.getRoleIdErr("role_id",errs)
val (mode,errs) = ScsFormVar.getEnumErr ["add","del"] ("mode", "Mode", errs)
val (target,errs) = 
  ScsFormVar.getStringErr ("target", ScsDict.s UcsDict.url_dict, errs)
val _ = ScsFormVar.anyErrors errs

val _ = 
  if mode = "add" then
    Db.execSp [`scs_role.add(party_id => ^(Int.toString person_id),
                             role_id => ^(Int.toString role_id))`]
  else
    Db.execSp [`scs_role.remove(party_id => ^(Int.toString person_id),
                                role_id => ^(Int.toString role_id))`]

val _ = Ns.returnRedirect target

val _ = ScsRole.flushRoleCache()
