val user_id = ScsLogin.auth_roles ["SiteAdm"]

val (person_id,errs) = ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val (role_id,errs) = ScsFormVar.getRoleIdErr("role_id",errs)
val _ = ScsFormVar.anyErrors errs

val _ = Db.execSp [`scs_role.remove(party_id => ^(Int.toString person_id),
                                    role_id => ^(Int.toString role_id))`]

val _ = Ns.returnRedirect (Html.genUrl "edit_person_role_rel.sml" [("person_id",Int.toString person_id)])
