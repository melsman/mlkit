val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (person_id,errs) = ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val passwd = 
  ScsDb.oneFieldErrPg (`select password
                          from scs_users_active
                         where scs_users_active.user_id = '^(Int.toString person_id)'`,
			 ScsDict.s' [(ScsLang.en,`You can't become this user, because he is not active.`),
				     (ScsLang.da,`Du kan ikke blive denne bruger, idet brugeren ikke er aktiv.`)])

(* Set cookies with new user identity *)
val _ = ScsLogin.set_user_pw_cookie person_id passwd (ScsConfig.scs_site_index_page())