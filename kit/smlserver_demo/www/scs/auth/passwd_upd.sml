val user_id = ScsLogin.auth()

val (cur_passwd,errs) = ScsFormVar.getStringErr ("cur_passwd",
						 ScsDict.s [(ScsLang.da,`Nuværende password`),
							    (ScsLang.en,`Current password`)],ScsFormVar.emptyErr)
val (new_passwd,errs) = ScsFormVar.getStringErr ("new_passwd",
						 ScsDict.s [(ScsLang.da,`Nyt kodeord`),
							    (ScsLang.en,`New password`)],errs)
val (confirm_new_passwd,errs) = ScsFormVar.getStringErr ("confirm_new_passwd",
							 ScsDict.s [(ScsLang.da,`Brkræftet password`),
								    (ScsLang.en,`Confirmed new password`)],errs)
val (new_passwd,confirm_new_passwd) = (ScsString.trim new_passwd,ScsString.trim confirm_new_passwd)
val errs = 
  if new_passwd <> confirm_new_passwd then
    ScsFormVar.addErr (ScsDict.s' [(ScsLang.da,`Nyt password og bekræftet password er ikke ens.`),
				   (ScsLang.en,`Your new password and confirmed password are different`)],errs)
  else
    errs
val errs = 
  if new_passwd = "" then
    ScsFormVar.addErr (ScsDict.s' [(ScsLang.da,`Du indtastede ikke et nyt password`),
				   (ScsLang.en,`Your did not type a new password.`)],errs)
  else
    errs
val (lang,errs) =
  case Db.zeroOrOneField
    `select scs_user.language_pref(user_id) as lang
       from scs_users_active 
      where scs_users_active.user_id = '^(Int.toString user_id)'
        and scs_users_active.password = ^(Db.qqq cur_passwd)` of
    SOME lang => (lang,errs)
  | NONE => 
    ("",ScsFormVar.addErr (ScsDict.s' [(ScsLang.da,`Det gamle password som du indtastedematcher ikke det der findes
					i databasen`),
				       (ScsLang.en,`The old password you typed does not match the one found 
                                        in the database`)],errs))
val _ = ScsFormVar.anyErrors errs

(* We are ready to update the password *)
val _ = ScsDb.panicDml `update scs_users
                           set ^(Db.setList [("password",new_passwd)])
                         where scs_users.user_id = ^(Int.toString user_id)`
val _ = Ns.Cache.insert (ScsLogin.getUserInfoCache,Int.toString user_id,SOME(ScsLogin.calcMd5 new_passwd,lang))

(* Set cookies with new password *)
val _ = ScsLogin.set_user_pw_cookie user_id new_passwd (ScsConfig.scs_site_index_page())

