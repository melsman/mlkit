val % = ScsDict.d ScsLang.en "scs_lib" "passwd_upd.sml"
val %% = ScsDict.d' ScsLang.en "scs_lib" "passwd_upd.sml"

val (cur_passwd,errs) = ScsFormVar.getStringErr ("cur_passwd",%"Current password",ScsFormVar.emptyErr)
val (new_passwd,errs) = ScsFormVar.getStringErr ("new_passwd",%"New password",errs)
val (confirm_new_passwd,errs) = ScsFormVar.getStringErr ("confirm_new_passwd",%"Confirmed new password",errs)
val (new_passwd,confirm_new_passwd) = (ScsString.trim new_passwd,ScsString.trim confirm_new_passwd)
val errs = 
  if new_passwd <> confirm_new_passwd then
    ScsFormVar.addErr (%%`Your new password and confirmed password are different`,errs)
  else
    errs
val errs = 
  if new_passwd = "" then
    ScsFormVar.addErr (%%`Your did not type a new password.`,errs)
  else
    errs
val errs =
  if Db.existsOneRow 
    `select user_id
       from scs_users_active 
      where scs_users_active.user_id = '^(Int.toString ScsLogin.user_id)'
        and scs_users_active.password = ^(Db.qqq cur_passwd)` then
    errs
  else
    ScsFormVar.addErr (%%`The old password you typed does not match the one found in the database`,errs)
val _ = ScsFormVar.anyErrors errs

(* We are ready to update the password *)
val _ = ScsDb.panicDml `update scs_users
                           set ^(Db.setList [("password",new_passwd)])
                         where scs_users.user_id = ^(Int.toString ScsLogin.user_id)`

(* Set cookies with new password *)
val _ = ScsLogin.set_user_pw_cookie ScsLogin.user_id new_passwd (ScsConfig.scs_site_index_page())

(*val _ = Ns.write
`HTTP/1.0 302 Found
Location: ^(ScsConfig.scs_site_index_page())
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_user_id", value=Int.toString ScsLogin.user_id,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_password", value=new_passwd,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})


You should not be seeing this!`*)
