val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val person_id = ScsFormVar.wrapOpt ScsFormVar.getIntErr "person_id"

val title = 
  case person_id of 
    NONE => ScsDict.s [(ScsLang.en,`User administration`),(ScsLang.da,`Brugeradministration`)]
  | SOME pid => 
      ScsDict.sl [(ScsLang.en,`User administration for %0`),(ScsLang.da,`Brugeradministration for %0`)] 
        [ScsPerson.name pid]

val body = 
  case person_id of
    NONE => ``
  | SOME pid => 
      let
	val roleadm =  ScsDict.sl' [(ScsLang.en,`Edit <a href="%0">role relations</a> for this user.<p>`),
				    (ScsLang.da,`Ret <a href="%0">rolle relationer</a> for denne bruger.<p>`)]
	  [Html.genUrl "/scs/admin/role/edit_person_role_rel.sml" [("person_id",Int.toString pid)]]
        val passwd = ScsWidget.formBox "/scs/admin/user/upd_passwd.sml"
	  [("submit",ScsDict.s [(ScsLang.en,`New Password`),(ScsLang.da,`Nyt kodeord`)]),
	   ("submit",ScsDict.s [(ScsLang.en,`Nullify Password`),(ScsLang.da,`Slet kodeord`)])]
	  (Html.export_hiddens [("person_id",Int.toString pid)] ^^ `
	   ^(ScsDict.s [(ScsLang.en,`Password administration`),(ScsLang.da,`Administration af kodeord`)])`)
	val become = ScsWidget.formBox "/scs/admin/user/become_this_user.sml"
 	  [("submit",ScsDict.sl [(ScsLang.en,`Become this user (%0)`),(ScsLang.da,`Bliv denne bruger (%0)`)]
	    [ScsPerson.name pid])]
	  (Html.export_hiddens [("person_id",Int.toString pid)] ^^ `
	   ^(ScsDict.s [(ScsLang.en,`Become another user`),(ScsLang.da,`Bliv en anden bruger`)])`)
      in
	roleadm ^^ `<hr>` ^^ passwd ^^ `<hr>` ^^ become
      end

val _ = UcsPage.returnPg title
  (`<h1>^title</h1>

    ` ^^ (ScsPerson.search_form "/scs/admin/user/index.sml" []) ^^ `<hr>` ^^
   body)

