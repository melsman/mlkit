val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val person_id_opt = ScsFormVar.wrapOpt ScsFormVar.getIntErr "person_id"

val title = 
  case person_id_opt of 
    NONE => ScsDict.s [(ScsLang.en,`User administration`),(ScsLang.da,`Brugeradministration`)]
  | SOME pid => 
      ScsDict.sl [(ScsLang.en,`User administration for %0 (%1)`),(ScsLang.da,`Brugeradministration for %0 (%1)`)] 
        [ScsPersonData.name pid, ScsPersonData.email pid]

val body = 
  case person_id_opt of
    NONE => ``
  | SOME pid => 
      let
	val passwd_exists = ScsError.wrapPanic Db.existsOneRow `
	  select password
	    from scs_users_active
	   where user_id = '^(Int.toString pid)'
	     and password is not null`

        val email_sql = `
	  select distinct email
	    from (
	      select per.person_id, appl.email email, 'ucs_ob' kilde
  	        from scs_persons per,
		     ucs_ob_applications appl
	       where per.person_id = appl.person_id

	      union 

	      select pr.person_id, pr_per.email email, 'ucs_pr' kilde
	        from scs_person_rels pr, ucs_pr_persons pr_per
	       where pr.on_what_table = 'person'
	         and pr.on_which_id = pr_per.person_id

	      union

	      select pr.person_id, 
		     sysadm.login || '@itu.dk' email, 'sysadm' kilde
	        from scs_person_rels pr, sysadm_login sysadm
	       where pr.on_what_table = 'sysadm_login'
	         and pr.on_which_id = sysadm.id
	      ) 
	      where person_id = ^(Int.toString pid)`
	fun f g = (g "email", g "email")
	val opts = Db.list f email_sql
        val select_email = 
	  ScsWidget.selectWithDefault opts (ScsPersonData.email pid) "email"

        val email_form = UcsWidget.layoutComponentGrp (
	  UcsWidget.FORMBOX{
	    action="save_email.sml",
	    form_attr = [],
	    buts = [("submit", ScsDict.s UcsDict.ok_dict,NONE)],
	    header = (Html.export_hiddens [
	      ("person_id", Int.toString pid)
	    ] ),
	    body = [
	      UcsWidget.twoColumns(
	        UcsDict.email_dict, UcsDict.email_dict, false, [select_email]
	      ) 
	    ],
	    table_attr = []} 
	  )

	val roleadm =  ScsDict.sl' [
	 (ScsLang.en,`Edit <a href="%0">role relations</a> for this user.<p>`),
	 (ScsLang.da,`Ret <a href="%0">rolle relationer</a> for denne bruger.<p>`)]
	 [Html.genUrl "/scs/admin/role/edit_person_role_rel.sml" [("person_id",Int.toString pid)]]

        val passwd = ScsWidget.formBox "/scs/admin/user/upd_passwd.sml"
	  [("submit",ScsDict.s [(ScsLang.en,`New Password`),(ScsLang.da,`Nyt kodeord`)]),
	   ("submit",ScsDict.s [(ScsLang.en,`Nullify Password`),(ScsLang.da,`Slet kodeord`)])]
	  (Html.export_hiddens [("person_id",Int.toString pid)] ^^ `
	   ^(ScsDict.s [(ScsLang.en,`Password administration.`),(ScsLang.da,`Administration af kodeord.`)])
	   <b> ^(if passwd_exists then ScsDict.s [(ScsLang.da,`Bruger er aktiv`),(ScsLang.en,`User is active`)]
	       else ScsDict.s [(ScsLang.da,`Bruger er ikke aktiv`),(ScsLang.en,`User is not active`)]) </b>`)

	val become = ScsWidget.formBox "/scs/admin/user/become_this_user.sml"
 	  [("submit",ScsDict.sl [(ScsLang.en,`Become this user (%0)`),(ScsLang.da,`Bliv denne bruger (%0)`)]
	    [ScsPersonData.name pid])]
	  (Html.export_hiddens [("person_id",Int.toString pid)] ^^ `
	   ^(ScsDict.s [(ScsLang.en,`Become another user`),(ScsLang.da,`Bliv en anden bruger`)])`)
      in
	roleadm ^^ `<hr>` ^^ become ^^ `<hr>` ^^ email_form ^^ `<hr>` ^^ passwd
      end

val new_user_form = 
  let
    val new_person = {
      person_id         = 0,
      first_names 	= "",
      last_name		= "",
      name 		= "",
      norm_name         = "",
      email		= "",
      url		= "",
      cpr		= "",
      upload_folder_id  = NONE,
      upload_folder_name = NONE,
      upload_folder_path = NONE,
      may_show_portrait_p = false
    }
  in
    UcsWidget.layoutComponentGrp (
	  UcsWidget.FORMBOX{
	    action="new_user.sml",
	    form_attr = [],
	    buts = [("submit", ScsDict.s UcsDict.create_dict,NONE)],
	    header = (Html.export_hiddens [
	    ] ),
	    body = UcsPerson.userComponent (new_person,true),
	    table_attr = []} 
	  )
  end


val _ = UcsPage.returnPg title
  (`<h1>^title</h1>

    ` ^^ (ScsPerson.search_form "/scs/admin/user/index.sml" []) ^^ `<hr>` ^^
   (case person_id_opt of 
        SOME id => `` 
      | NONE => `<h2>Opret ny bruger</h2>` ^^ new_user_form ) ^^
   body)

