val user_id = ScsLogin.auth_roles ["SiteAdm"]

val d = ScsDict.d ScsLang.en "scs/admin/role" "edit_person_role_rel.sml"

val title = d"Edit Person and Role relations"

val _ = UcsPage.returnPg title
  (`<h1>^title</h1>

    You must first find a person and then edit role relations for that person.<p>

    ` ^^ (ScsPerson.search_form "/scs/admin/role/edit_person_role_rel.sml" []))
   

