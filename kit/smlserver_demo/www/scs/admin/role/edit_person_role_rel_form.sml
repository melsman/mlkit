val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val title = ScsDict.s [(ScsLang.en,`Edit Person and Role relations`),
		       (ScsLang.da,`Ret Person og Rolle Relationer`)]

val _ = UcsPage.returnPg title
  (`<h1>^title</h1>

    ^(ScsDict.s [(ScsLang.en,`You must first find a person and then edit role relations for that person.`),
		 (ScsLang.da,`Du må først finde en person og så redigere rolle relationer for denne person.`)])<p>

    ` ^^ (ScsPerson.search_form "/scs/admin/role/edit_person_role_rel.sml" []))
   

