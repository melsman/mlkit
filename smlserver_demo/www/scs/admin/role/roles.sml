(* $Id$ *)

val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val role_form = UcsWidget.layoutComponentGrp ( UcsWidget.FORMBOX {
	action     = "edit_role.sml",
	form_attr  = [],
	buts       = [("submit",ScsDict.s UcsDict.choose_dict,NONE)],
	header     = [],
	table_attr = [],
	body       = [UcsAdm.Widget.allRolesWidget false UcsWidget.WRITE "role_id" NONE]
} )


val title = ScsDict.s [
  (ScsLang.en,`Edit role memberships`),
  (ScsLang.da,`Ret rolletildelinger`)
]

val _ = UcsPage.returnPg title ( `<h1>^title</h1> ` ^^ role_form )

