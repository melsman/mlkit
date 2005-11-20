(* $Id$ *)

val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (person_id,errs) = ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val name = ScsPersonData.name person_id

val title = ScsDict.sl [(ScsLang.en,`Edit roles for %0`),
			(ScsLang.da,`Ret roller for %0`)] [name]

val chosen_roles = Db.list (fn g => (g"role_id", g "abbreviation")) 
                              `select scs_roles.role_id, scs_roles.abbreviation
                                 from scs_roles, scs_role_rels
                                where scs_roles.role_id = scs_role_rels.role_id
                                  and scs_role_rels.party_id = '^(Int.toString person_id)'`

val not_chosen_roles = Db.list (fn g => (g "abbreviation", g"role_id"))
                                   `select scs_roles.role_id, scs_roles.abbreviation
                                      from scs_roles
                                     where scs_roles.role_id not in
                                             (select scs_role_rels.role_id
                                                from scs_role_rels
                                               where scs_role_rels.party_id = '^(Int.toString person_id)')`

val this_url = Html.genUrl "edit_person_role_rel.sml" [
		     ("person_id", Int.toString person_id)]

val footer = 
  case not_chosen_roles of
	  [] => ``
        | xs => `<tr>
                 <form method=post action="add_del_person_role_rel.sml">
                 <input type=hidden name="mode" value="add">
                 <input type=hidden name="target" value=^( this_url )>
                 <input type=hidden name="person_id" value="^(Int.toString person_id)">
           	 <td>` ^^ (ScsWidget.select not_chosen_roles "role_id") ^^ `</td>
	         <td><input type=submit name=submit value=^(ScsDict.s [(ScsLang.en,`Add`),
								       (ScsLang.da,`Tilf�j`)])></td>
                 </form>
                 </tr>`

val role_table =
  (ScsWidget.lineTable
     {hdcolor="silver",row_col1="silver",row_col2="lightgrey",
      header=`<th>Role</th><th>&nbsp</th>`,
      align="center",
      footer=footer} 
	(fn (role_id,abbrev) =>
	   `<td align=left>^abbrev</td>
	    <td><a href="^(Html.genUrl "add_del_person_role_rel.sml" 
                            [("person_id",Int.toString person_id),
                             ("role_id",role_id),
                             ("mode","del"),
			     ("target", this_url)])">^(ScsDict.s [(ScsLang.en,`del`),(ScsLang.da,`slet`)])</a></td>`)
        chosen_roles)

val _ = UcsPage.returnPg title
  (`<h1>^title</h1> 
   ` ^^ role_table)