(* $Id$ *)

val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (role_id, errs) = 
  UcsAdm.Widget.getRoleIdErr ("role_id", ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val (role_opt,errs) = UcsAdm.Data.getRoleFromDbErr (role_id,errs)
val _ = ScsFormVar.anyErrors errs
val role = ScsError.valOf role_opt
val role_abbr = #role_abbreviation role

val role_rels = UcsAdm.Data.getAllRoleRels role_id

val this_url = Html.genUrl "/scs/admin/role/edit_role.sml" [
  ("role_id", Int.toString role_id)
]

val body = 
  let
    val first : UcsAdm.role_rel_record  = (hd role_rels)
    fun fn_row bgcolor (role_rel:UcsAdm.role_rel_record) =
      let
        val name = UcsWidget.valOfString (#name_opt role_rel)
        val confirm_del_text = ScsDict.sl [
	  (ScsLang.da, `Skal %0 ikke længere have rollen ^role_abbr?`),
	  (ScsLang.en, `Delete %0 from the role ^role_abbr?`)] [name]
      in
        `
        <tr bgcolor="^bgcolor">
          <td>^name</td>
	  <td align="center">
            <a ^(UcsPage.confirmOnClick confirm_del_text) 
	         href="^(Html.genUrl "add_del_person_role_rel.sml" 
                            [("person_id",Int.toString (#party_id role_rel)),
                             ("role_id", Int.toString role_id),
                             ("mode","del"),
			     ("target", this_url)])"> ^(UcsPage.addSymbol false) </a>
   	  </td>
	</tr>`
      end
 
    val role_box = UcsPage.lineTable {
      hdcolor = "white",
      width = "100%",
      header = `
	 <tr bgcolor="#999999">
	   <td class="headercell">^(ScsDict.s UcsDict.name_dict)</td>
	   <td class="headercell">^(ScsDict.s UcsDict.del_dict)</td>
	 </tr>`,
      row_col1 = "#FFFFFF", 
      row_col2 = "#EEEEEE",
      align = "",
      footer = ``} fn_row role_rels
    val add_new_rel_form = ScsPerson.search_form "/scs/admin/role/add_del_person_role_rel.sml" 
                            [("role_id", Int.toString role_id),
                             ("mode","add"),
			     ("target", this_url)]
    val title = ScsDict.sl [
	  (ScsLang.da, `Indehavere af rollen %0`),
	  (ScsLang.en, `Owners of the role %0`)] [role_abbr]
  in
    UcsWidget.innerBox title (role_box ^^ add_new_rel_form)
  end
  handle _ => `fejl`

val title = ScsDict.s [
  (ScsLang.en,`Edit role memberships`),
  (ScsLang.da,`Ret rolletildelinger`)
]

val _ = UcsPage.returnPg title ( `<h1>^title</h1> ` ^^ body )

(*
fun mk_role_box role_id_opt =
  let
    val rel_id_opt = ScsFormVar.wrapOpt ScsFormVar.getNatErr "rel_id" 
    val (edit_rel_opt, errs) = case rel_id_opt of
        SOME rel_id => getRoleRelFromDbErr (rel_id,errs) 
      | NONE       => (NONE, errs)

    fun row_fn (role_rel:UcsAdm.role_rel_record) = `
      <td>^(UcsWidget.valOfString (#name_opt role_rel) )</td>
    `
    val all_rels = UcsAdm.Data.getAllRoleRels role_id
    val action = "save_role.sml"
    fun edit_row_fn (role_rel_opt:UcsAdm.role_rel_record option) = 
      let
	val (name) =
	  case role_rel_opt of 
	      SOME role_rel => (UcsWidget.valOfString (#name_opt role_rel))
	    | NONE	    => ("")
	val edit_hiddens = [
	  ("mode", UcsAdm.modeToString UcsAdm.EDIT_ROLE_REL)
	] @ (case role_rel_opt of 
	      SOME role_rel => [
	      ("rel_id", Int.toString rel_id),
	      ("req_id", Int.toString (#req_id req)),
	      ("round_id", Int.toString (#round_id req)) ]
	    | NONE	    => [] 
	)
	val new_hiddens = [
	  ("optag_id", Int.toString optag_id),
	  ("round_id", Int.toString (#round_id round)),
	  ("mode", UcsOb.modeToString UcsOb.NEW_REQ)
	]
	val td_list = (
	  if isSome grad_study_prog_rel_id_opt then 
	    (UcsOb.Widget.selectAvailableGradStudyProgs user_lang 
	      (#round_id round)  
	      "grad_study_prog_rel_id" 
	      (UcsOb.Widget.gradStudyProgName 
		 user_lang (valOf grad_study_prog_rel_id_opt),
	       valOf grad_study_prog_rel_id_opt ) )
	  else 
	    (UcsOb.Widget.selectGradStudyProgsNotInRound user_lang 
	      (#round_id round) 
	      "grad_study_prog_rel_id" grad_study_prog_rel_id_opt)
	) :: [
	  (ScsWidget.intextMaxLenVal 
	     4 4 (Int.toString max_students) "max_students"),
	  (ScsWidget.intextMaxLenVal 
	     4 4 (Int.toString objective_pct) "objective_pct"),
	  (ScsWidget.intextMaxLenVal 
	     4 4 (Int.toString subjective_pct) "subjective_pct")
	]
      in
	UcsWidget.mk_edit_row_fn NONE
	  (action, action)
	  (edit_hiddens, new_hiddens)
	  (td_list, td_list) req_opt
      end

	fun edit_link (req:UcsOb.grad_prog_req_record) = 
	  Html.genUrl "optag.sml" [
	      ("optag_id", Int.toString optag_id),
	      ("mode", UcsOb.modeToString mode),
	      ("req_id", Int.toString (#req_id req))
	  ] 
	fun del_link (req:UcsOb.grad_prog_req_record) = 
	  Html.genUrl action [
	      ("optag_id", Int.toString optag_id),
	      ("mode", UcsOb.modeToString UcsOb.DEL_REQ),
	      ("req_id", Int.toString (#req_id req))
	  ]

    fun req_cmp (req1:UcsOb.grad_prog_req_record, req2:UcsOb.grad_prog_req_record) = 
      (#req_id req1) = (#req_id req2)

    val grad_prog_reqs_body = UcsWidget.tableEntryBox user_lang
	  NONE [
	    UcsDict.graduate_programme_dict,
	    UcsDict.maximum_dict,
	    UcsDict.objective_pct_dict,
	    UcsDict.subjective_pct_dict
	  ] all_reqs edit_req_opt req_cmp
	  row_fn edit_row_fn edit_link del_link
	  UcsWidget.WRITE (SOME 11) NONE

  in
    SOME (UcsWidget.innerBox 
      (ScsDict.s 
	[(ScsLang.da, `Optag-runde:`),(ScsLang.en, `Application round:`)] ^
      " " ^ (#description round) )
      grad_prog_reqs_body
    )

  end (* of mk_role_box *)

*)




