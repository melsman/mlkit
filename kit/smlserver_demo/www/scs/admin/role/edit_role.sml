(* $Id$ *)

val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (role_id_opt, errs) = 
  UcsAdm.Widget.getRoleIdOpt ("role_id", ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val role_rels = case role_id_opt of 
    SOME role_id => UcsAdm.Data.getAllRoleRels role_id
  | NONE         => [] 




val body = 
  let
    val first : UcsAdm.role_rel_record  = (hd role_rels)
  in
    `^(ScsString.valOf (#name_opt first) )`
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

    fun row_fn (role_rel:role_rel_record) = `
      <td>^(#full_name role_rel) )</td>
      <td>^( Int.toString (#max_students req) )</td>
      <td>^( Int.toString (#objective_pct req) )</td>
      <td>^( Int.toString (#subjective_pct req) )</td>
    `
	val all_reqs = UcsOb.Data.getAllGradProgReqs (#round_id round)
	val action = "save_optag.sml"
	fun edit_row_fn (req_opt:UcsOb.grad_prog_req_record option) = 
	  let
	    val (grad_study_prog_rel_id_opt, max_students, 
		 objective_pct, subjective_pct) = 
	      case req_opt of 
	          SOME req => (SOME (#grad_study_prog_rel_id req), #max_students req, 
		 	       #objective_pct req, #subjective_pct req)
	        | NONE	    => (NONE, 0, 100, 0 )
	    val edit_hiddens = [
	      ("mode", UcsOb.modeToString UcsOb.EDIT_REQ)
	    ] @ (case req_opt of 
	          SOME req => [
		  ("optag_id", Int.toString optag_id),
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




