val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val enum_id_opt = ScsFormVar.wrapOpt ScsEnum.getEnumerationErr "enum_id"

val val_id = ScsFormVar.wrapOpt ScsFormVar.getNatErr "val_id"

val title = ScsDict.s [(ScsLang.en,`Edit Enumerations`),
		       (ScsLang.da,`Ret Enumereringer`)]

val search_form =
  UcsPb.formBox NONE "edit_enum.sml" 
  [("submit",ScsDict.s [(ScsLang.en,`Edit Enumeration`),
			(ScsLang.da,`Ret Enumerering`)],NONE)]
  ``
  (ScsEnum.selectEnumeration "enum_id" NONE)

val edit_enum_form =
  case enum_id_opt of
    NONE => ``
  | SOME enum_id => 
      let
	fun enum_fn_row bgcolor (enum_val : ScsEnum.enum_value) =
	  let
	    val (text_da_widget,text_eng_widget) =
	      if val_id = SOME(#val_id enum_val) then
		(Quot.toString (ScsWidget.intextVal 40 (#text_da enum_val) "text_da"),
		 Quot.toString (ScsWidget.intextVal 40 (#text_eng enum_val) "text_eng"))
	      else
		(#text_da enum_val,#text_eng enum_val)
	    val edit_link = 
	      if val_id = SOME(#val_id enum_val) then
		`<input type="submit" name="submit" value="^(ScsDict.s ScsEnum.save_dict)">`
	      else
		`<a href="^(Html.genUrl "edit_enum.sml" [("val_id",Int.toString (#val_id enum_val)),
                                                         ("enum_id",Int.toString enum_id)])">^(ScsDict.s ScsEnum.edit_dict)</a>`
	    val (pre_form,post_form) = 
	      if val_id = SOME(#val_id enum_val) then 
		(`<form action="save_enum.sml" method=post>
		 ` ^^ (Html.export_hiddens [("enum_id",Int.toString enum_id),
					    ("text_id",Int.toString (#text_id enum_val))]),`</form>`)
	      else
		(``,``)
	  in
	    pre_form ^^
	    `<tr bgcolor="^bgcolor">
	     <td>^(#value enum_val)</td>
	    <td>^(text_da_widget)</td>
	    <td>^(text_eng_widget)</td>
	    <td>^(if #active_p enum_val then ScsDict.s UcsDict.yes_dict else ScsDict.s UcsDict.no_dict)</td>
	    <td>^(Int.toString (#ordering enum_val))</td>
	    <td>^(Quot.toString edit_link)</td>
	    </tr>` ^^ post_form
	  end
	val enum_table = 
	  UcsPage.lineTable {hdcolor = "white",
			     width = "100%",
			     header = `
			     <tr bgcolor="#999999">
			     <td class="headercell">^(ScsDict.s ScsEnum.value_dict)</td>
			     <td class="headercell">^(ScsDict.s ScsEnum.text_da_dict)</td>
			     <td class="headercell">^(ScsDict.s ScsEnum.text_eng_dict)</td>
			     <td class="headercell">^(ScsDict.s ScsEnum.active_p_dict)</td>
			     <td class="headercell">^(ScsDict.s ScsEnum.ordering_dict)</td>
			     <td class="headercell">&nbsp;</td>
			     </tr>`,
			     row_col1 = "#FFFFFF", 
			     row_col2 = "#EEEEEE",
			     align = "",
			     footer = ``} enum_fn_row 
		 (ScsEnum.getEnumerationsFromDb enum_id)
      in
	enum_table
      end

val _ = UcsPage.returnPg title
  (`<h1>^title</h1>

    ` ^^ search_form ^^ 
   `<hr>` ^^ 
   edit_enum_form)
