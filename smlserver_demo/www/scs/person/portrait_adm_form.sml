val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm,ScsRole.PortraitAdm]
val mode = UcsSs.Widget.PORTRAIT_ADM (* There is only one mode *)
(* The person for which we upload pictures. *)
val person_id_opt = ScsFormVar.wrapOpt ScsFormVar.getIntErr "person_id"

val search_form = ScsPerson.search_form "/scs/person/portrait_adm_form.sml" []
val search_title = ScsDict.s UcsDict.search_person_dict
val search_box = SOME (UcsWidget.innerBox search_title (search_form))

val portrait_adm_box =
  case person_id_opt of
    NONE => NONE
  | SOME person_id =>
      let
	val title = ScsDict.s UcsDict.my_upload_portrait_dict ^ " for " ^ (ScsPerson.name person_id)
	val portraits = ScsPerson.getPortraits person_id

	(* Non Official Pictures *)
	val non_official_thumb = ScsPerson.getPicture ScsPerson.thumb_fixed_height false portraits
	val non_official_large = ScsPerson.getPicture ScsPerson.original false portraits
	val non_official_form = ScsPerson.editPicForm person_id "adm" false 
	  non_official_thumb non_official_large

	(* Official Pictures *)
	val official_thumb = ScsPerson.getPicture ScsPerson.thumb_fixed_height true portraits
	val official_large = ScsPerson.getPicture ScsPerson.original true portraits
	val official_form = ScsPerson.editPicForm person_id "adm" true official_thumb official_large

	val pic_form = `
	  <table border=1>
	  <tr><th>^(ScsDict.s UcsDict.non_official_portrait_dict)</th>
	       <th>^(ScsDict.s UcsDict.official_portrait_dict)</th></tr>
	  <tr><td>^non_official_form</td><td>^official_form</td></tr>
	  </table>`
      in
	SOME (UcsWidget.innerBox title (`^(ScsDict.s ScsPerson.portrait_req_info) <p> ` ^^
					pic_form))
      end

val faneside = UcsSs.Widget.faneside mode [search_box,portrait_adm_box]

val (body,nb) = 
  (UcsSs.Widget.faneblad mode [faneside],[UcsPage.ucs_nb()])

val _ = UcsPvt.returnPg (UcsPvt.service_name user_id) (UcsPvt.leftList()) UcsPvt.rightList nb body