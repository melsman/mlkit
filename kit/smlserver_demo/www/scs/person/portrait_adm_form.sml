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
	val (per_opt,errs) = ScsPerson.getPersonErr (person_id,ScsFormVar.emptyErr)
	val _ = ScsFormVar.anyErrors errs
	val per = ScsError.valOf per_opt

	val title = ScsDict.s UcsDict.my_upload_portrait_dict ^ " for " ^ (#name per)
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

        (* Toggle may_show_portrait_p *)
	val may_show_portrait_form = Quot.toString
	  (`<form action="/scs/person/upload_portrait.sml">
	   <input type=hidden name="mode" value="may_show_portrait"> 
           <input type=hidden name="target" value="adm"> 
           <input type=hidden name="person_id" value="^(Int.toString person_id)">
	    ^(ScsDict.s [(ScsLang.da,`Må billede vises offentligt?`),
			 (ScsLang.en,`May picture be shown to the public?`)])<p>
	    ` ^^ (ScsWidget.selectYesNoWithDefault (Db.fromBool (#may_show_portrait_p per)) "show_p") ^^ `
	    ^(UcsPage.mk_submit("submit",ScsDict.s UcsDict.save_dict,NONE))
	    </form>`)

	(* Make non official picture the official one *)
	val make_non_official_official_form = 
	  case (non_official_thumb,non_official_large) of
	    (SOME _,SOME _) => Quot.toString
	      (`<form action="/scs/person/upload_portrait.sml">
	       <input type=hidden name="mode" value="make_non_official_official"> 
	       <input type=hidden name="target" value="adm"> 
	       <input type=hidden name="person_id" value="^(Int.toString person_id)">
	       ^(UcsPage.mk_submit("submit",
				   ScsDict.s [(ScsLang.da,`Gør ikke officielt billede officielt`),
					      (ScsLang.en,`Make non official picture the official one`)],
				   NONE))
	       </form>`)
	  | _ => ""

	val pic_form = `
	  <table border=1>
	  <tr><th>^(ScsDict.s UcsDict.non_official_portrait_dict)</th>
	       <th>^(ScsDict.s UcsDict.official_portrait_dict)</th></tr>
	  <tr><td>^non_official_form</td><td>^official_form</td></tr>
	  <tr><td colspan=2> ^may_show_portrait_form </td></tr>
	  <tr><td colspan=2> ^make_non_official_official_form </td></tr>
	  </table>`

      in
	SOME (UcsWidget.innerBox title (`^(ScsDict.s ScsPerson.portrait_req_info) <p> ` ^^
					pic_form))
      end

val faneside = UcsSs.Widget.faneside mode [search_box,portrait_adm_box]

val (body,nb) = 
  (UcsSs.Widget.faneblad mode [faneside],[UcsPage.ucs_nb()])

val _ = UcsPvt.returnPg (UcsPvt.service_name user_id) (UcsPvt.leftList()) UcsPvt.rightList nb body