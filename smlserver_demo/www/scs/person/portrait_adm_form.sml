val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm,ScsRole.PortraitAdm]
val (mode,errs) = ScsPerson.getPortraitModeErr("mode",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs
(* The person for which we upload pictures. *)
val person_id_opt = ScsFormVar.wrapOpt ScsFormVar.getIntErr "person_id"

val search_form = ScsPerson.search_form "/scs/person/portrait_adm_form.sml" []
val search_title = ScsDict.s UcsDict.search_person_dict
val search_box = SOME (UcsWidget.innerBox search_title (search_form))

val (portrait_adm_boxes,name_opt) =
  case mode of
    ScsPerson.PORTRAIT_ADM_HELP => ([],NONE)
  | ScsPerson.PORTRAIT_ADM => 
      case person_id_opt of
	NONE => ([],NONE)
      | SOME person_id =>
	  let
	    val (per_opt,errs) = ScsPerson.getPersonErr (person_id,ScsFormVar.emptyErr)
	    val _ = ScsFormVar.anyErrors errs
	    val per = ScsError.valOf per_opt
	    (* Info Box *)
	    val title = ScsDict.s UcsDict.my_upload_portrait_dict ^ " for " ^ (#name per)
	    val info_box = 
	      SOME (UcsWidget.innerBox title (`^(ScsDict.s ScsPerson.portrait_req_info)`))

	    val portraits = ScsPerson.getPortraits person_id

	    (* Non Official Pictures *)
	    val non_official_thumb = ScsPerson.getPicture ScsPerson.thumb_fixed_height false portraits
	    val non_official_large = ScsPerson.getPicture ScsPerson.original false portraits
	    val non_official_form = ScsPerson.editPicForm person_id "adm" false 
	      non_official_thumb non_official_large
	    val non_official_form_box =
	      SOME (UcsWidget.innerBox (ScsDict.s UcsDict.non_official_portrait_dict) `^(non_official_form)`)

	    (* Official Pictures *)
	    val official_thumb = ScsPerson.getPicture ScsPerson.thumb_fixed_height true portraits
	    val official_large = ScsPerson.getPicture ScsPerson.original true portraits
	    val official_form = ScsPerson.editPicForm person_id "adm" true official_thumb official_large
	    val official_form_box =
	      SOME (UcsWidget.innerBox (ScsDict.s UcsDict.official_portrait_dict) `^(official_form)`)
	      
	    (* Toggle may_show_portrait_p *)
	    val may_show_portrait_info = 
	      case ScsApproval.log (ScsPerson.scs_approvals_show_portrait_name,person_id) of
		NONE => ""
	      | SOME q => 
		  let
		    val header = ScsDict.s [(ScsLang.da,`<h2>Registreringer omkring synlighed af billeder.</h2>`),
					    (ScsLang.en,`<h2>Registrations about the visibility of 
					     pictures.</h2>`)]
		  in
		    UcsPage.info(Quot.toString(`^(header) ` ^^ q))
		  end
            val may_show_portrait_box = 
	      SOME (UcsWidget.innerBox (ScsDict.s [(ScsLang.da,`Synlighed`),
						   (ScsLang.en,`Publicity`)])
		    (`<form action="/scs/person/upload_portrait.sml">
		     <input type=hidden name="mode" value="may_show_portrait"> 
		     <input type=hidden name="target" value="adm"> 
		     <input type=hidden name="person_id" value="^(Int.toString person_id)">
		     ^(ScsDict.s [(ScsLang.da,`Må billede vises offentligt?. Ja betyder, at billede både må 
				   vises på Intranettet og på Internettet. Nej betyder at det kun er personer med
				   administratorrettigheder, som kan se billedet.`),
				  (ScsLang.en,`May picture be shown to the public? Yes means that the picture 
				   may be
				   shown on both the Intranet and the Internet. A No means that only persons with
				   administrator priviledges may see the picture.`)])<p>
		     ` ^^ (ScsWidget.selectYesNoWithDefault (Db.fromBool (#may_show_portrait_p per)) "show_p") ^^ `
		     ^(UcsPage.mk_submit("submit",ScsDict.s UcsDict.save_dict,NONE))^(may_show_portrait_info)
		     </form>`))

              (* Make non official picture the official one *)
            val confirm_but =
	      SOME (UcsPage.confirmOnClick (ScsDict.s [(ScsLang.da,`Bekræft, at du ønsker at gøre det ikke 
							officielle billede
							officielt. Det nuværende officielle billede vil blive 
							slettet.`),
						       (ScsLang.en,`Please confirm, that you want to make the non 
							official picture the official one. The current official 
							picture
							will be deleted.`)]))
	    val make_non_official_official_box = 
	      case (non_official_thumb,non_official_large) of
		(SOME _,SOME _) => 
		  SOME (UcsWidget.innerBox (ScsDict.s [(ScsLang.da,`Gør ikke officielt billede officielt`),
						       (ScsLang.en,`Make non official picture the official one`)])
			`^(ScsDict.s [(ScsLang.da,`Du kan gøre det ikke officielle billede officielt. Du skal 
				       sikre dig,
				       at det faktisk er rigtige person før du gør dette. Det officielle billede
				       er det der findes på personens adgangskort.`),
				      (ScsLang.en,`Yo can make the non official picture the official one. To do 
				       this
				       you musk make sure that it is actually the same person on the picture and 
				       the one
				       you are editing. The official picture is the picture you see on the 
				       entrance card.`)])<p>
			<form action="/scs/person/upload_portrait.sml">
			<input type=hidden name="mode" value="make_non_official_official"> 
			<input type=hidden name="target" value="adm"> 
			<input type=hidden name="person_id" value="^(Int.toString person_id)">
			^(UcsPage.mk_submit("submit",
					    ScsDict.s [(ScsLang.da,`Gør ikke officielt billede officielt`),
						       (ScsLang.en,`Make non official picture the official one`)],
					    confirm_but))
		  </form>`)
	      | _ => NONE
	  in
	    ([info_box,may_show_portrait_box,make_non_official_official_box,official_form_box,
	      non_official_form_box],
	     SOME (#name per))
          end

val faneside = ScsPerson.portraitFaneside mode (search_box::portrait_adm_boxes)

val (body,nb) = 
  (ScsPerson.portraitFaneblad mode [faneside],[UcsPage.ucs_nb(),
					       ScsPerson.portrait_adm_nb name_opt])

val _ = UcsPvt.returnPgCache false (UcsPvt.service_name user_id) (UcsPvt.leftList()) UcsPvt.rightList nb body