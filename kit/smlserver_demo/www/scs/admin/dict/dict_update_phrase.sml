val % = ScsDict.d ScsLang.en "scs" "admin/dict/dict_update_phrase.sml"
val %% = ScsDict.d' ScsLang.en "scs" "admin/dict/dict_update_phrase.sml"

val phrase_id          = 
  ScsFormVar.wrapFail (ScsFormVar.wrapIntAsString ScsFormVar.getIntErr) ("phrase_id", %"phrase identifier")
val (target_lang,errs) = ScsFormVar.getLangErr ("target_lang", %"target language", ScsFormVar.emptyErr)
val (source_lang,errs) = ScsFormVar.getLangErr ("source_lang", %"source language", errs)
val (search_lang,errs) = ScsFormVar.getEnumErr ["source","target","script"] ("search_lang", %"search language", errs)
val (regexp_str,errs)  = ScsFormVar.wrapMaybe ScsFormVar.getStringErr ("regexp", %"regular expression", errs)
val (limit_rows,errs)  = ScsFormVar.wrapIntAsString ScsFormVar.getNatErr ("limit_rows",%"limit rows", errs)
val (submit,errs)      = ScsFormVar.getEnumErr [%"Insert",%"Update",%"Delete"] 
  ("submit",%"press insert, delete or update button",errs)
val _ = ScsFormVar.anyErrors errs

val _ = 
  if submit = (%"Delete") then
    ScsDb.panicDml `delete scs_dict_sources where phrase_id = '^(phrase_id)'`
  else
    let
      val (target_phrase,errs) = ScsFormVar.getStringErr ("target_phrase", %"translated phrase",ScsFormVar.emptyErr)
      val _ = ScsFormVar.anyErrors errs
    in
      if submit = (%"Insert") then
	ScsDb.errorDml (%% `Phrase already exists`) 
	`insert into scs_dict_targets(target_id,phrase_id,lang,phrase,modifying_user)
	 values (scs.new_obj_id,
		 ^(Db.valueList [phrase_id,ScsLang.toString target_lang,
				 target_phrase,Int.toString (ScsLogin.user_id())]))`
      else (* Update *)
	let
	  val target_id = 
	    ScsFormVar.wrapFail (ScsFormVar.wrapIntAsString ScsFormVar.getIntErr) ("target_id",%"target id missing")
	in
	  ScsDb.errorDml (%% `Phrase can't be updated`) 
	  `update scs_dict_targets 
              set ^(Db.setList [("phrase",target_phrase),
				("modifying_user",Int.toString (ScsLogin.user_id()))]),last_modified = sysdate
	   where target_id = ^(Db.qqq target_id)`
	end
    end

val _ = Ns.returnRedirect (Html.genUrl "dict_form.sml" [("target_lang",ScsLang.toString target_lang),
							("source_lang",ScsLang.toString source_lang),
							("search_lang",search_lang),
							("regexp",regexp_str),("limit_rows",limit_rows)])


