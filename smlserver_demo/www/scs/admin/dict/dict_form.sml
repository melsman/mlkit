val % = ScsDict.d ScsLang.en "scs/admin/dict/" "dict_form.sml"
val %% = ScsDict.d' ScsLang.en "scs/admin/dict/" "dict_form.sml"
val dl' = ScsDict.dl' ScsLang.en "scs/admin/dict/" "dict_form.sml"

val (target_lang,errs) = ScsFormVar.getLangErr ("target_lang", %"target language", ScsFormVar.emptyErr)
val (source_lang,errs) = ScsFormVar.getLangErr ("source_lang", %"source language", errs)
val (search_lang,errs) = ScsFormVar.getEnumErr ["source","target","script"] ("search_lang", %"search language", errs)
val (regexp_str,_)     = ScsFormVar.wrapMaybe ScsFormVar.getStringErr ("regexp", %"regular expression", errs)
val (regexp,errs)      = ScsFormVar.getRegExpErr ("regexp", %"regular expression", errs)
val (limit_rows,errs)  = ScsFormVar.wrapIntAsString ScsFormVar.getNatErr ("limit_rows",%"limit rows", errs)
val (submit,errs)      = 
  ScsFormVar.wrapMaybe (ScsFormVar.getEnumErr [%"Show Dictionary",%"Empty Cache",%"Export Dictionary"]) 
  ("submit",%"submit button",errs)

val limit_rows = if limit_rows = "" then "50" else limit_rows
val regexp_str = if regexp_str = "" then "$" else regexp_str

val _ = 
  if submit = (%"Empty Cache") then 
    (ScsDict.cacheFlush (source_lang,target_lang);
     Ns.returnRedirect (Html.genUrl "dict_form.sml" [("target_lang",ScsLang.toString target_lang),
						     ("source_lang",ScsLang.toString source_lang),
						     ("search_lang",search_lang),
						     ("regexp",regexp_str),("limit_rows",limit_rows)]);
     Ns.exit())
  else
    ()

val filter_p = 
  fn (phrase_id:string,source_phrase:string,module:string,file_name:string,
      create_date:string,last_read_date:string,target_phrase:string,target_id:string,
      last_modified:string,modifying_user:string) => 
  case search_lang of
    "source" => RegExp.match regexp source_phrase
  | "target" => RegExp.match regexp target_phrase
  | "script" => RegExp.match regexp (module ^ file_name)
  | _ => true

val phrases = 
  let
    val l = List.filter filter_p
    (Db.list (fn g => (g "phrase_id", g "source_phrase",g "module",g "file_name",
		       g "create_date",g "last_read_date",g "target_phrase",
		       g "target_id",g "last_modified", g "modifying_user"))
	     `select scs_dict_sources.phrase_id,
                     scs_dict_sources.phrase as source_phrase,
		     scs_dict_sources.module,
		     scs_dict_sources.file_name, 
		     scs_dict_sources.create_date,
		     scs_dict_sources.last_read_date,
		     t.phrase as target_phrase, 
		     t.target_id,
		     t.last_modified,
		     t.modifying_user
	        from scs_dict_sources, (select * from scs_dict_targets where lang = '^(ScsLang.toString target_lang)') t
               where scs_dict_sources.phrase_id = t.phrase_id(+)
                 and scs_dict_sources.lang = '^(ScsLang.toString source_lang)'
	       order by create_date desc`)
  in
    List.take (l,Option.valOf (Int.fromString limit_rows)) handle _ => l
  end


val entry_forms =
  if submit = %"Export Dictionary" then
    Db.fold (fn (g, acc) => acc ^^ 
	     (if g "target_id" = "" then
	       `insert into scs_dict_source (phrase_id,lang,phrase,script,entry_date) values (^(Db.seqNextvalExp "scs_dict_phrase_seq"),^(Db.valueList [g "lang", g "phrase", g "script", g "entry_date"]));
`
	     else
	       `insert into scs_dict_target (phrase_id,target_id,lang,phrase) values (^(Db.seqCurrvalExp "scs_dict_phrase_seq"),^(Db.seqNextvalExp "scs_dict_target_seq"),^(Db.valueList [g "lang", g "phrase"]));
`)) `<pre>`
	       `select * from
	       (select phrase_id, to_number(null) as target_id, lang, phrase, script, entry_date from scs_dict_source
		union
		select phrase_id, target_id, lang, phrase, '' as script, to_date(null) as entry_date from scs_dict_target)
	       order by phrase_id,target_id desc` ^^ `</pre>`
  else
    if ScsFormVar.isErrors errs orelse source_lang = target_lang then
      %% `No phrases selected<p> The source language and target language must not be the same.`
    else
      %% `You have selected the following phrases:<p>` ^^ 
      (ScsWidget.lineTable
       {hdcolor="silver",row_col1="silver",row_col2="lightgrey",
	header=`<th>
	^(%"Translation from") ^(%(ScsLang.toString source_lang)) ^(%"to") ^(%(ScsLang.toString target_lang))</th><th>&nbsp;</th>`,
	align="center",footer=``} 
       (fn (phrase_id:string,source_phrase:string,module:string,file_name:string,
	    create_date:string,last_read_date:string,target_phrase:string,target_id:string,
	    last_modified:string,modifying_user:string) => 
	`<form method=post action=dict_update_phrase.sml>
	<input type=hidden value="^phrase_id" name=phrase_id> ` ^^ 
	(Html.export_hiddens [("target_lang",ScsLang.toString target_lang),
			      ("source_lang",ScsLang.toString source_lang),
			      ("search_lang",search_lang),
			      ("regexp",regexp_str),("limit_rows",limit_rows)]) ^^
	(if target_phrase<>"" then `<input type=hidden value="^target_id" name=target_id>` else ``) ^^ `
	   <td><code><b>^module(^file_name)</b>, ^(ScsDate.pp (Option.valOf(Db.toDate create_date)))<br>` ^^ 
	   (Html.htmlencode `^source_phrase`) ^^ `</code><br>
	   <textarea name=target_phrase cols=70 rows=^(Int.toString(Int.div(String.size source_phrase,50) + 1)) 
	   wrap=vertual>^target_phrase</textarea></td>
	   <td>
	   <input type=submit name=submit value="^(%(if target_phrase="" then "Insert" else "Update"))">
	   <input type=submit name=submit value="^(%"Delete")"></td></form>`) phrases)

val _ =
 ScsPage.returnPg "Dictionary" 
 (%% `Use this form to maintain the
  dictionary translating source phrases used in the scripts into other
  languages.<p>` ^^ 

  (ScsWidget.formBox "dict_form.sml" 
   [("submit",%"Show Dictionary"),("submit",%"Empty Cache"),("submit",%"Export Dictionary")] 
   (`Source language` ^^ (ScsWidget.selectLang source_lang "source_lang") ^^ `
     <br> Target language` ^^ (ScsWidget.selectLang target_lang "target_lang") ^^ 
   (dl' [(Quot.toString (ScsWidget.selectWithDefault [(%"source phrases","source"),
						      (%"target phrases","target"),
						      (%"script filenames","script")]
			 search_lang "search_lang")),
	 (Quot.toString (ScsWidget.intextVal 20 regexp_str "regexp")),
	 (Quot.toString (ScsWidget.intextVal 5 limit_rows "limit_rows"))]
    `<br>Search on %0  <br> using the regular expression %1 <br>
       (leave <b>$</b> to get all empty phrases and type <b>.*</b> to get all phrases).<p>
       Limit rows: %2` ) )) ^^ `<p><hr>` ^^ entry_forms)



