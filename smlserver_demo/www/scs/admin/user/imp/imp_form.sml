val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val title = ScsDict.s [(ScsLang.en,`Central Personnel Register`),
		       (ScsLang.da,`Centralt Person Register`)]

(* Get data to show on the page. *)
fun imp_users_source (source:ScsUserImp.external_source) = 
  (source,
   ScsError.wrapPanic
   (Db.list (fn g => (g "user_imp_id", g "first_names", g "last_name",
		      g "security_id", g "email", Db.toDate (g "last_auto_import_try"),
		      Db.toDate (g "last_modified"), g "exact_match_id")))
       `select scs_user_imports.*,
               scs_user.imp_exact_match(user_imp_id) as exact_match_id
          from scs_user_imports
         where scs_user_imports.on_what_table = ^(Db.qqq (#db_name source))
         order by exact_match_id,last_modified`)

val imp_users = List.map imp_users_source (List.map ScsUserImp.getSource ScsUserImp.all_sources)

fun layout_user (user_imp_id,first_names,last_name,
		 security_id,email,last_auto_import_try,
		 last_modified,exact_match_id) =
  `<td><b>^first_names ^last_name</b></td>
   <td>^security_id</td>
   <td>^email</td>
   <td>^(ScsDate.wrapOpt ScsDate.pp last_auto_import_try)</td>
   <td align="center">^(ScsUserImp.exactMatchLink user_imp_id exact_match_id)</td>
   <td align="center">^(ScsUserImp.delLink user_imp_id)</td>
   <td align="center">^(ScsUserImp.moreInfoLink user_imp_id)</td>`

fun gen_table (source:ScsUserImp.external_source) [] = 
  ScsDict.s' [(ScsLang.en,`There are no persons to import in this group.`),
	      (ScsLang.da,`Der er ikke noget at importere i denne  gruppe.`)]
  | gen_table source persons =
  let
    val del_all_link = 
      ScsDict.sl [(ScsLang.en,`You may <a ^(UcsPage.confirmDelOnClick()) href="%0">delete</a> all rows from this external source.`),
		  (ScsLang.da,`Du kan <a ^(UcsPage.confirmDelOnClick()) href="%0">slette</a> alle rækker denne ekterne kilde.`)]
      [Html.genUrl "del_all_section.sml" [("source",#db_name source)]]
  in
    `^del_all_link
    ` ^^ 
    (UcsPage.lineTable
     {hdcolor="silver",row_col1="silver",row_col2="lightgrey",width="",
      header=`<th>^(ScsUserImp.nameField())</th>
              <th>^(ScsUserImp.securityIdField())</th>
              <th>^(ScsUserImp.emailField())</th>
              <th>^(ScsUserImp.lastImportField())</th>
              <th>^(ScsUserImp.exactMatchField())</th>
              <th>^(ScsUserImp.deleteField())</th>
              <th>^(ScsUserImp.moreInfoField())</th>`,
      align="center",
      footer=``} 
     layout_user
     persons)
  end

fun gen_page_intro imp_users =
  let
    fun gen_entry ((source:ScsUserImp.external_source,persons),acc) = 
      acc ^^ 
      (ScsDict.sl' [(ScsLang.en,`<li>%0 with %1 entries.`),
		    (ScsLang.da,`<li>%0 med %1 poster.`)]
       [Quot.toString `<a href="^(Html.genUrl ("#"^(#db_name source)) [])">
	^(ScsDict.s (#name source))</a>`,
	Int.toString (List.length persons)])
    val intro_text = ScsDict.s [(ScsLang.en,`External sources:`),
				(ScsLang.da,`Eksterne kilder:`)]
    val auto_import_link = 
      ScsDict.s [(ScsLang.en,`You may apply <a href="apply_auto_import.sml">auto import</a>.`),
		 (ScsLang.da,`Du kan anvende <a href="apply_auto_import.sml">automatisk inlæsning</a>.`)]

    val inconsistencies_link = 
      ScsDict.s [(ScsLang.en,`You may check for <a href="chk_inconsistencies.sml">inconsistencies</a>
                              in the central personnel register.`),
		 (ScsLang.da,`Du kan checke for <a href="chk_inconsistencies.sml">inkonsistente</a>
                              data i den centrale personregister.`)]
  in
    `^intro_text
    <ul>
    ` ^^ (List.foldl gen_entry `` imp_users) ^^ `
    </ul>
    ^auto_import_link <br>
    ^inconsistencies_link
    `
  end
val page_intro = gen_page_intro imp_users

fun gen_section ((source:ScsUserImp.external_source,persons),acc) = 
    acc ^^ 
    `<a name="^(#db_name source)"></a>
    <h3>^(ScsDict.s (#name source))</h3>
    ` ^^ (gen_table source persons)

val user_imp_sections = List.foldl gen_section `` imp_users

val _ = ScsUserImp.returnPg title
  (`<h1>^title</h1> 
   ` ^^ page_intro ^^ `
   <p>
   <h1>^(ScsDict.s [(ScsLang.en,`Look up one person`),
		    (ScsLang.da,`Søg efter en person`)])</h1>
  ` ^^ (ScsPerson.search_form "/scs/admin/user/imp/show_person_with_all_sources.sml" []) ^^ `
   <p>
   <h1>^(ScsDict.s [(ScsLang.en,`Rows to Import`),
		    (ScsLang.da,`Rækker der skal importeres`)])</h1>
   ` ^^ user_imp_sections)
