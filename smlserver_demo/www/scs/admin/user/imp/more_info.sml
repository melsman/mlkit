val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val (user_imp_id,errs) = ScsUserImp.getUserImpIdErr ("user_imp_id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors

val page_title = ScsDict.s [(ScsLang.en,`Central Personnel Register - More Info`),
			    (ScsLang.da,`Centralt Person Register - Mere Info`)]

val (first_names,last_name,norm_name,
     security_id, email, url, on_what_table, on_which_id,
     last_auto_import_try,last_modified,modifying_user,
     exact_match_id) =
  ScsDb.oneRowErrPg' (fn g => (g "first_names", g "last_name", g "norm_name",
			       g "security_id", g "email", g "url", g "on_what_table", g "on_which_id",
			       Db.toTimestamp (g "last_import"),
			       Db.toTimestamp (g "last_modified"), 
			       g "modifying_user",
			       g "exact_match_id"),
		      `select first_names, last_name, norm_name,
		              security_id, email, url, on_what_table, on_which_id,
                              ^(Db.toTimestampExp "last_auto_import_try") as last_import,
                              ^(Db.toTimestampExp "last_modified") as last_modified,
			      scs_person.name(modifying_user) as modifying_user,
                              scs_user.imp_exact_match(user_imp_id) as exact_match_id
                         from scs_user_imports
                        where scs_user_imports.user_imp_id = '^(Int.toString user_imp_id)'`,
			 ScsDict.s' [(ScsLang.en,`Import row does not exists`),
				     (ScsLang.da,`Importrække eksisterer ikke`)])

val create_new_component =
  if ScsError.wrapPanic 
    Db.existsOneRow `select email 
                       from scs_parties
                      where lower(email) = lower(^(Db.qqq email))`
    then
      UcsWidget.oneColumnText [``]
  else
    UcsWidget.oneColumnText
    [ScsDict.sl'
     [(ScsLang.en,`You may <a ^(UcsPage.confirmCreateOnClick()) href="%0">create</a> %1 as a new person.`),
      (ScsLang.da,`Du kan <a ^(UcsPage.confirmCreateOnClick()) href="%0">oprette</a> %1 som en ny person.`)]
     [Html.genUrl "imp_row.sml" [("user_imp_id",Int.toString user_imp_id)],
      Quot.toString `^first_names ^last_name`]]

val delete_component =
  UcsWidget.oneColumnText
  [ScsDict.sl'
   [(ScsLang.en,`You may <a ^(UcsPage.confirmDelOnClick()) href="%0">delete</a> %1 from the import table.`),
    (ScsLang.da,`Du kan <a ^(UcsPage.confirmDelOnClick()) href="%0">slette</a> %1 fra importtabellen.`)] 
   [Html.genUrl "del_imp_row.sml" [("user_imp_id",Int.toString user_imp_id)],
    Quot.toString `^first_names ^last_name`]]

val user_imp_info =
  [ScsUserImp.nameWidget (first_names ^ " " ^ last_name),
   ScsUserImp.normNameWidget norm_name,
   ScsUserImp.securityIdWidget security_id,
   ScsUserImp.emailWidget email,
   ScsUserImp.urlWidget url,
   ScsUserImp.externSourceWidget on_what_table on_which_id,
   ScsUserImp.lastImportWidget last_auto_import_try,
   ScsUserImp.lastModifiedWidget last_modified modifying_user,
   ScsUserImp.exactMatchWidget 
   (Int.toString user_imp_id) exact_match_id on_what_table security_id email norm_name,
   create_new_component,
   delete_component]

(* Get data on persons with same normalised name in DB *)
fun persons_in_db norm_name =
  ScsError.wrapPanic
   (Db.list (fn g => (g "person_id", g "first_names", g "last_name", g "norm_name",
		      g "security_id", g "email", g "url",
		      Db.toDate (g "last_modified"), g "modifying_user")))
       `select per.person_id, per.first_names, per.last_name, per.norm_name, 
               per.security_id, party.email, party.url,
               per.last_modified,
 	       scs_person.name(per.modifying_user) as modifying_user
          from scs_persons per, scs_parties party
         where (per.norm_name = lower(^(Db.qqq norm_name)) or
                party.email = lower(^(Db.qqq email)))
           and per.person_id = party.party_id
           and per.deleted_p = 'f'
           and party.deleted_p = 'f'
         order by per.last_name`

fun layout_person_db (person_id,first_names,last_name,norm_name,
		      security_id,email,url,
		      last_modified,modifying_user) =
  `<td><a href="^(Html.genUrl "show_person_with_all_sources.sml" 
		                             [("person_id",person_id)])">
					     <b>^first_names ^last_name</b></a></td>
   <td>^security_id</td>
   <td>^email</td>
   <td>` ^^ (ScsQuot.maybe `^url` (Html.ahref `url` `^(Html.genUrl url [])`)) ^^ `</td>
   <td>^(UcsWidget.valOfTimestamp last_modified) (^modifying_user)</td>
   <td>^(ScsUserImp.importLink user_imp_id (ScsError.wrapPanic (valOf o Int.fromString) person_id))</td>`

fun gen_table [] = ScsDict.s' [(ScsLang.en,`There are no persons to import in this group.`),
			       (ScsLang.da,`Der er ikke noget at importere i denne  gruppe.`)]
  | gen_table persons =
  let
    val title_with_norm_name =
      ScsDict.s [(ScsLang.en,`Below, you see persons with the same normalised name or email in the personnel register.`),
		 (ScsLang.da,`Nedenfor ser du de personer i personregistret med samme normaliserede navn eller email.`)]
  in
    `
    <hr>
    ^title_with_norm_name 
    ` ^^ 
    (ScsWidget.lineTable
     {hdcolor="silver",row_col1="silver",row_col2="lightgrey",
      header=`<th>^(ScsUserImp.nameField())</th>
              <th>^(ScsUserImp.securityIdField())</th>
              <th>^(ScsUserImp.emailField())</th>
              <th>^(ScsUserImp.urlField())</th>
              <th>^(ScsUserImp.lastModifiedField())</th>
              <th>^(ScsDict.s [(ScsLang.en,`Import as this person`),
			       (ScsLang.da,`Indlæs som denne person`)])</th>`,
      align="center",
      footer=``}
     layout_person_db
     persons)
						     end

val _ = ScsUserImp.returnPg page_title
  (`<h1>^page_title</h1> 
   <p>
   ` ^^ (UcsWidget.layoutComponentGrp (UcsWidget.V(user_imp_info))) ^^ `<p>
   ` ^^
   (gen_table (persons_in_db norm_name)))

