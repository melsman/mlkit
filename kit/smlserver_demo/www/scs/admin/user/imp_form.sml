val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val title = ScsDict.s [(ScsLang.en,`Import New Users`),(ScsLang.da,`Importer Nye Brugere`)]

val imp_users = Db.list (fn g => (g "user_imp_id", g "first_names", g "last_name", g "norm_name",
                                  g "security_id", g "email", g "url", g "on_what_table", g "on_which_id",
                                  Db.toDate (g "last_auto_import_try"),
                                  Option.valOf(Db.toDate (g "last_modified")), g "modifying_user",
                                  g "exact_match_id"))
                              `select scs_user_imports.*,
                                      scs_user.imp_exact_match(user_imp_id) as exact_match_id
                                 from scs_user_imports
                                order by exact_match_id,last_modified`

fun layout_user title empty_str user_imp_id person_id =
case
    Db.zeroOrOneRow' (fn g => (g "first_names", g "last_name", g "norm_name", 
                               g "security_id", g "email", g "url"))
       `select scs_persons.*, scs_parties.*
          from scs_persons, scs_parties
         where scs_persons.person_id = scs_parties.party_id
           and scs_persons.person_id = '^(person_id)'` of
    NONE => `<td align=left><table border=0><tr><td align=0>^title</td><td>^empty_str</td></tr></table></td>`
  | SOME (first_names,last_name,norm_name,security_id,email,url) =>
    `
       <tr><td align=left>
       <table border=1>
         <tr><td>^("Name"):</td><th align=left><b>^first_names ^last_name</b></th></tr>
         <tr><td>^("Norm"):</td><td>^norm_name</td></tr>
         <tr><td>^("Security id"):</td><td>^security_id</td></tr>
         <tr><td>^("Email"):</td><td>^email</td></tr>
         <tr><td>^("Url"):</td><td>^url</td></tr> ` ^^
            (Db.fold (fn (g,acc) => acc ^^ `<tr><td>^("Relation"):</td><td>
                 ^(g "on_what_table").^(g "on_which_id")</td></tr>`) ``
               `select scs_person_rels.on_what_table,scs_person_rels.on_which_id
                  from scs_persons, scs_person_rels
                 where scs_persons.person_id = '^(person_id)'
                   and scs_persons.person_id = scs_person_rels.person_id`) ^^ `
       </table>
      </td>
	    <td><a href="^(Html.genUrl "imp_row.sml" 
                            [("user_imp_id",user_imp_id),
                             ("person_id",person_id)])">^("Import as same person")</a></td></tr>`

fun layout_same_norm_name user_imp_id norm_name =
  Db.fold (fn (g,acc) => acc ^^ layout_user ("Maybe same person") ("None") 
                                            user_imp_id (g "person_id")) `` 
    `select person_id
      from scs_persons
     where scs_persons.norm_name = ^(Db.qqq norm_name)`

fun layout_imp_user (user_imp_id,first_names,last_name,norm_name,security_id,email,
                     url,on_what_table,on_which_id,last_auto_import_try,last_modified,
                     modifying_user,exact_match_id) =
  `<td align=left>
   <table border=0>
    <tr><td align=left>
     <table border=0>
     <tr><td>^("Name"):</td><th align=left><b>^first_names ^last_name</b></th></tr>
     <tr><td>^("Norm"):</td><td>^norm_name</td></tr>
     <tr><td>^("Security id"):</td><td>^security_id</td></tr>
     <tr><td>^("Email"):</td><td>^email</td></tr>
     <tr><td>^("Url"):</td><td>^url</td></tr>
     <tr><td>^("Source"):</td><td>^on_what_table.^on_which_id</td></tr>
     <tr><td>^("Last import try"):</td><td>^(UcsWidget.valOfDate last_auto_import_try)</td></tr>
     </table>
    </td>
	    <td><a href="^(Html.genUrl "imp_row.sml" 
                            [("user_imp_id",user_imp_id)])">^("Create as new user")</a><br>
                          <a href="^(Html.genUrl "del_imp_row.sml" 
                            [("user_imp_id",user_imp_id)])">^("Delete row from import table")</a>
</td>
     </tr>
     ` ^^ (layout_user ("Exact match") ("None") user_imp_id exact_match_id) ^^ 
          (layout_same_norm_name user_imp_id norm_name) ^^ `
    </table>
    </td>`

val user_imp_table =
  (ScsWidget.lineTable
     {hdcolor="silver",row_col1="silver",row_col2="lightgrey",
      header=`<th>^("User")</th>`,
      align="center",
      footer=``}
       layout_imp_user imp_users)

val _ = UcsPage.returnPg title
  (`<h1>^title</h1> 
   ^(ScsDict.sl [(ScsLang.en,`There are %0 entries.`),(ScsLang.da,`Der er %0 poster.`)] [Int.toString (List.length imp_users)])<p>
   <h2><a href="^(Html.genUrl "apply_auto_import.sml" [])">^(ScsDict.s[(ScsLang.en,`Apply auto import`),
								       (ScsLang.da,`Anvend auto import`)])</h2>
   ` ^^ user_imp_table)
