val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val title = ScsDict.s [(ScsLang.en,`Central Personnel Register - Check for inconsistencies`),
		       (ScsLang.da,`Centralt Person Register - Analyser for inkonsistente data`)]

val intro_text =
  ScsDict.s [(ScsLang.en,`This page shows you the result of various inconsistency checks.`),
	     (ScsLang.da,`Denne side viser dig resultatet af forskellige inkonsistens analyser på data
                          i personregisteret og de eksterne kilder`)]

(* Check for dublicate security ids in the personnel register
   Result table:
     name1 email1 name2 email2 security_id *)
local
  val rows =
    ScsError.wrapPanic
    (Db.list (fn g => (g "p1_name", g "p1_email",
		      g "p2_name", g "p2_email",
		      g "security_id")))
    `select scs_person.name(p1.person_id) as p1_name,
            party1.email as p1_email,
            scs_person.name(p2.person_id) as p2_name,
            party2.email as p2_email,
            p1.security_id
       from scs_persons p1, scs_parties party1, scs_persons p2, scs_parties party2
      where lower(p1.security_id) = lower(p2.security_id)
        and p1.person_id < p2.person_id
        and p1.person_id = party1.party_id
        and p2.person_id = party2.party_id
        and p1.deleted_p = 'f'
        and p2.deleted_p = 'f'
        and party1.deleted_p = 'f'
        and party2.deleted_p = 'f'`

  fun layout_row (p1_name, p1_email, p2_name, p2_email, security_id) =
    `<td><b>^p1_name</b></td>
     <td>^p1_email</td>
     <td><b>^p2_name</b></td>
     <td>^p2_email</td>
     <td>^security_id</td>`
  
  val title =
    ScsDict.s [(ScsLang.en,`Persons with equal security ids`),
	       (ScsLang.da,`Personer med samme cpr`)]

  val intro =
    ScsDict.s [(ScsLang.en,`The following rows shows persons with equal security ids.`),
	       (ScsLang.da,`De følgende rækker viser personer med samme cpr.`)]
in
  val security_id_section =
    `<h3>^title</h3>` ^^ 
    (case rows of
       [] => ScsDict.s' [(ScsLang.en,`There are no persons with equal security ids.`),
			 (ScsLang.da,`Der er ikke nogen personer med samme cpr.`)]
     | _ => 
	 `^intro <p>
	 ` ^^ 
	 (UcsPage.lineTable
	  {hdcolor="silver",row_col1="silver",row_col2="lightgrey",width="",
	   header=`<tr><th>^(ScsUserImp.nameField())</th>
	           <th>^(ScsUserImp.emailField())</th>
                   <th>^(ScsUserImp.nameField())</th>
	           <th>^(ScsUserImp.emailField())</th>
                   <th>^(ScsUserImp.securityIdField())</th></tr>`,
	   align="center",
	   footer=``} 
	  layout_row
	  rows))
end

local
  fun gen_rows (source: ScsUserImp.external_source, sql) =
    (source,
     ScsError.wrapPanic
     (Db.list (fn g => (g "person_id", g "on_what_table", g "on_which_id",
		      g "e_name", g "e_email",
		      g "e_security_id",
		      g "p_name",
		      g "p_email",
		      g "p_security_id"))) sql)

  fun layout_row (person_id, on_what_table, on_which_id, 
		  e_name, e_email, e_security_id,
		  p_name, p_email, p_security_id) =
    `<td>^on_what_table.^on_which_id</td>
     <td>^e_name</td>
     <td>^e_email</td>
     <td>^e_security_id</td>
     <td>^p_name</td>
     <td>^p_email</td>
     <td><a href="^(Html.genUrl "show_person_with_all_sources.sml" 
		                             [("person_id",person_id)])">
					     ^p_security_id</a></td>`
  
  fun all_empty rowss = List.all (fn (_,xs) => xs = []) rowss

  val central_reg_name = ScsDict.s [(ScsLang.en,`Central Personnel Register`),
				    (ScsLang.da,`Fælles Person Register`)]
  fun gen_all_tables rowss =
    List.foldl (fn ((source:ScsUserImp.external_source,rows),acc) => acc ^^ `<p>` ^^
		(case rows of
		   [] => `` 
                 | _ => (UcsPage.lineTable
		 {hdcolor="silver",row_col1="silver",row_col2="lightgrey",width="",
		  header=`<tr><th colspan="4" align="right">^(ScsDict.s (#name source))</th>
                              <th colspan="3" align="left">^central_reg_name</th></tr>
                          <tr><th>^(ScsUserImp.externSourceField())</th>
                              <th>^(ScsUserImp.nameField())</th>
                              <th>^(ScsUserImp.emailField())</th>
	                      <th>^(ScsUserImp.securityIdField())</th>
                              <th>^(ScsUserImp.nameField())</th>
	                      <th>^(ScsUserImp.emailField())</th>
                              <th>^(ScsUserImp.securityIdField())</th></tr>`,
		  align="center",
		  footer=``} 
		 layout_row
		 rows))) `` rowss
in
  (* For each external source, check that security id match
     Result table
       External Source                  Personnel Register 
       Relation Name Email Security id  Name Email Security id *)
  val rowss_security_id_chk = 
    List.foldr (fn (source:ScsUserImp.external_source,acc) => 
		case #security_id_chk_sql source of 
		  NONE => acc 
		| SOME sql => gen_rows(source,sql) :: acc) [] 
    (List.map ScsUserImp.getSource ScsUserImp.all_sources)

  val title_ext_security_id_section =
    ScsDict.s [(ScsLang.en,`Persons in central personnel register with security ids 
                            that differs from security ids in external sources`),
	       (ScsLang.da,`Personer i det centrale personregister med cpr numre som er
                            forskellige fra de der findes i de eksterne kilder`)]
  val intro_ext_security_id_section =
    ScsDict.s [(ScsLang.en,`The following tables show persons with security ids that are not equal.`),
	       (ScsLang.da,`De følgende tabeller viser personer med cpr som ikke er ens.`)]


  val ext_security_id_section =
    `<h3>^title_ext_security_id_section</h3>` ^^ 
    (if all_empty rowss_security_id_chk then
       ScsDict.s' [(ScsLang.en,`There are no persons with mismatching security ids.`),
		   (ScsLang.da,`Der er ikke nogen personer cpr numre,
                                som ikke matcher de eksterne kilder.`)]
     else
	 `^intro_ext_security_id_section <p>
	 ` ^^ (gen_all_tables rowss_security_id_chk))

  (* For each external source, check that lower case name match
     Result table
       External Source                  Personnel Register 
       Relation Name Email Security id  Name Email Security id *)
  val rowss_name_chk = 
    List.foldr (fn (source:ScsUserImp.external_source,acc) => 
		case #name_chk_sql source of 
		  NONE => acc 
		| SOME sql => gen_rows(source,sql) :: acc) [] 
    (List.map ScsUserImp.getSource ScsUserImp.all_sources)

  val title_ext_name_section =
    ScsDict.s [(ScsLang.en,`Persons in central personnel register with lower case names
                            that differs from lower case names in external sources`),
	       (ScsLang.da,`Personer i det centrale personregister med navne, som er
                            forskellige fra de der findes i de eksterne kilder`)]
  val intro_ext_name_section =
    ScsDict.s [(ScsLang.en,`The following tables show persons with names that are not equal.`),
	       (ScsLang.da,`De følgende tabeller viser personer med navne som ikke er ens.`)]

  val ext_name_section =
    `<h3>^title_ext_name_section</h3>` ^^ 
    (if all_empty rowss_name_chk then
       ScsDict.s' [(ScsLang.en,`There are no persons with mismatching names.`),
		   (ScsLang.da,`Der er ikke nogen personer med navne,
                                som ikke matcher de eksterne kilder.`)]
     else
	 `^intro_ext_name_section <p>
	 ` ^^ (gen_all_tables rowss_name_chk))


  (* For each external source, check that lower email match
     Result table
       External Source                  Personnel Register 
       Relation Name Email Security id  Name Email Security id *)
  val rowss_email_chk = 
    List.foldr (fn (source:ScsUserImp.external_source,acc) => 
		case #email_chk_sql source of 
		  NONE => acc 
		| SOME sql => gen_rows(source,sql) :: acc) [] 
    (List.map ScsUserImp.getSource ScsUserImp.all_sources)

  val title_ext_email_section =
    ScsDict.s [(ScsLang.en,`Persons in central personnel register with lower case emails
                            that differs from lower case emails in external sources`),
	       (ScsLang.da,`Personer i det centrale personregister med emails, som er
                            forskellige fra de der findes i de eksterne kilder`)]
  val intro_ext_email_section =
    ScsDict.s [(ScsLang.en,`The following tables show persons with emails that are not equal.`),
	       (ScsLang.da,`De følgende tabeller viser personer med emails som ikke er ens.`)]

  val ext_email_section =
    `<h3>^title_ext_email_section</h3>` ^^ 
    (if all_empty rowss_email_chk then
       ScsDict.s' [(ScsLang.en,`There are no persons with mismatching emails.`),
		   (ScsLang.da,`Der er ikke nogen personer med emails,
                                som ikke matcher de eksterne kilder.`)]
     else
	 `^intro_ext_email_section <p>
	 ` ^^ (gen_all_tables rowss_email_chk))


end

val _ = ScsUserImp.returnPg title
  (`<h1>^title</h1> 
   ^intro_text <p>

   ` ^^ security_id_section ^^ `

   ` ^^ ext_security_id_section ^^ `

   ` ^^ ext_name_section ^^ `

   ` ^^ ext_email_section)



