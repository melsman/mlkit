val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val (person_id,errs) = 
  ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val title = ScsDict.s [(ScsLang.en,`Central Personnel Register - Show external sources on one person`),
		       (ScsLang.da,`Centralt Person Register - Vis eksterne kilder på en person`)]

val (name,email,security_id) =
  ScsError.wrapPanic
  (Db.oneRow' (fn g => (g "name", g "email", g "security_id")))
  `select scs_person.name(person_id) as name, scs_parties.email, scs_persons.security_id
     from scs_persons, scs_parties
    where scs_persons.person_id = scs_parties.party_id
      and scs_persons.person_id = ^(Db.qqq (Int.toString person_id))`

val intro_text =
  ScsDict.sl [(ScsLang.en,`This page shows you the name, security_id and email stored in the external
	                   sources that have been read into the central personnel register 
                           for this person: <b>%0</b>`),
	      (ScsLang.da,`Denne side viser navn, cpr og email gemt i eksterne kilder, som er blevet
                           læst ind i det centrale personregister for denne person: <b>%0</b>`)]
  [name]

fun gen_rows (name, sql) =
  (name,
   ScsError.wrapPanic
   (Db.list (fn g => (g "name", g "email", g "security_id"))) sql)

fun layout_row (name, email, security_id) =
    `<td><b>^name</b></td>
     <td>^email</td>
     <td>^security_id</td>`

fun gen_table (name,rows) =
  case rows of
    [] => ``
  | _ => `<p>` ^^
	 (UcsPage.lineTable
	  {hdcolor="silver",row_col1="silver",row_col2="lightgrey",width="",
	   header=`<tr><th align="center" colspan="3">^(ScsDict.s name)</th></tr>
	           <tr><th>^(ScsUserImp.nameField())</th>
	           <th>^(ScsUserImp.emailField())</th>
                   <th>^(ScsUserImp.securityIdField())</th></tr>`,
	   align="center",
	   footer=``} 
	  layout_row
	  rows)

fun gen_tables rowss =
  List.foldl (fn (rows,acc) => acc ^^ gen_table rows) `` rowss

val rowss = 
  List.foldr (fn (source:ScsUserImp.external_source,acc) =>
	      gen_rows(#name source,(#basic_info_sql source) (Int.toString person_id)) :: acc) []
  (List.map ScsUserImp.getSource ScsUserImp.all_sources)

(* Add central personnel register info *)
val rowss = (ScsUserImp.service_name, [(name,email,security_id)]) :: rowss

val _ = UcsPage.returnPg title
  (`<h1>^title</h1> 
   ^intro_text <p>
   ` ^^ (gen_tables rowss))
