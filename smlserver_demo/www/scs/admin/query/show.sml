val user_id = ScsLogin.auth_roles [ScsRole.StudAdm]

val query = ScsFormVar.wrapOpt ScsFormVar.getStringErr "query"
val id = ScsFormVar.wrapOpt ScsFormVar.getNatErr "id"
val submit = ScsFormVar.wrapOpt (ScsFormVar.getEnumErr ["Do Query","Add","Update"]) "submit"

val _ = 
  if submit = SOME "Add" then
    let
      val (category,errs) = ScsFormVar.getStringErr ("category","Category",ScsFormVar.emptyErr)
      val (name,errs) = ScsFormVar.getStringErr("name","name",errs)
      val (desc,errs) = ScsFormVar.getStringErr("description","description",errs)
      val (query,errs) = ScsFormVar.getStringErr("query","query",errs)
      val (arity,errs) = ScsFormVar.getIntErr("arity","arity",errs)
      val _ = ScsFormVar.anyErrors errs

      val q_id = Int.toString (Db.seqNextval "scs_query_id_seq")
    in
      (Db.dml `insert into scs_query 
                 (id,query,arity,category,name,description,create_date,create_user)
                values (^(Db.qqq q_id),^(Db.qqq query),'^(Int.toString arity)',^(Db.qqq category),
                        ^(Db.qqq name),^(Db.qqq desc),sysdate,'^(Int.toString (ScsLogin.user_id()))')`;
       Ns.returnRedirect ("show.sml?id="^q_id);
       Ns.exit())
    end
  else ()

val _ = 
  if submit = SOME "Update" then
    let
      val (category,errs) = ScsFormVar.getStringErr ("category","Category",ScsFormVar.emptyErr)
      val (name,errs) = ScsFormVar.getStringErr("name","name",errs)
      val (desc,errs) = ScsFormVar.getStringErr("description","description",errs)
      val (query,errs) = ScsFormVar.getStringErr("query","query",errs)
      val (arity,errs) = ScsFormVar.wrapIntAsString ScsFormVar.getIntErr("arity","arity",errs)
      val (q_id,errs) = ScsFormVar.wrapIntAsString ScsFormVar.getIntErr("id","id",errs)
      val _ = ScsFormVar.anyErrors errs
    in
      (Db.dml `update scs_query 
                  set ^(Db.setList [("query",query),("arity",arity),("category",category),
				    ("name",name),("description",desc)])
                where id = ^(Db.qqq q_id)`;
       Ns.returnRedirect ("show.sml?id="^q_id);
       Ns.exit())
    end
  else ()
 
val (cat,name,id,desc,query,arity) =
  case (query,id) of
    (NONE,NONE) => ("","","","","","0")
  | (SOME q,NONE) => ("","","","",q,"0")
  | (NONE,SOME id) =>
      ScsDb.oneRowErrPg' (fn g => (g "category", g "name", Int.toString id, 
				   g "description", g "query", g "arity"),
			  `select category, name, description, query, arity
			     from scs_query
			    where scs_query.id = ^(Db.qqq (Int.toString id))`, 
			    `Query does not exist in database`)
  | (SOME q, SOME id) => 
      ScsDb.oneRowErrPg' (fn g => (g "category", g "name", Int.toString id, 
				   g "description", q, "0"),
			  `select category, name, description
			     from scs_query
			    where scs_query.id = ^(Db.qqq (Int.toString id))`, 
			    `Query does not exist in database`)

fun f (s:Ns.Set.set,acc) = 
  let 
    val ls = List.rev (Ns.Set.list s)
  in
    case acc of
      NONE => SOME ((Quot.concatWith ";" (List.map (Quot.fromString o #2) ls) ^^ `^("\n")`) :: 
		    [Quot.concatWith ";" (List.map (Quot.fromString o #1) ls) ^^ `^("\n")`], 1)
    | SOME (acc,n) => SOME (((Quot.concatWith ";" (List.map (Quot.fromString o #2) ls)) ^^ `^("\n")`) ::
			    acc,n+1)
  end
    
val res = 
  if query = "" then 
    [`No query specified`]
  else
    (case Db.foldSet f NONE `^query` of
       NONE => [`No rows returned`]
     | SOME (res,n) => `There are ^(Int.toString n) rows<p>.` :: (List.rev res))
       handle Fail e => [`Database error: returned error:<p>
	 <pre>^e</pre>`]

val upd_add_but = if id = "" then ("submit","Add") else ("submit","Update")

val _ = ScsPage.returnPg "Query" (`Evaluate queries directly in the database<p>
` ^^ (ScsWidget.formBox "show.sml" [("submit","Do Query"), upd_add_but] 
      (`<input type=hidden name=id value="^id">
        Category: <input type=text name=category value="^cat"><br>
        Name: <input type=text name=name value="^name"><br>
        Arity: <input type=text name=arity value="^arity"><br>
        Description: <textarea name=description cols=30 rows=3>^desc</textarea><br>
        Query: <textarea name=query cols=120 rows=25>^query</textarea>`)) ^^ 
`
<hr>
<pre>
` ^^ (Quot.concat res) ^^ `
</pre>`)
