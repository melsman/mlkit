val d = ScsDict.d ScsLang.en "scs/person" "person_search.sml"
val dl' = ScsDict.dl' ScsLang.en "scs/person" "person_search.sml"

val (pat,errs) = ScsFormVar.getStringErr ("pat", d"Search pattern", ScsFormVar.emptyErr)
val (target_url,errs) = ScsFormVar.getStringErr ("target_url", d"Target url", errs)
val _ = ScsFormVar.anyErrors errs

val pat = if pat = "" then "%" else "%"^pat^"%"
val query = `select first_names || ' ' || last_name || ' (' || email || ')' as name, person_id 
               from scs_persons_active per, scs_parties party
              where per.person_id = party.party_id
                and (lower(first_names || ' ' || last_name) like lower(^(Db.qqq pat)) or
		     lower(url) like lower(^(Db.qqq pat)) or
                     lower(email) like lower(^(Db.qqq pat)) or
                     lower(norm_name) like lower(^(Db.qqq pat)) or
                     lower(security_id) like lower(^(Db.qqq pat)))
              order by name`
val items = Db.list (fn g => (`^(g "name")`,"person_id",  g "person_id")) query

(* We must keep other hidden query data that should be given to the target url. *)
val query_data = 
  case Ns.Conn.getQuery() of 
    NONE => []
  | SOME s => Ns.Set.list s
val hvs = List.filter (fn (n,_) => n <> "pat" andalso n <> "target_url") query_data

val _ = 
  ScsWidget.pickFromList target_url ("submit",d"Choose Person")
  hvs
  items
  (d"List of persons")
  (dl' [pat] `Below you find the persons that matches your search pattern (%0)`)
