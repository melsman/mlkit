val (pat,errs) = ScsFormVar.getStringErr ("pat", ScsDict.s UcsDict.search_pattern_dict, ScsFormVar.emptyErr)
val (target_url,errs) = ScsFormVar.getStringErr ("target_url", ScsDict.s UcsDict.url_dict, errs)
val (mode_opt, errs) = (ScsFormVar.wrapMaybeOpt ScsFormVar.getStringErr) ("mode", "mode_opt", errs)
val _ = ScsFormVar.anyErrors errs

val pat = ScsString.mk_search_pattern pat
val query = `
      select first_names || ' ' || last_name || ' (' || email || ')' as name, 
	     per.person_id 
        from scs_persons_active per, scs_parties party ` ^^ ( case mode_opt of 
	    SOME "ucs_pr" => `, scs_person_rels prels`
	  | _		  => ``
	) ^^ `
       where per.person_id = party.party_id
         and ( full_name_lower like lower(^(Db.qqq pat)) or
	       lower(url) like lower(^(Db.qqq pat)) or
	       lower(email) like lower(^(Db.qqq pat)) or
	       lower(security_id) like lower(^(Db.qqq pat))
	     ) ` ^^ ( case mode_opt of 
		          SOME "ucs_pr" => `
	 and prels.person_id = per.person_id
	 and prels.on_what_table = 'person'`
			| _	        => ``
		    ) ^^ `
       order by name`
val items = ScsError.wrapPanic (Db.list (fn g => (`^(g "name")`,"person_id",  g "person_id"))) query

(* We must keep other hidden query data that should be given to the target url. *)
val query_data = 
  case Ns.Conn.getQuery() of 
    NONE => []
  | SOME s => Ns.Set.list s
val hvs = List.filter (fn (n,_) => n <> "pat" andalso n <> "target_url") query_data

val _ = 
  ScsWidget.pickFromList target_url ("submit",ScsDict.s [(ScsLang.en,`Choose person`),
							 (ScsLang.da,`Vælg person`)])
  hvs
  items
  (ScsDict.s [(ScsLang.en,`List of persons`),(ScsLang.da,`Personliste`)])
  (ScsDict.sl' [(ScsLang.en,`Below you find the persons that matches your search pattern (%0)`),
		(ScsLang.da,`Nedenfor finder du de personer som matcher dit søgemønster (%0)`)] [pat])
