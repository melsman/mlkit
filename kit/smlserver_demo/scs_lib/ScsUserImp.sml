(* External Sources *)

signature SCS_USER_IMP =
  sig

    exception ScsUserImp of string

    (* external_source: necessary information about external sources,
       that is, name, abbreviation used in table scs_user_imports *)
    type external_source = {name: ScsDict.dict, db_name: string,
			    security_id_chk_sql : quot option,
			    name_chk_sql : quot option,
			    email_chk_sql : quot option,
			    (* rel_to_del_row checks that all relations in the
			       central register is to non deleted rows in the
                               external sources *)
			    rel_to_del_row_chk_sql : quot option,
			    basic_info_sql : string -> quot}

    (* external_sources: datatype holding the external sources available. *)
    datatype external_sources =
      SysAdmLogin of external_source
    | HSASPer of external_source
    | PersonnelRoster of external_source

    (* The supported external sources *)
    val sysadm_login     : external_sources
    val hsas_per         : external_sources
    val personnel_roster : external_sources
    val all_sources      : external_sources list

    (* [getSource ext_source] returns the external source record *)
    val getSource : external_sources -> external_source

    (* [fromDb name] returns the external source datatype matching
        name. Raises ScsUserImp in case name is not a defined external
        source *)
    val fromDb : string -> external_sources

    (* Field names and widgets *)
    val nameField          : unit -> string
    val nameWidget         : string -> UcsWidget.component
    val normNameField      : unit -> string
    val normNameWidget     : string -> UcsWidget.component
    val securityIdField    : unit -> string
    val securityIdWidget   : string -> UcsWidget.component
    val extSrcDelField     : unit -> string
    val emailField         : unit -> string
    val emailWidget        : string -> UcsWidget.component
    val urlField           : unit -> string
    val urlWidget          : string -> UcsWidget.component
    val externSourceField  : unit -> string
    val externSourceWidget : string -> string -> UcsWidget.component
    val lastImportField    : unit -> string
    val lastImportWidget   : Date.date option -> UcsWidget.component 
    val lastModifiedField  : unit -> string
    val lastModifiedWidget : Date.date option -> string -> UcsWidget.component
    val exactMatchField    : unit -> string
    val exactMatchWidget   : string -> string -> string -> string -> string -> string -> UcsWidget.component
    val deleteField        : unit -> string
    val moreInfoField      : unit -> string
    val delRelField        : unit -> string

    (* [getUserImpIdErr (fv,errs)] function to check fv-error for
       scs_user_imports.user_imp_id *)
    val getUserImpIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

    (* [getExtSourceErr (fv,errs)] function to check fv-error for
       external sources (the db-names) *)
    val getExtSourceErr : string * ScsFormVar.errs -> string * ScsFormVar.errs

    (* [getOnWhichIdErr (fv,errs)] function to check fv-error for
       on_which_id error *)
    val getOnWhichIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

    (* [exactMatchLink user_imp_id person_id] if person_id exists, then an
        exact match exists, and return a link that will merge
        user_imp_id with person_id *)
    val exactMatchLink : string -> string -> string
 
    (* [delLink user_imp_id] returns a link to file that will remove
       user_imp_id from scs_user_imports. *)
    val delLink : string -> string

    (* [moreInfoLink user_imp_id] returns a link to file that will
       show more info for record user_imp_id in table scs_user_imports *)
    val moreInfoLink : string -> string

    (* [importLink user_imp_id person_id] returns a link to file that
       will import the user_imp_id into user person_id in the central
       personnel register *)
    val importLink : int -> int -> string

    (* [service_name] is the name of this service (i.e., Central
       Personnel Register) *)
    val service_name : ScsDict.dict

    (* [service_adm_email] is the email of the person who
       administrates this service. *)
    val service_adm_email : string

    val returnPg : string -> quot -> Ns.status
  end

structure ScsUserImp :> SCS_USER_IMP =
  struct
    exception ScsUserImp of string

    type external_source = {name: ScsDict.dict, db_name: string,
			    security_id_chk_sql : quot option,
			    name_chk_sql : quot option,
			    email_chk_sql : quot option,
			    rel_to_del_row_chk_sql : quot option,
			    basic_info_sql : string -> quot}

    datatype external_sources =
      SysAdmLogin of external_source
    | HSASPer of external_source
    | PersonnelRoster of external_source

    val sysadm_login =
      SysAdmLogin {name= [(ScsLang.en,`Login from IT-department`),
			  (ScsLang.da,`Login fra IT afdeling`)],
		   db_name = "sysadm_login",
		   security_id_chk_sql = SOME
		     `select p.person_id, pr.on_what_table, pr.on_which_id,
                             '' as e_name, 
                             e.login as e_email, 
                             e.cpr as e_security_id,
                             scs_person.name(p.person_id) as p_name,
                             party.email as p_email,
                             p.security_id as p_security_id
                        from sysadm_login_w e,scs_persons p,scs_person_rels pr, scs_parties party
                       where e.id = pr.on_which_id 
                         and pr.on_what_table='sysadm_login'
                         and p.person_id = pr.person_id
                         and p.person_id = party.party_id
                         and e.cpr is not null
                         and ucs_hsas_dw.neqT_null(p.security_id,e.cpr) = 't'`,
		   name_chk_sql = NONE,
		   email_chk_sql = SOME
		     `select p.person_id,pr.on_what_table, pr.on_which_id,
                             '' as e_name, 
                             e.login as e_email, 
                             e.cpr as e_security_id,
                             scs_person.name(p.person_id) as p_name,
                             party.email as p_email,
                             p.security_id as p_security_id
                        from sysadm_login_w e,scs_persons p,scs_person_rels pr, scs_parties party
                       where e.id = pr.on_which_id 
                         and pr.on_what_table='sysadm_login'
                         and p.person_id = pr.person_id
                         and p.person_id = party.party_id
                         and e.login is not null
                         and ucs_hsas_dw.neqT_null(lower(party.email),
                                                   lower(concat(e.login,'@it-c.dk'))) = 't'`,
	        rel_to_del_row_chk_sql = SOME
		  `select p.person_id, pr.on_what_table, pr.on_which_id,
                          '' as e_name,
                          e.login as e_email,
                          e.cpr as e_security_id,
                          scs_person.name(p.person_id) as p_name,
                          party.email as p_email,
                          p.security_id as p_security_id
                     from sysadm_login e, scs_persons p, scs_person_rels pr, scs_parties party
                    where e.id = pr.on_which_id
                      and pr.on_what_table = 'sysadm_login'
                      and p.person_id = pr.person_id
                      and p.person_id = party.party_id
                      and e.deleted_p = 't'
		  `,
		basic_info_sql = fn id => `select '' as name,
                                                  login || '@it-c.dk' as email,
                                                  cpr as security_id,
                                                  on_what_table,
                                                  on_which_id,
						  sysadm_login.deleted_p as e_deleted_p
                                             from sysadm_login, scs_person_rels
                                            where scs_person_rels.person_id = ^(Db.qqq id)
                                              and scs_person_rels.on_what_table = 'sysadm_login'
                                              and scs_person_rels.on_which_id = sysadm_login.id`}

    val hsas_per =
      HSASPer {name= [(ScsLang.en,`Student database (HSAS)`),
		      (ScsLang.da,`Studenterdatabase (HSAS)`)],
	       db_name = "hsas_per",
	       security_id_chk_sql = SOME
                 `select p.person_id,pr.on_what_table, pr.on_which_id,
                         e.fornavn || ' ' || e.efternavn as e_name, 
                         '' as e_email, 
                         e.cpr as e_security_id,
                         scs_person.name(p.person_id) as p_name,
                         party.email as p_email,
                         p.security_id as p_security_id
                    from hsas_per_w e,scs_persons p,scs_person_rels pr, scs_parties party
                   where e.id = pr.on_which_id 
                     and pr.on_what_table='hsas_per'
                     and p.person_id = pr.person_id
                     and p.person_id = party.party_id
                     and e.cpr is not null
                     and ucs_hsas_dw.neqT_null(p.security_id,e.cpr) = 't'`,
	       name_chk_sql = SOME
	         `select p.person_id,pr.on_what_table, pr.on_which_id,
                         e.fornavn || ' ' || e.efternavn as e_name, 
                         '' as e_email, 
                         e.cpr as e_security_id,
                         scs_person.name(p.person_id) as p_name,
                         party.email as p_email,
                         p.security_id as p_security_id
                    from hsas_per_w e,scs_persons p,scs_person_rels pr, scs_parties party
                   where e.id = pr.on_which_id 
                     and pr.on_what_table='hsas_per'
                     and p.person_id = pr.person_id
                     and p.person_id = party.party_id
                     and e.fornavn is not null
                     and e.efternavn is not null
                     and ucs_hsas_dw.neqT_null(lower(scs_person.name(p.person_id)),
                                               lower(e.fornavn || ' ' || e.efternavn)) = 't'`,
	       email_chk_sql = NONE,
	       rel_to_del_row_chk_sql = SOME 
	         `select p.person_id,pr.on_what_table, pr.on_which_id,
                         e.fornavn || ' ' || e.efternavn as e_name, 
                         '' as e_email, 
                         e.cpr as e_security_id,
                         scs_person.name(p.person_id) as p_name,
                         party.email as p_email,
                         p.security_id as p_security_id
                    from hsas_per e,scs_persons p,scs_person_rels pr, scs_parties party
                   where e.id = pr.on_which_id 
                     and pr.on_what_table='hsas_per'
                     and p.person_id = pr.person_id
                     and p.person_id = party.party_id
                     and e.deleted_p = 't'`,
	       basic_info_sql = fn id => `select fornavn || ' ' || efternavn as name,
                                                 '' as email,
                                                 cpr as security_id,
                                                 on_what_table,
                                                 on_which_id,
						 hsas_per.deleted_p as e_deleted_p
                                            from hsas_per, scs_person_rels
                                           where scs_person_rels.person_id = ^(Db.qqq id)
                                             and scs_person_rels.on_what_table = 'hsas_per'
                                             and scs_person_rels.on_which_id = hsas_per.id`}

    val personnel_roster = 
      PersonnelRoster {name = [(ScsLang.en,`Personnel Roster`),
			       (ScsLang.da,`Personfortegnelse`)],
		       db_name = "person",
		       security_id_chk_sql = SOME
  		         `select p.person_id,pr.on_what_table, pr.on_which_id,
                                 e.fornavne || ' ' || e.efternavn as e_name, 
                                 e.email as e_email, 
                                 e.person_cpr as e_security_id,
                                 scs_person.name(p.person_id) as p_name,
                                 party.email as p_email,
                                 p.security_id as p_security_id
                            from person e,scs_persons p,scs_person_rels pr, scs_parties party
                           where e.person_id = pr.on_which_id 
                             and pr.on_what_table='person'
                             and p.person_id = pr.person_id
                             and p.person_id = party.party_id
                             and e.person_cpr is not null
                             and ucs_hsas_dw.neqT_null(lower(p.security_id),
                                                       lower(e.person_cpr)) = 't'`,
		       name_chk_sql = SOME
  		         `select p.person_id,pr.on_what_table, pr.on_which_id,
                                 e.fornavne || ' ' || e.efternavn as e_name, 
                                 e.email as e_email, 
                                 e.person_cpr as e_security_id,
                                 scs_person.name(p.person_id) as p_name,
                                 party.email as p_email,
                                 p.security_id as p_security_id
                            from person e,scs_persons p,scs_person_rels pr, scs_parties party
                           where e.person_id = pr.on_which_id 
                             and pr.on_what_table='person'
                             and p.person_id = pr.person_id
                             and p.person_id = party.party_id
                             and e.fornavne is not null
                             and e.efternavn is not null
                             and ucs_hsas_dw.neqT_null(lower(scs_person.name(p.person_id)),
                                                       lower(e.fornavne || ' ' || e.efternavn)) = 't'`,
 		       email_chk_sql = SOME
                         `select p.person_id,pr.on_what_table, pr.on_which_id,
                                 e.fornavne || ' ' || e.efternavn as e_name, 
                                 e.email as e_email, 
                                 e.person_cpr as e_security_id,
                                 scs_person.name(p.person_id) as p_name,
                                 party.email as p_email,
                                 p.security_id as p_security_id
                            from person e,scs_persons p,scs_person_rels pr, scs_parties party
                           where e.person_id = pr.on_which_id 
                             and pr.on_what_table='person'
                             and p.person_id = pr.person_id
                             and p.person_id = party.party_id
                             and e.email is not null
                             and ucs_hsas_dw.neqT_null(lower(party.email),
                                                       lower(e.email)) = 't'`,
	               rel_to_del_row_chk_sql = NONE,
		       basic_info_sql = fn id => `select fornavne || ' ' || efternavn as name, 
                                                         email,
                                                         person_cpr as security_id,
                                                         on_what_table,
                                                         on_which_id,
                                                         'f' as e_deleted_p
                                                    from person, scs_person_rels
                                                   where scs_person_rels.person_id = ^(Db.qqq id)
                                                     and scs_person_rels.on_what_table = 'person'
                                                     and scs_person_rels.on_which_id = person.person_id`}

    val all_sources = [personnel_roster,hsas_per,sysadm_login]

    fun getSource ext_source =
      case ext_source of
	SysAdmLogin s => s
      | HSASPer s => s
      | PersonnelRoster s => s

    fun fromDb name =
      case name of
	"sysadm_login" => sysadm_login
      | "hsas_per" => hsas_per
      | "person" => personnel_roster
      | _ => raise ScsUserImp ("ScsUserImp.fromDb. Does not recognize " ^ name)

   (* Field names and widgets *)
    local
      val titles = [(ScsLang.en,`Name`),(ScsLang.da,`Navn`)]
    in
      fun nameField () = ScsDict.s titles
      fun nameWidget v =
	UcsWidget.twoColumns(titles, [], false, [`^v`])
    end

    local
      val info = [(ScsLang.en,`Lower case condensed name with only one first name and last name`),
		  (ScsLang.da,`Kondenseret navn med kun et fornavn og efternavn og i små bogstaver`)]
      val titles = [(ScsLang.en,`Normalised name`),(ScsLang.da,`Normaliseret navn`)]
    in
      fun normNameField () = ScsDict.s titles
      fun normNameWidget v =
	UcsWidget.twoColumns(titles, info, false, [`^v`])
    end

    local
      val info = [(ScsLang.en,`Security id for the person (may be empty)`),
		  (ScsLang.da,`Cpr for personen (kan være tom)`)]
      val titles = [(ScsLang.en,`Security id`),(ScsLang.da,`Cpr`)]
    in
      fun securityIdField () = ScsDict.s titles
      fun securityIdWidget v =
	UcsWidget.twoColumns(titles, info, false, [`^v`])
    end

    local
      val titles = [(ScsLang.en,`Ext. src. deleted`),
		    (ScsLang.da,`Ekstern kilde slettet`)]
    in
      fun extSrcDelField() = ScsDict.s titles
    end

    local
      val titles = [(ScsLang.en,`Email`),(ScsLang.da,`Email`)]
    in
      fun emailField () = ScsDict.s titles
      fun emailWidget v =
	UcsWidget.twoColumns(titles, [], false, [`^v`])
    end

    local
      val info = [(ScsLang.en,`Url to home page.`),
		  (ScsLang.da,`Url til hjemmeside`)]
      val titles = [(ScsLang.en,`Url (home page)`),(ScsLang.da,`Url (hjemmeside)`)]
    in
      fun urlField () = ScsDict.s titles
      fun urlWidget v =
	UcsWidget.twoColumns(titles, info, false, [`^v`])
    end

    local
      val info = [(ScsLang.en,`Table and id on external source.`),
		  (ScsLang.da,`Tabel og id på ekstern kilde.`)]
      val titles = [(ScsLang.en,`External source`),(ScsLang.da,`Ekstern kilde`)]
    in
      fun externSourceField () = ScsDict.s titles
      fun externSourceWidget table id =
	UcsWidget.twoColumns(titles, info, false, [`^table.^id`])
    end

    local
      val info = [(ScsLang.en,`Timestamp for last time we tried to import row.`),
		  (ScsLang.da,`Tidsstempel på sidste gang vi prøvede at importere række.`)]
      val titles = [(ScsLang.en,`Last Import`),(ScsLang.da,`Sidste Import`)]
    in
      fun lastImportField () = ScsDict.s titles
      fun lastImportWidget date =
	UcsWidget.twoColumns(titles, info, false, [`^(UcsWidget.valOfTimestamp date)`])
    end

    local
      val info = [(ScsLang.en,`Timestamp for last time row was modified.`),
		  (ScsLang.da,`Tidsstempel på sidste gang rækken blev ændret.`)]
      val titles = [(ScsLang.en,`Last Modification`),(ScsLang.da,`Sidste Ændring`)]
    in
      fun lastModifiedField () = ScsDict.s titles
      fun lastModifiedWidget date user_name =
	UcsWidget.twoColumns(titles, info, false, [`^(UcsWidget.valOfTimestamp date) (^user_name)`])
    end

    fun importLink user_imp_id person_id =
      let
	val msg = ScsDict.sl [(ScsLang.en,`Please confirm, that you want to import %0`),
			      (ScsLang.da,`Bekræft venligst, at du vil importere %0`)]
	                     [ScsPerson.name person_id]
      in
	Quot.toString
          `<a ^(UcsPage.confirmOnClick msg) href="^(Html.genUrl "imp_row.sml" [("user_imp_id",Int.toString user_imp_id),
                                                               		       ("person_id",Int.toString person_id)])">import</a>`
      end

    local
      val info = [(ScsLang.en,`If there is an exact match, then there exists a record in the
                               central register that matches this record.`),
		  (ScsLang.da,`Hvis der er et eksakt match, så betyder det at der i det centrale
                               personregister allerede findes en post, som svarer til denne.`)]
      val titles = [(ScsLang.en,`Exact Match`),(ScsLang.da,`Eksakt Match`)]
    in
      fun exactMatchField () = ScsDict.s titles
      fun exactMatchLink user_imp_id person_id =
	case person_id of
	  "" => ScsDict.s [(ScsLang.en,`None`),(ScsLang.da,`Nej`)]
	| person_id => 
	    importLink (ScsError.wrapPanic (valOf o Int.fromString) user_imp_id)
	               (ScsError.wrapPanic (valOf o Int.fromString) person_id)

      fun exactMatchWidget user_imp_id person_id on_what_table cpr email norm_name =
	case person_id of
	  "" => (* No exact match, so we execture a few queries in order 
                   to tell the user what may be the problem. *)
	    let
              (* It's a new record *)
	      val new_record = 
		ScsError.wrapPanic Db.oneField `select count(*)
                                                  from scs_person_rels
                                                 where on_what_table = ^(Db.qqq on_what_table)
                                                   and on_which_id = ^(Db.qqq user_imp_id)`
	      val new_record = ScsError.valOf (Int.fromString new_record)
	      val msg = 
		if new_record = 0 then
		  ScsDict.s' [(ScsLang.en,`<li>the record to import is new`),
			      (ScsLang.da,`<li>posten er ikke tidligere indlæst`)]
		else
		  ``
	      (* No security id *)
	      val msg = msg ^^
		(if cpr = "" then
		   ScsDict.s' [(ScsLang.en,`<li>the record to import has no security id`),
			       (ScsLang.da,`<li>posten har ikke noget cpr`)]
		 else
		   ``)
	      (* No security id or to many security ids in DB *)
	      val no_security_id_str = ScsError.wrapPanic Db.oneField `select count(*)
                                                                         from scs_persons
                                                                        where lower(security_id) = lower(^(Db.qqq cpr))
                                                                          and deleted_p = 'f'`
	      val no_security_id = ScsError.valOf (Int.fromString no_security_id_str)
	      val msg = msg ^^
		(if no_security_id <> 1 andalso cpr <> "" then
		   ScsDict.sl' [(ScsLang.en,`<li>there are %0 records in the personnel register with the same security id`),
				(ScsLang.da,`<li>der er %0 poster i person registeret med samme cpr.`)] 
		   [no_security_id_str]
		 else
		   ``)
              (* No email and norm_name in the record to import *)            
              val msg = msg ^^ 
		(if email = "" orelse norm_name = "" then
		   ScsDict.s' [(ScsLang.en,`<li>either email or normalised name or both are empty in record to import`),
			       (ScsLang.da,`<li>enten er email eller normaliseret navn eller begge tomme i posten.`)]
		 else
		   ``)
              (* No matching email and norm_name in DB *)            
	      val no_email_norm_name = 
		ScsError.wrapPanic Db.oneField `select count(*)
                                                  from scs_persons,scs_parties
                                                 where scs_persons.person_id = scs_parties.party_id
						   and lower(email) = lower(^(Db.qqq email))
                                                   and norm_name = ^(Db.qqq norm_name)
                                                   and scs_persons.deleted_p = 'f'
                                                   and scs_parties.deleted_p = 'f'`
	      val no_email_norm_name = ScsError.valOf (Int.fromString no_email_norm_name)
	      val msg = msg ^^
		(if no_email_norm_name = 0 andalso email <> "" andalso norm_name <> "" then
		   ScsDict.s' [(ScsLang.en,`<li>no matching email and normalised name in personnel register`),
			       (ScsLang.da,`<li>ingen tilsvarende email og normaliseret navn i person register.`)]
		 else
		   ``)
	      val msg = 
		if Quot.==(msg,``) then 
		  ScsDict.s' [(ScsLang.en,`There are no exact match`),
			      (ScsLang.da,`Der er ikke noget eksakt match`)]
		else 
		  ScsDict.s' [(ScsLang.en,`There are no exact match because:`),
			      (ScsLang.da,`Der er ikke noget eksakt match fordi:`)] ^^ 
		  `<ol>` ^^ msg ^^ `</ol>`

	      (* No exacts match exists, so it is interesting to know if the row would be created automatically. *)
	      val no_norm_name_str = 
		ScsError.wrapPanic Db.oneField `select count(*)
                                                  from scs_persons
                                                 where norm_name = ^(Db.qqq norm_name)
                                                   and scs_persons.deleted_p = 'f'`
	      val no_norm_name = ScsError.valOf (Int.fromString no_norm_name_str)
	      val msg = msg ^^ 
		(if norm_name = "" then
		   ScsDict.s' [(ScsLang.en,`Applying auto import will not create the row because the name field is empty.`),
			       (ScsLang.da,`Automatisk import vil ikke oprette denne række, da feltet navn er tomt.`)]
		 else if no_norm_name = 0 then
		   ScsDict.s' [(ScsLang.en,`Applying auto import will create the row because no other rows in the
                                            personnel register exists with same normalised name.`),
			       (ScsLang.da,`Automatisk import vil oprette denne række, 
                                            da der ikke findes andre personer med samme normaliseret navn.`)]
		      else
			ScsDict.sl' [(ScsLang.en,`The record will not be created automatically because %0 other rows
				                  exists with same normalised name.`),
				     (ScsLang.da,`Posten kan ikke oprettes automatisk idet der findes %0 andre rækker
			                          med samme normaliseret navn.`)] [no_norm_name_str])
	    in
	      UcsWidget.twoColumns(titles, info, false, [msg])
	    end
	| _ => UcsWidget.twoColumns(titles, info, false, [`^(exactMatchLink user_imp_id person_id)`])
    end

    fun deleteField () = ScsDict.s [(ScsLang.en,`Delete`),(ScsLang.da,`Slet`)]
    fun moreInfoField () = ScsDict.s [(ScsLang.en,`More Info`),(ScsLang.da,`Mere Info`)]
    fun delRelField () = ScsDict.s [(ScsLang.en,`Delete Relation`),(ScsLang.da,`Slet Relation`)]

    fun getUserImpIdErr(fv,errs) = 
      ScsFormVar.getNatErr(fv,ScsDict.s [(ScsLang.da,`Id på importrække`),
					 (ScsLang.en,`Id on import row`)],errs)

    fun getExtSourceErr(fv,errs) = 
      ScsFormVar.getEnumErr (List.map (fn s => #db_name(getSource s)) all_sources)
      (fv,ScsDict.s [(ScsLang.en,`External source`),
		     (ScsLang.da,`Ekstern kilde`)],errs)

    fun getOnWhichIdErr(fv,errs) = 
      ScsFormVar.getNatErr (fv,ScsDict.s [(ScsLang.en,`id on external source`),
					  (ScsLang.da,`id på ekstern kilde`)],errs)

    fun delLink user_imp_id =
      case user_imp_id of
	"" => "&nbsp;"
      | user_imp_id => 
	  Quot.toString
	  `<a  ^(UcsPage.confirmDelOnClick())  href="^(Html.genUrl "del_imp_row.sml" 
                                                         [("user_imp_id",user_imp_id)])">^(UcsPage.icon_remove())</a>`

    fun moreInfoLink user_imp_id =
      case user_imp_id of
	"" => "&nbsp;"
      | user_imp_id => Quot.toString
	  `<a href="^(Html.genUrl "more_info.sml" 
	  [("user_imp_id",user_imp_id)])">^(UcsPage.icon_info())</a>`

    val service_name = [(ScsLang.en, `Central Personnel Register`),
			(ScsLang.da, `Centralt Person Register`)]

    val service_adm_email = "nh@it-c.dk"

    (* ====================================================================== *)
    (* page generation                                                        *)
    (* ====================================================================== *)
    local
      val html_description = service_name
      val html_keywords = ""
      val leftSpaces = 2
      val rightSpaces = 2
      val url_home = "/index.sml"
      fun leftList() = [("/scs/admin/user/imp/imp_form.sml",
			 ScsDict.s [(ScsLang.en,`Central Personnel Register`),(ScsLang.da,`Matrikel`)]),
			("/scs/admin/user/imp/working_procedure.sml",
			 ScsDict.s [(ScsLang.en,`Manual`),(ScsLang.da,`Vejledning`)])]
      fun rightList() = []
      fun genPg title body = 
	UcsPage.genPg false title (ScsDict.s html_description) html_keywords service_adm_email
	              url_home leftSpaces rightSpaces (leftList()) (rightList()) UcsPage.emptyNavbar body
    in
      fun returnPg  title body = 
	Ns.return ( genPg title body )
    end
  end
