signature SCS_PERSON =
  sig
    datatype sex = Female | Male

    type person_record = {
      person_id         : int,
      first_names 	: string,
      last_name		: string,
      name 		: string,
      email		: string,
      url		: string,
      cpr               : string
    }

    type profile_record = 
      { party_id       : int,
        profile        : string,
	keywords       : string,
	edit_no        : int,
	last_modified  : Date.date,
	modifying_user : int,
	deleted_p      : bool}

    (* [getPerson user_id] fetches a person from the database *)
    val getPerson : int -> person_record option

    (* [getPersonByExtSource on_what_table on_which_id] fetches a
       person from the database that relates to the external source
       represented by on_what_table and on_which_id. *)
    val getPersonByExtSource : string -> int -> person_record option

    (* [getPersonErr (id,errs)] returns a person record if exists;
       otherwise an error is appended to the error list errs. *)
    val getPersonErr : int * ScsFormVar.errs -> person_record option * ScsFormVar.errs 

    (* [getProfile user_id] fetches a profile from the database. It
       only returns NONE if user_id does not exists in scs_parties. An
       empty profile is created for user_id if no one exists *)
    val getProfile : int -> profile_record option

    (* [getProfileErr (id,errs)] returns a profile record if exists;
       otherwise an error is appended to the error list errs. *)
    val getProfileErr : int * ScsFormVar.errs -> profile_record option * ScsFormVar.errs 

   (* [searchPerson pat keep_del_p] returns a list of persons matching
      the pattern pat. If deleted_p is true then we also search in
      deleted persons *)
    val searchPerson : string -> bool -> person_record list

    (* [name user_id] returns the name found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val name : int -> string

    (* [email user_id] returns the email found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val email : int -> string

    (* [nameToHtml name email] returns HTML for a name linking to
        email *)
    val nameToHtml : string * string -> string

    (* [search_form target_url hvs] generates a standard HTML search
       form. The user enter a search expression (e.g., name, security
       id) and either

         * the search expression does not identity any person and an
           info message is shown to the user.

         * the search expression identify exactly one person, and a
           redirect to the page represented by target_url is issued
           with hidden variables hvs and user_id = x where x is
           user_id for the person found.

         * the search expression identity more than one person in
           which case the user is presented a page on which she slects
           the person she seeks. *)
    val search_form: string -> (string*string) list -> quot

    (* [getPersonIdErr fv errs] checks that fv is an integer which can
       be used as a person_id. The database is not checked. See also
       ScsFormVar.sml *)
    val getPersonIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs

    (* [splitCpr cpr] if cpr is on the form xxxxxxyyyy then the pair
       (xxxxxx,yyyy) is returned. *)
    val splitCpr      : string -> (string*string)

    (* [ppCpr cpr]  if cpr is on the form xxxxxxyyyy then the string
       xxxxxx-yyyy is returned; otherwise the argument is returned. *)
    val ppCpr : string -> string

    (* [makeCprPublic cpr] if cpr in on the form aaaaaabbbb then the
       revised cpr aaaaaaXXXX is returned *)
    val makeCprPublic : string -> string

    (* [cprToDate cpr] if cpr is a valid cpr then a date is returned
       representing the birth date. If cpr is not valid then an
       exception is raised. *)
    val cprToDate     : string -> Date.date

    (* [cprToSex cpr] if cpr is a valid cpr then the sex (either Male
       of Female) is returned. Otherwise an exception is returned. *)
    val cprToSex : string -> sex

    (* [isFemale_p cpr] returns true if the cpr is female. Throws an 
	exception if cpr is invalid *)
    val isFemale_p : string -> bool

    (* [fix_email email] do the following conversions:
         - if email is of form login@it-c.dk => login@itu.dk
         - if email is of form login@it.edu => login@itu.dk
         - if email is of form login => login@itu.dk
     *)
    val fix_email : string -> string
  end

structure ScsPerson :> SCS_PERSON =
  struct
    datatype sex = Female | Male

    type person_record = {
      person_id         : int,
      first_names 	: string,
      last_name		: string,
      name 		: string,
      email		: string,
      url		: string,
      cpr		: string
    }

    type profile_record = 
      { party_id       : int,
        profile        : string,
	keywords       : string,
	edit_no        : int,
	last_modified  : Date.date,
	modifying_user : int,
	deleted_p      : bool}

    local
      fun f g = {person_id = (ScsError.valOf o Int.fromString) (g "person_id"),
		 first_names = g "first_names",
		 last_name = g "last_name",
		 name = g "name",
		 email = g "email",
		 url = g "url",
		 cpr = g "cpr"}
      fun personSQL from_wh =
	` select p.person_id, p.first_names, p.last_name, 
		 scs_person.name(p.person_id) as name, 
		 scs_party.email(p.person_id) as email,
		 scs_party.url(p.person_id) as url,
		 p.security_id as cpr
            ` ^^ from_wh
    in
      fun getPerson user_id = 
	SOME( Db.oneRow' f (personSQL ` from scs_persons p
                                       where person_id = '^(Int.toString user_id)'`))
	handle _ => NONE
      fun getPersonErr (user_id,errs) =
	case getPerson user_id of
	  NONE => 
	    let
	      val err_msg = [(ScsLang.da,`Personen du søger findes ikke.`),
			     (ScsLang.en,`The person you are seeking does not exits.`)]
	    in
	      (NONE,ScsFormVar.addErr(ScsDict.s' err_msg,errs))
	    end
	| p => (p,errs)
      fun getPersonByExtSource on_what_table on_which_id =
	SOME( Db.oneRow' f (personSQL ` from scs_persons p, scs_person_rels r
                                       where r.on_what_table = ^(Db.qqq on_what_table)
                                         and r.on_which_id = '^(Int.toString on_which_id)'
                                         and r.person_id = p.person_id`))
	handle _ => NONE
      fun searchPerson pat keep_del_p =
	Db.list f (personSQL 
		   ` from scs_persons p
                    where (lower(scs_person.name(p.person_id)) like ^(Db.qqq pat)
                       or lower(scs_party.email(p.person_id)) like ^(Db.qqq pat)
		       or p.security_id like ^(Db.qqq pat))
                      and p.deleted_p in (^(if keep_del_p then "'t','f'" else "'f'"))`)
	handle _ => []
    end

    local
      fun f g = {party_id = (ScsError.valOf o Int.fromString) (g "party_id"),
		 profile = g "profile",
		 keywords = g "keywords",
		 edit_no = (ScsError.valOf o Int.fromString) (g "edit_no"),
		 last_modified = (ScsError.valOf o Db.toDate) (g "last_modified"),
		 modifying_user = (ScsError.valOf o Int.fromString) (g "modifying_user"),
		 deleted_p = (ScsError.valOf o Db.toBool) (g "deleted_p")}
      fun profileSQL from_wh =
	` select p.party_id, 
                 p.profile, 
                 p.keywords, 
                 p.edit_no, 
                 p.last_modified, 
                 p.modifying_user, 
                 p.deleted_p
            ` ^^ from_wh
    in
      fun getProfile user_id =
	SOME(Db.oneRow' f (profileSQL ` from scs_profiles p
			               where p.party_id = '^(Int.toString user_id)'`))
	handle _ => 
	  (* Profile does not exits - so try to insert empty profile *)
	  let
	    val per_opt = getPerson user_id 
	  in
	    case per_opt of
	      NONE => NONE (* User does not exists *)
	    | SOME per => (* User exists so insert empty profile *)
		let
		  (* We set the creating user to be the not logged in user - pretty much arbitrarily *)
                  (* The current user may not be logged in. (e.g., if he comes from Find Person)     *)
		  val empty_profile =
		    {party_id = user_id,
		     profile = "",
		     keywords = "",
		     edit_no = 0,
		     last_modified = ScsDate.now_local(),
		     modifying_user = 0,
		     deleted_p = false}
		  val ins_sql = `insert into scs_profiles 
                                   (party_id,profile,keywords,edit_no,last_modified,modifying_user)
                                 values
                                   ('^(Int.toString user_id)',null,null,0,sysdate,
				    '^(Int.toString ScsLogin.default_id)')` 
		in
		  (Db.dml ins_sql;
		   SOME empty_profile)
		  handle _ => (Ns.log(Ns.Warning, "Could not create profile for user_id " ^ 
				      Int.toString user_id);
			       NONE)
		end
	  end
      fun getProfileErr (user_id,errs) =
	case getProfile user_id of
	  NONE => 
	    let
	      val err_msg = [(ScsLang.da,`Personen du søger findes ikke.`),
			     (ScsLang.en,`The person you are seeking does not exits.`)]
	    in
	      (NONE,ScsFormVar.addErr(ScsDict.s' err_msg,errs))
	    end
	| p => (p,errs)

    end
    fun name user_id =
      Db.oneField `select scs_person.name(person_id)
                     from scs_persons
                    where scs_persons.person_id = '^(Int.toString user_id)'`
      handle Fail _ => ""

    fun email user_id =
      Db.oneField `select scs_party.email(^(Int.toString user_id))
                     from dual`
      handle Fail _ => ""

    fun nameToHtml (name,email) = Quot.toString
      `<a href="mailto:^(email)">^(name)</a>`

    fun search_form target_url hvs =
      ScsWidget.formBox "/scs/person/person_search.sml" 
        [("submit",ScsDict.s [(ScsLang.en,`Search`),(ScsLang.da,`Søg`)])]
        (Html.export_hiddens (("target_url",target_url)::hvs) ^^ 
          (ScsDict.s' [(ScsLang.en,`Search after all persons that matches the pattern you type in below. 
			            Several fields related to a person are searched 
                                    including name, security number and email.`),
		       (ScsLang.da,`Søg efter alle personer som matcher det mønster du indtaster nedenfor.
                                    Der søges i flere felter, bl.a. navn, cpr nummer og email.`)]) ^^ `<p>` ^^
       (ScsWidget.tableWithTwoCols[(ScsDict.s' [(ScsLang.en,`Search pattern:`),
						(ScsLang.da,`Søgemønster`)],ScsWidget.intext 40 "pat")]))

   (* Check for form variables *)
    fun getPersonIdErr (fv,errs) = ScsFormVar.getIntErr(fv,"Person id",errs)

    fun splitCpr cpr = (String.substring (cpr,0,6),String.substring (cpr,6,4))

    fun ppCpr cpr =
      let 
	val (x,y) = splitCpr cpr
      in
	x ^ "-" ^ y
      end
    handle _ => cpr

    fun makeCprPublic cpr =
      let
	val (cpr1,_) = splitCpr cpr
      in
	cpr1 ^ "-xxxx"
      end

    fun cprToDate cpr =
      let
	val (cpr1,_) = splitCpr cpr
	val day = Option.valOf(Int.fromString(String.substring(cpr1,0,2)))
	val mth = Option.valOf(Int.fromString(String.substring(cpr1,2,2)))
	val year = Option.valOf(Int.fromString(String.substring(cpr1,4,2)))
	val year = if year < 20 then 2000 + year else 1900 + year
      in
	ScsDate.genDate(day,mth,year)
      end

    fun cprToSex cpr =
      case String.substring(cpr,9,1) of
	"1" => Male
      | "3" => Male
      | "5" => Male
      | "7" => Male
      | "9" => Male
      | _ => Female

    (* do the following conversions:
         - if email is of form login@itu.dk => login@it-c.dk
         - if email is of form login@it.edu => login@it-c.dk
         - if email is of form login => login@it-c.dk
     *)
    fun fix_email email =
      let
	val email = ScsString.lower email
	val regExpExtract = RegExp.extract o RegExp.fromString
      in
	case regExpExtract "([a-z][a-z0-9\\-]*)@(it-c.dk|it.edu)" email of
	  SOME [l,e] => l ^ "@itu.dk"
	| _ => 
	    (case regExpExtract "([a-z][a-z0-9\\-]*)" email of
	       SOME [l] => l ^ "@itu.dk"
	     | _ => email)
      end
    (* Test code for fix_email
       fun try s =
         print (s ^ " = " ^ (fix_email s) ^ "\n")
       val _ =
         (try "nh";
          try "hanne@ruc.dk";
          try "nh@it-c.dk";
          try "nh@itu.dk";
          try "nh@it.edu";
          try "nh@diku.dk")
       handle Fail s => print s*)

    fun isFemale_p cpr = 
      cprToSex cpr = Female

  end
