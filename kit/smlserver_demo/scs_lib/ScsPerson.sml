signature SCS_PERSON =
  sig
    datatype sex = Female | Male

    (* [name user_id] returns the name found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val name : int -> string

    (* [email user_id] returns the email found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val email : int -> string

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
  end

structure ScsPerson :> SCS_PERSON =
  struct
    datatype sex = Female | Male

    fun name user_id =
      Db.oneField `select scs_person.name(person_id)
                     from scs_persons
                    where scs_persons.person_id = '^(Int.toString user_id)'`
      handle Fail _ => ""

    fun email user_id =
      Db.oneField `select scs_party.email(^(Int.toString user_id))
                     from dual`
      handle Fail _ => ""

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
  end
