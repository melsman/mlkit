signature SCS_PERSON =
  sig
    val name : int -> string
    val search_form: string -> (string*string) list -> quot

    (* Check for form variables *)
    val getPersonIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs
  end

structure ScsPerson :> SCS_PERSON =
  struct
    val d = ScsDict.d ScsLang.en "ucs_lib" "ScsPerson"
    val d' = ScsDict.d' ScsLang.en "ucs_lib" "ScsPerson"

    fun name user_id =
      Db.oneField `select scs_person.name(person_id)
                     from scs_persons
                    where scs_persons.person_id = '^(Int.toString user_id)'`

    fun search_form target_url hvs =
      ScsWidget.formBox "/scs/person/person_search.sml" [("submit",d"Search")]
      (Html.export_hiddens (("target_url",target_url)::hvs) ^^ 
        (d' `Search after all persons that matches the pattern you type in below. 
             Several fields related to a person are searched 
             including name, security number and email.<p>`) ^^
       (ScsWidget.tableWithTwoCols[(d'`Search pattern:`,ScsWidget.intext 40 "pat")]))

   (* Check for form variables *)
    fun getPersonIdErr (fv,errs) = ScsFormVar.getIntErr(fv,d"Person id",errs)
  end
