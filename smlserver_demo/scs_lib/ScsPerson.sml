signature SCS_PERSON =
  sig
    val name : int -> string
    val search_form: string -> (string*string) list -> quot

    (* Check for form variables *)
    val getPersonIdErr : string * ScsFormVar.errs -> int * ScsFormVar.errs
  end

structure ScsPerson :> SCS_PERSON =
  struct
    fun name user_id =
      Db.oneField `select scs_person.name(person_id)
                     from scs_persons
                    where scs_persons.person_id = '^(Int.toString user_id)'`
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
  end
