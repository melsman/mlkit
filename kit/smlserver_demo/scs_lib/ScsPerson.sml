signature SCS_PERSON =
  sig
    val name : int -> string
  end

structure ScsPerson :> SCS_PERSON =
  struct
    fun name user_id =
      Db.oneField `select scs_person.name(person_id)
                     from scs_persons
                    where scs_persons.person_id = '^(Int.toString ScsLogin.user_id)'`
  end