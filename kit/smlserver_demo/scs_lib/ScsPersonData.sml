(* $Id$ *)

signature SCS_PERSON_DATA =
  sig
    (* [name user_id] returns the name found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val name : int -> string

    (* [email user_id] returns the email found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val email : int -> string

  end


structure ScsPersonData :> SCS_PERSON_DATA =
  struct

    local
      (* Names only change in external sources once a day. Hence, it
         shouldn't be a big problem not updating the cache. *)
      val name_cache = 	
	Ns.Cache.get(Ns.Cache.Int,
		     Ns.Cache.String,
		     "ScsPersonName",
		     Ns.Cache.TimeOut 900 (*15 min*))
      fun name' user_id = Db.oneField `
	select scs_person.name(person_id)
	  from scs_persons
	 where scs_persons.person_id = '^(Int.toString user_id)'`
	handle Fail _ => ""
    in
      fun name user_id = Ns.Cache.memoize name_cache name' user_id
    end

    fun email user_id =
      Db.oneField `select scs_party.email(^(Int.toString user_id))
                     from dual`
      handle Fail _ => ""

  end
