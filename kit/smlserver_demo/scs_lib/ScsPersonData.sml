(* $Id$ *)

signature SCS_PERSON_DATA =
  sig
    (* [name user_id] returns the name found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val name : int -> string

    (* [email user_id] returns the email found in the database for user
       identified by user_id. Returns "" if no email exists. *)
    val email : int -> string

    (* [fix_email email] do the following conversions:
         - if email is of form login@it-c.dk => login@itu.dk
         - if email is of form login@it.edu => login@itu.dk
         - if email is of form login => login@itu.dk
     *)
    val fix_email : string -> string

    val getEmailCache : (int,string) Ns.Cache.cache
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
	select scs_person.name(^(Int.toString user_id))
	  from dual `
	handle Fail _ => ""
    in
      fun name user_id = Ns.Cache.memoize name_cache name' user_id
    end

    val getEmailCache = Ns.Cache.get(
      Ns.Cache.Int,
      Ns.Cache.String,
      "ScsPersonEmail",
      Ns.Cache.TimeOut 900 (*15 min*)
    )

    local
      (* Emails only change in external sources once a day. Hence, it
         shouldn't be a big problem not updating the cache. *)
      val email_cache = getEmailCache	

      fun email' user_id = 
	Db.oneField `
	  select scs_party.email(^(Int.toString user_id))
	    from dual `
        handle Fail _ => ""
    in
      fun email user_id = Ns.Cache.memoize email_cache email' user_id
    end

    (* do the following conversions:
         - if email is of form login@it-c.dk => login@itu.dk
         - if email is of form login@it.edu => login@itu.dk
         - if email is of form login => login@itu.dk
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

  end
