signature LOGIN =
  sig
    val default_id : int
    val person_id   : int

    val verifyPerson : unit -> int
    val loggedIn   : bool

    val auth_filter : string list -> unit
  end

structure Login : LOGIN =
  struct
    val default_id : int = 0

    (* auth_verify_user; return user_id if happy, 0 otherwise *)
    fun verifyPerson () =
      let
	val auth_person_id = Ns.Cookie.getCookieValue "auth_person_id"
	val auth_password = Ns.Cookie.getCookieValue "auth_password"
      in
	case (auth_person_id,auth_password) of
	  (SOME person_id, SOME psw) =>
	    (case Db.zeroOrOneRow `select password from person 
                                   where person_id = ^(Db.qq' person_id)` 
	       of NONE => default_id
	        | SOME [db_psw] => 
		 if db_psw = psw then 
		   (case Int.fromString person_id of
		      NONE => default_id
		    | SOME pid => pid)
		 else default_id
	     | SOME _ => raise Fail "Login.auth_verify_user")
	| _ => (Ns.log (Ns.Notice,"auth_verify_user (NONE): No cookies");default_id)
      end
    handle Ns.Cookie.CookieError _ => (Ns.log (Ns.Notice,"auth_verify_person: No cookies");default_id)
      
    (* We look for login-cookies on every request *)
    (* If you don't want that, then apply a filter similar to the one below. *)
    val person_id = verifyPerson()
    val loggedIn = person_id <> 0

    (* ============================================= *)
    (* Below, we check for password protected pages. *)
    (* ============================================= *)
    fun auth_filter protected_pages =
      let
	val target = Ns.Conn.location()^Ns.Conn.url()
	fun verifyPersonFilter () =
	  if loggedIn then ()
	  else (Ns.returnRedirect (Ns.Conn.location()^"/auth_form.sml?target=" ^ 
				   Ns.encodeUrl target); Ns.exit())
      in
	(* we tell SMLserver to verify that the user is logged in before
           serving any of the protected_pages *)
	if List.foldl (fn (p,acc) => acc orelse 
		       (RegExp.match o RegExp.fromString) (Ns.Conn.location()^p) target) 
	  false protected_pages then
	  verifyPersonFilter ()
	else ()
      end
  end



