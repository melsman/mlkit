signature SCS_LOGIN =
  sig
    val default_lang : ScsLang.lang
    val default_id : int

    val user_lang : ScsLang.lang
    val user_id   : int

    val verifyUser : unit -> int * ScsLang.lang
    val loggedIn   : bool

    val auth_filter : string list -> unit
  end

structure ScsLogin :> SCS_LOGIN =
  struct
    val default_lang : ScsLang.lang = ScsLang.Danish
    val default_id : int = 0
    val default = (default_id,default_lang)

    (* auth_verify_user; return user_id if happy, 0 otherwise *)
    fun verifyUser () =
      let
	val auth_user_id = Ns.Cookie.getCookieValue "auth_user_id"
	val auth_password = Ns.Cookie.getCookieValue "auth_password"
      in
	case (auth_user_id,auth_password) of
	  (SOME user_id, SOME psw) =>
	    (case Db.zeroOrOneRow `select password,lang from auth_user 
 	                              where user_id = ^(Db.qqq user_id)` 
	       of NONE => default
	     | SOME [db_psw,db_lang] => 
		 if db_psw = psw then 
		   (case Int.fromString user_id of
		      NONE => default
		    | SOME u_id => (u_id,ScsLang.fromString db_lang))
		 else default
	     | SOME _ => raise Fail "ScsLogin.auth_verify_user")
	| _ => (Ns.log (Ns.Notice,"auth_verify_user (NONE): No cookies");default)
      end
    handle Ns.Cookie.CookieError _ => (Ns.log (Ns.Notice,"auth_verify_user: No cookies");default)
	 | Ns.MissingConnection => (Ns.log (Ns.Notice,"auth_verify_user: Missing Connection");default)
      
    (* We look for login-cookies on every request *)
    (* If you don't want that, then apply a filter similar to the one below. *)
    val (user_id,user_lang) = verifyUser()
    val loggedIn = user_id <> 0

    (* ============================================= *)
    (* Below, we check for password protected pages. *)
    (* ============================================= *)
    fun auth_filter protected_pages =
      let
	val target = Ns.Conn.location()^Ns.Conn.url()
        val query_data = case Ns.Conn.getQuery() of
	  NONE => ""
	| SOME s => Quot.toString (Html.export_url_vars (Ns.Set.list s))
	fun verifyUserFilter () =
	  if loggedIn then ()
	  else (Ns.returnRedirect (Ns.Conn.location()^"/auth_form.sml?target=" ^ 
				   Ns.encodeUrl (target^query_data)); Ns.exit())
      in
	(* we tell SMLserver to verify that the user is logged in before
           serving any of the protected_pages *)
	if List.foldl (fn (p,acc) => acc orelse 
		       (RegExp.match o RegExp.fromString) (Ns.Conn.location()^p) target) 
	  false protected_pages then
	  verifyUserFilter ()
	else ()
      end
  end



