signature SMLS_LOGIN =
  sig
    val default_lang : SmlsLang.lang
    val default_id : int

    val user_lang : SmlsLang.lang
    val user_id   : int

    val verifyUser : unit -> int * SmlsLang.lang
    val loggedIn   : unit -> bool
  end

structure SmlsLogin :> SMLS_LOGIN =
  struct
    val default_lang : SmlsLang.lang = SmlsLang.English
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
 	                              where user_id = ^(Db.qq' user_id)` 
	       of NONE => default
	     | SOME [db_psw,db_lang] => 
		 if db_psw = psw then 
		   (case Int.fromString user_id of
		      NONE => default
		    | SOME u_id => (u_id,SmlsLang.fromString db_lang))
		 else default
	     | SOME _ => raise Fail "SmlsLogin.auth_verify_user")
	| _ => (Ns.log (Ns.Notice,"auth_verify_user (NONE): No cookies");default)
      end
    handle Ns.Cookie.CookieError _ => (Ns.log (Ns.Notice,"auth_verify_user: No cookies");default)
      
    (* We look for login-cookies on every request *)
    (* If you don't want that, then apply a filter similar to the one below. *)
    val (user_id,user_lang) = verifyUser()
    fun loggedIn () = user_id <> 0

    (* ============================================= *)
    (* Below, we check for password protected pages. *)
    (* ============================================= *)
    val login_pages = ["/show_cookies.sml*", "/email*"]

    val _ =
      let
	val target = Ns.Conn.location()^Ns.Conn.url()
	fun verifyUserFilter () =
	  if loggedIn() then ()
	  else (Ns.returnRedirect (Ns.Conn.location()^"/auth_form.sml?target=" ^ 
				   Ns.encodeUrl target); Ns.exit())
      in
	(* we tell SMLserver to verify that the user is logged in before
           serving any of the login_pages *)
	if List.foldl (fn (p,acc) => RegExp.regExpBool (Ns.Conn.location()^p) target orelse acc) false login_pages then
	  verifyUserFilter ()
	else ()
      end
  end



