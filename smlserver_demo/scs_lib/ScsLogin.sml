signature SCS_LOGIN =
  sig
    val default_lang : ScsLang.lang
    val default_id : int

    val user_lang : ScsLang.lang
    val user_id   : int

    val verifyUser : unit -> int * ScsLang.lang
    val loggedIn   : bool

    val d : 'a * 'a -> 'a
    (* [d (str_dk,str_eng)] returns either str_dk or str_eng depending
    on the logged in user preference *)

    val auth_filter : (string * ScsRole.role list) list -> unit
  end

structure ScsLogin :> SCS_LOGIN =
  struct
    val default_lang : ScsLang.lang = ScsLang.da
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
	    (case Db.zeroOrOneRow `select password,scs_user.language_pref(user_id) as lang
                                     from scs_users
                                    where user_id = ^(Db.qqq user_id)
                                      and deleted_p = 'f'` of
               NONE => default
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

    (* Language selection *)
    fun d (str_dk,str_eng) =
      case user_lang of
	ScsLang.en => str_eng
      | ScsLang.da => str_dk

    (* ==================================================== *)
    (* Below, we check for password protected pages.        *)
    (* To each page we specify which roles that             *)
    (* may see the page.                                    *)
    (* protected_pages = [("page.sml",["role","role"]),...] *)
    (* ==================================================== *)
    fun auth_filter protected_pages =
      let
	val target = (*Ns.Conn.location()^*)Ns.Conn.url()
	fun reject msgs =
	  let
	    val query_data = case Ns.Conn.getQuery() of
	      NONE => ""
	    | SOME s => Quot.toString (Html.export_url_vars (("msg",Quot.toString (d msgs)) :: Ns.Set.list s))
	  in
	    (Ns.returnRedirect (Ns.Conn.location()^"/scs/auth/auth_form.sml?target=" ^ 
				Ns.encodeUrl (target^query_data)); 
	     Ns.exit())
	  end
        (* Reject the user to login if he accesses a protected page
           and does not hold one of the role-privileges. *)
	fun verifyUserFilter (p,roles) =
	  if (RegExp.match o RegExp.fromString) ((*Ns.Conn.location()^*)p) target then
	    (* Access to the protected page p: he must hold atleast one of the roles *)
	    if ScsRole.has_one_p user_id roles then
	      ()
	    else
	  reject (`Du har ikke de nødvendige adgangsprivilegier til at tilgå siden.<p>
                   Hvis du mener det er en fejl, så send en email til ` ^^ 
		   (Html.aemail "ScsConfig.scs_site_adm_email" "administrator") ^^ `.<p>`,
		  `You do not have access to the page you are requesting.<p>
                   Please send an email to the ` ^^ 
                   (Html.aemail "ScsConfig.scs_site_adm_email" "administrator") ^^ ` if you
                   believe you should have access to the page.<p>`)
	  else
	    (* Access is not to the protected page p *)
	    ()
      in
	(* we tell SMLserver to verify that the user is logged in and
           holds one of the roles in the roles list before serving any
           of the protected_pages *)
	if not loggedIn then
	  reject (`Du skal indtaste email og password for at få adgang til systemet.<p>
		   Hvis du allerede har indtastet en email og password, så må du prøve igen.<p>`,
		  `You must type in an email and password to get access to the web-site.<p>
                   If you have already typed in an email and password, then you must try again.<p>`)
	else
	  List.app verifyUserFilter protected_pages
      end
  end



