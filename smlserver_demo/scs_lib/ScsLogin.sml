signature SCS_LOGIN =
  sig
    val default_lang : ScsLang.lang
    val default_id : int

    val user_id   : unit -> int
    val user_lang : unit -> ScsLang.lang

    (* [set_user_lang lang] sets the user language to lang for this
       script only. Used to control language preference for users not
       logged in*)
    val set_user_lang : ScsLang.lang -> unit

    val verifyUser : unit -> int * ScsLang.lang
    val loggedIn   : unit -> bool

    val d : 'a * 'a -> 'a
    (* [d (str_dk,str_eng)] returns either str_dk or str_eng depending
    on the logged in user preference *)

    val auth        : unit -> int
    (* [auth] returns a login page if not logged in; 
       otherwise returns the user_id. *)

    val auth_roles  : ScsRole.role list -> int
    (* [auth_roles roles] returns a login page if not 
       logged or the logged in user does not have one of 
       the roles. On succes returns a user_id. *)

    val auth_filter : (string * ScsRole.role list) list -> unit
    (* [auth_filter urls_and_roless] returns a login page if accessing
       a page in the list and the logged in user does not have one of
       the roles affiliated with that page *)

    val auth_filter_accept_no_login : (string * ScsRole.role list) list -> unit
    (* [auth_filter_accept_no_login urls_and_roless] same as
       auth_filter except that it is ok, that the user is not logged
       at all if not accessing one of the pages in the urls list. *)

    val set_user_pw_cookie : int -> string -> string -> Ns.status
    (* [set_user_pw_cookie user_id passwd redirect] sets two new
       cookies with user_id and password. Redirects to url redirect.*)

    val getUserInfoCache : (string,(string*string)option) Ns.Cache.cache
    val getUserInfoFromDb : string -> (string * string) option
    (* [getUserInfoFromDb user_id] fetches encrypted password and
        language preference from DB. The functions is memoized. *)

    val calcMd5 : string -> string
    (* [calcMd5 s] returns the result of MD5 hashing s *)

  end

structure ScsLogin :> SCS_LOGIN =
  struct
    (* We might add a check on a form variable lang and 
       use that as default if exists. 2003-11-17, nh *)
    val default_lang : ScsLang.lang = ScsLang.da
    val default_id : int = 0
    val default = (default_id,default_lang)

    datatype cookie_info = COOKIE | NO_COOKIE

    fun calcMd5 s =
      Db.oneField `select fast_md5.md5_string('^(s)') from dual`

    val getUserInfoCache =
      Ns.Cache.get (Ns.Cache.String,
		    Ns.Cache.Option(Ns.Cache.Pair Ns.Cache.String Ns.Cache.String),
		    "ScsLogin.getUserInfor",
		    Ns.Cache.WhileUsed 3600)
    local
      fun getUserInfoFromDb' user_id = 
	case Db.zeroOrOneRow `select fast_md5.md5_string(password) as password,
	                             scs_user.language_pref(user_id) as lang
                                from scs_users
                               where user_id = ^(Db.qqq user_id)
                                 and password is not null
                                 and deleted_p = 'f'` of
	  NONE => NONE
	| SOME [db_psw,db_lang] => SOME (db_psw,db_lang)
    in
      val getUserInfoFromDb = Ns.Cache.memoize getUserInfoCache getUserInfoFromDb'
    end

    (* auth_verify_user; return user_id if happy, 0 otherwise *)
    local
      (* We memoize the cookie information for one connection
         only. The cookie information is checked exactly once for
         every connection. This also works with caching of library
         code because the function verifyUser is never executed by the
         library code - only on every request *)
      val user_info : ((cookie_info * (int * ScsLang.lang)) option) ref = ref NONE
      fun verifyUser'' () =
	let
	  val auth_user_id = Ns.Cookie.getCookieValue "auth_user_id"
	  val session_id = Ns.Cookie.getCookieValue "session_id"
	in
	  case (auth_user_id,session_id) of
	    (SOME user_id, SOME psw) =>
	      (case Int.fromString user_id of
		 (* matching on "deleted" is a IE hack. It seems that IE always sends the 
		  deleted cookie back to us! *)
		 NONE => if user_id = "deleted" then (NO_COOKIE,default) else (COOKIE,default)
	       | SOME _ => 
		   (case getUserInfoFromDb user_id of
		      NONE => (COOKIE,default)
		    | SOME (db_psw,db_lang) => 
			if db_psw = psw then 
			  (case Int.fromString user_id of
			     NONE => (COOKIE,default)
			   | SOME u_id => (COOKIE,(u_id,ScsLang.fromString db_lang)))
			else (COOKIE,default)))
	  | _ => (NO_COOKIE,default)
	end
      handle Ns.Cookie.CookieError _ => (NO_COOKIE,default)
	   | Ns.MissingConnection => raise Fail ("ScsLogin.verifyUser'': You probably call this " ^ 
						 "function during library initialization")
    in
      fun verifyUser' () =
	case !user_info of
	  NONE => (user_info := SOME (verifyUser''());
		   Option.valOf (!user_info))
	| SOME i => i
      fun upd_user_lang lang = 
	case !user_info of
	  NONE => user_info := SOME((NO_COOKIE,(default_id,lang)))
	| SOME(c,(id,_)) => user_info := SOME(c,(id,lang))
    end

    fun verifyUser () = #2(verifyUser'())
    fun set_user_lang lang =
      let
	(* Make sure that cookie information, if exists, has been read *)
	val _ = verifyUser()
      in
	upd_user_lang lang
      end

    (* We look for login-cookies on every request *)
    (* If you don't want that, then apply a filter similar to the one below. *)
    fun user_id_user_lang () = verifyUser()
    fun user_id () = #1 (verifyUser())
    (* If no connection exists, then use default lang *)
    fun user_lang () = #2 (verifyUser()) handle _ => default_lang
    fun loggedIn () = user_id () <> 0

    (* Language selection *)
    fun d (str_dk,str_eng) =
      case user_lang() of
	ScsLang.en => str_eng
      | ScsLang.da => str_dk

    local
      fun reject msg =
	let
  	  val target = Ns.Conn.url()

	  val target_url = Html.genUrl "/scs/auth/auth_form.sml"
            [("target", Html.genUrl target
   	       (case Ns.Conn.getQuery() of
	          NONE => []
	        | SOME s => Ns.Set.list s)),
             ("msg",Quot.toString msg)]
	in
	  (Ns.write 
`HTTP/1.0 302 Found
Location: ^target_url
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="session_id",path=SOME "/"})
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/"})

You should not be seeing this!`;
(*Ns.returnRedirect target_url; 2003-03-10, nh*)
	   Ns.exit())
	end
    in
      fun auth () =
      let
	val (cookie,(user_id,user_lang)) = verifyUser'()
	val msg = 
	  case cookie of
	    NO_COOKIE => ``
	    | COOKIE => `<b>Fejl ved login:</b> Er du ny bruger eller har du glemt password 
 	                 så klik <a href="mail_passwd_form.sml">Glemt password?</a>.<br><br>
                         <b>Error on login:</b> If you are a new user or you forgot password then 
		         click <a href="mail_passwd_form.sml">Forgot password?</a>.`
      in	
	if user_id = 0 then
          (reject (msg);
	   default_id)
        else
          user_id
      end

      fun auth_roles roles =
      let
	val user_id = auth()
      in	
	if ScsRole.has_one_p user_id roles orelse roles = [] then
          user_id
        else
	  (reject (`Du har ikke de nødvendige adgangsprivilegier til at tilgå siden. ` ^^
                   `Hvis du mener det er en fejl, så send en email til ` ^^ 
		    (Html.aemail (ScsConfig.scs_site_adm_email()) "administrator") ^^ `.<br>` ^^
		   `(eng. You do not have access to the page you are requesting. ` ^^
                   `Please send an email to the ` ^^ 
                    (Html.aemail (ScsConfig.scs_site_adm_email()) "administrator") ^^ ` if you
                    believe you should have access to the page.)<p>`);
           default_id)
      end

    (* ==================================================== *)
    (* Below, we check for password protected pages.        *)
    (* To each page we specify which roles that             *)
    (* may see the page.                                    *)
    (* protected_pages = [("page.sml",["role","role"]),...] *)
    (* If the list of roles is empty, then auth_filter      *)
    (* only checks, that the user is logged in depending    *)
    (* on the flag force_login.                             *)
    (* ==================================================== *)
    fun auth_filter' force_login protected_pages =
      let
        val target = Ns.Conn.url()
        (* Reject the user to login if he accesses a protected page
           and does not hold one of the role-privileges. *)
	fun verifyUserFilter (p,roles) =
	  if (RegExp.match o RegExp.fromString) p target then
	    (* Access to the protected page p: he must hold atleast one of the roles *)
            (auth_roles roles;())
	  else
	    (* Access is not to the protected page p *)
	    ()
      in
	(* we tell SMLserver to verify that the user is logged in and
           holds one of the roles in the roles list before serving any
           of the protected_pages *)
	if force_login andalso not (loggedIn()) then
	  reject (`<b>Fejl ved login:</b> Er du ny bruger eller har du glemt password 
		   så klik <a href="mail_passwd_form.sml">Glemt password?</a>.<br><br>
                   <b>Error on login:</b> If you are a new user or you forgot password then 
		   click <a href="mail_passwd_form.sml">Forgot password?</a>.`)
	else
	  List.app verifyUserFilter protected_pages
      end

    val auth_filter = auth_filter' true
    val auth_filter_accept_no_login = auth_filter' false
    end

    fun set_user_pw_cookie user_id passwd redirect =
Ns.write
`HTTP/1.0 302 Found
Location: ^redirect
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_user_id", value=Int.toString user_id,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})
^(Ns.Cookie.deleteCookie{name="session_id",path=SOME "/"})
^(Ns.Cookie.setCookie{name="session_id", value=calcMd5 passwd,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})


You should not be seeing this!`
  end



