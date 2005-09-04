signature AUTH =
  sig
    type person_id = int

    val loginPage    : string
    val defaultHome  : string
    val siteName     : string

    val verifyPerson : unit -> person_id option
    val isLoggedIn   : unit -> bool

    val newPassword  : int -> string
    val sendPassword : person_id -> unit
  (*
   [person_id] type of value that uniquely identifies a user.

   [loginPage] absolute path to login page.

   [defaultHome] absolute path to the page.
   
   [siteName] name of the web-site.

   [verifyPerson()] returns SOME(p) if the user with 
   person_id p is logged in; otherwise NONE is returned.
   
   [isLoggedIn()] returns true if the user is logged in; 
   returns false otherwise.
   
   [newPassword n] generates a new password constructed from n
   characters chosen randomly from the set {a-zA-Z2-9} \ {loO}.

   [sendPassword p] sends an email to the user with person_id p
   containing the user's password.
   *)
  end

structure Auth : AUTH =
  struct
    (* Configuration *)
    val emailFrom   = "anonymous@it.edu"
    val defaultHome = "/web/link/index.sml"
    val siteName    = "SMLserver.org"
    val loginPage   = "/web/auth_form.sml"

    type person_id = int

    (* verifyPerson; return SOME(p) if user with person_id p 
     * is logged in; returns NONE otherwise. *)
    fun verifyPerson0 (getPasswd: string -> string option) 
      : person_id option =
      (case (Web.Cookie.getCookieValue "auth_person_id",
	     Web.Cookie.getCookieValue "auth_password") 
	 of (SOME person_id, SOME psw) =>
	   (case getPasswd person_id
	      of NONE => NONE
	       | SOME db_psw => 
		if db_psw = psw then Int.fromString person_id
		else NONE)
	  | _ => NONE) 
	 handle Web.Cookie.CookieError _ => NONE
(*
    fun verifyPerson() = 
      verifyPerson0 (fn p => Db.zeroOrOneField 
		     `select password from person 
 	  	      where person_id = ^p`)
*)
    fun verifyPerson() = 
      let fun f p = 
            Db.zeroOrOneField 
	           `select password from person 
	            where person_id = ^p`
	  val cache = Web.Cache.get (Web.Cache.String, Web.Cache.String,"auth", 
                   Web.Cache.WhileUsed (SOME (Time.fromSeconds 600), SOME 10000))
	  val g = Web.Cache.memoizePartial cache f
      in verifyPerson0 g
      end

    fun isLoggedIn() : bool = case verifyPerson()
				of SOME _ => true
				 | NONE => false

    val okchs = "abcdefghijkmnpqrstuvwxyzABCDEFGHIJKLMNPQRSTUVWXYZ23456789"
    fun newPassword n : string =
      let val gen = Random.newgen()
	  val range = size okchs
	  fun ch() = CharVector.sub(okchs, Random.range(0,range) gen)
	  fun loop (0,acc) = implode(rev acc)
	    | loop (n,acc) = loop (n-1, ch()::acc)
      in loop (n, nil)
      end

    fun sendPassword (pid : person_id) : unit =
      let val query = `select email, name, password
                       from person 
                       where person_id = ^(Int.toString pid)`
	  val (email, name, passwd) =
	    case Db.zeroOrOneRow query
	      of NONE => raise Fail "sendPassword: no such user"
	       | SOME [e, n, p] => (e, n, p)
	       | _ => raise Fail "sendPassword: database error"
	  val subject = "welcome to " ^ siteName
	  val loginPage = Web.Conn.location() ^ loginPage
	  val body = Quot.toString
`Dear ^name,

To login to ^siteName, visit the page 

   ^loginPage

and provide your email address and password:

   email    : ^email
   password : ^passwd

Best Regards,

The ^siteName administrator`

      in Web.Mail.send {to=email, from=emailFrom,
		       subject=subject,body=body}
      end
  end



