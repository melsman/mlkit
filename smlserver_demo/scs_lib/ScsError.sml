(* Error handling, i.e. sending emails to systemadministrator when
errors happens. Should be extended with database and only a few emails
sent *)

signature SCS_ERROR =
  sig
    val raiseError : quot -> 'a
    val logError   : quot -> unit
    val emailError : quot -> unit
    val panic      : quot -> 'a
    val panic'	   : quot -> quot -> 'a
    val valOf      : 'a option -> 'a
    val valOf'     : string -> 'a option -> 'a

    val errorMsg   : quot -> 'a

    (* [logFormVars ()] logs all formvars *)
    val logFormVars : unit -> quot

    (* [valOfMsg msg v_opt] returns the msg to the user and no error
        is logged if v_opt is NONE; otherwise returns the value v
        where SOME v = v_opt *)
    val valOfMsg   : quot -> 'a option -> 'a

    (* [wrapPanic f a] applies f a and returns the result. If an
        exception is raised then the web-service fails with a system
        error page. *)
    val wrapPanic : ('a -> 'b) -> 'a -> 'b

    (* [wrapPanic' msg f a] applies f a and returns the result. If an
        exception is raised then the web-service fails with a system
        error page including the message msg. *)
    val wrapPanic' : quot -> ('a -> 'b) -> 'a -> 'b

    (* [wrapPanic'' logmsg f a] has the same semantics as wrapPanic, except
	that logmsg is added to the log for better debugging *)
    val wrapPanic'' : quot -> ('a -> 'b) -> 'a -> 'b

    (* [wrapOpt f a] applies f a and returns SOME (result). If an
        exception is raised then NONE is returned. No error is logged
        or mailed. This is not a panic error. *)
    val wrapOpt   : ('a -> 'b) -> 'a -> 'b option

    (* [wrapMsg msg f a] similar to wrapPanic except that msg is
       shown to the user and no error is logged or emailed. This is 
       not a panic error.*)
    val wrapMsg   : quot -> ('a -> 'b) -> 'a -> 'b

    (* [log msg] writes msg to the serverlog *)
    val log : string -> unit

    val err_msg_db : ScsDict.dict -> string -> ScsDict.dict
  end

structure ScsError :> SCS_ERROR =
  struct 
    fun panic' a b = raise Fail "not implemented"
    fun logError emsg = Ns.log (Ns.Error, Quot.toString emsg)

    fun raiseError emsg = ( logError emsg; raise (Fail (Quot.toString emsg)) )

    fun emailError emsg = Ns.Mail.send {
      to=ScsConfig.scs_site_adm_email(),
      from=ScsConfig.scs_site_adm_email(),
      subject="ScsPanic",body=Quot.toString emsg
    }

    fun logFormVars () = 
      let
	val fvs  = map (fn (n,v)=> `
	  name=^n, value=^v`) (
	    case Ns.Conn.getQuery() of 
	      NONE => []
	    | SOME s => Ns.Set.list s
 	  )
      in
	logError (foldl op^^ `` fvs); 
	(foldl op^^ `` fvs)
      end


    fun panic' msg emsg = 
      let
	val emsg = `script: ^(Ns.Conn.location())^(Ns.Conn.url()).
user: ^(ScsPersonData.name(ScsLogin.user_id()))
email: ^(ScsPersonData.email(ScsLogin.user_id()))
	  ` ^^ emsg
	val title = case ScsLogin.user_lang() of
	    ScsLang.en => "System Error"
	  | ScsLang.da => "Systemfejl"
      in
	(logError emsg;
	 logFormVars ();
	 emailError (emsg ^^ (logFormVars ()));
	 ScsPage.returnPg title msg;
	 Ns.exit())
      end

    fun panic emsg = 
      let
        val msg = 
	  case ScsLogin.user_lang() of
	    ScsLang.en => `
		 It seems that the system can't complete your request.<p>
		 This is probably our fault. The system administrator has been
		 notified.<p>
		 Please try again later.`
	 | ScsLang.da => `
	     Vi kan desværre ikke fuldføre din forespørgsel.<p>
	     Dette er sandsynligvis vores fejl. Vores systemadministrator 
	     er blevet informeret om problemet.<p>
	     Du må meget gerne prøve igen senere.`
      in
        panic' msg emsg
      end

    fun errorMsg msg = ( ScsPage.returnPg "" msg ; Ns.exit() )

    fun valOf NONE     = panic `valOf(NONE)`
      | valOf (SOME v) = v

    fun valOf' str NONE     = panic `valOf'(NONE): ^str`
      | valOf' str (SOME v) = v

    fun valOfMsg msg NONE     = errorMsg msg
      | valOfMsg msg (SOME v) = v

    fun wrapPanic' msg f a = f a 
      handle 
          Fail s => panic' msg (`Fail raised: ^s`)
	| X      => panic' msg `wrapPanic: some error happended: ^(General.exnMessage X)`

    fun wrapPanic f a = f a 
      handle 
          Fail s => panic (`Fail raised: ^s`)
	| X      => panic `wrapPanic: some error happended: ^(General.exnMessage X)`

    fun wrapPanic'' logmsg f a = f a 
      handle 
          Fail s => panic (`Fail raised: ^s ` ^^ logmsg)
	| X      => panic (`wrapPanic: some error happended: ^(General.exnMessage X) ` ^^ logmsg)



    fun wrapOpt f a = SOME(f a)
      handle _ => NONE

    fun wrapMsg msg f a = f a
      handle _ => errorMsg msg

    fun log msg = Ns.log (Ns.Notice, msg)


    fun err_msg_db X_dict help_link = [
      (ScsLang.da, `^(ScsDict.getString X_dict ScsLang.da) du s&oslash;ger 
                    findes desv&aelig;rre ikke i databasen. Hvis du mener 
		    dette er en fejl, s&aring; henvend dig venligst hos 
		    ^help_link.`),
      (ScsLang.en, `^(ScsDict.getString X_dict ScsLang.en) you seek does 
		    not exist in the database. If you believe this is an 
		    error then please contact ^help_link.`)
    ]

  end
