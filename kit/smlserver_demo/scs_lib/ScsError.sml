(* Error handling, i.e. sending emails to systemadministrator when
errors happens. Should be extended with database and only a few emails
sent *)

signature SCS_ERROR =
  sig
    val raiseError : quot -> 'a
    val logError   : quot -> unit
    val emailError : quot -> unit
    val panic      : quot -> 'a
    val valOf      : 'a option -> 'a

    val wrapPanic     : ('a -> 'b) -> 'a -> 'b
  end

structure ScsError :> SCS_ERROR =
  struct 
    fun logError emsg = Ns.log (Ns.Error, Quot.toString emsg)

    fun raiseError emsg = (logError emsg; raise (Fail (Quot.toString emsg)))

    fun emailError emsg = Ns.Mail.send {to="nh@it-c.dk",from="ucs@it-c.dk",subject="ScsPanic",body=Quot.toString emsg}

    fun panic emsg = 
      let
	val emsg = `script: ^(Ns.Conn.location())^(Ns.Conn.url()).
	  ` ^^ emsg
      in
	(logError emsg;
	 emailError emsg;
	 case ScsLogin.user_lang of
	   ScsLang.en =>
	     ScsPage.returnPg "System Error" `
	     It seems that the system can't complete your request.<p>
	     This is probably our fault. The system administrator has been
	     notified.<p>
	     Please try again later`
	 | ScsLang.da => ScsPage.returnPg "Systemfejl" `
	     Vi kan desværre ikke fuldføre din forespørgsel.<p>
	     Dette er sandsynligvis vores fejl. Vores systemadministrater 
	     er blevet informeret om problemt.<p>
	     Du må meget gerne prøve igen senere.`;
	 Ns.exit())
      end
    fun valOf NONE = panic `valOf(NONE)`
      | valOf (SOME(v)) = v

    fun wrapPanic f a = f a 
      handle Fail s => panic (`Fail raised: ^s`)
	| X => panic(`wrapPanic: some error happended: ^(General.exnMessage X)`)
      
  end