(* Error handling, i.e. sending emails to systemadministrator when
errors happens. Should be extended with database and only a few emails
sent *)

signature SCS_ERROR =
  sig
    val logError   : quot -> unit
    val emailError : quot -> unit
    val panic      : quot -> 'a
    val valOf      : 'a option -> 'a
  end

structure ScsError :> SCS_ERROR =
  struct 
    fun logError emsg = Ns.log (Ns.Error, Quot.toString emsg)

    fun emailError emsg = Ns.Mail.send {to="nh@it-c.dk",from="ucs@it-c.dk",subject="ScsPanic",body=Quot.toString emsg}

    fun panic emsg = 
      let
	val emsg = `script: ^(Ns.Conn.location())^(Ns.Conn.url()).` ^^ emsg
      in
	(logError emsg;
	 emailError emsg;
	 ScsPage.returnPg "System Error" `
	 It seems that the system can't complete your request.<p>
	 This is probably our fault. The system administrator has been
	 notified.<p>
	 Please try again later`;
	 Ns.exit())
      end
    fun valOf NONE = panic `valOf(NONE)`
      | valOf (SOME(v)) = v
  end