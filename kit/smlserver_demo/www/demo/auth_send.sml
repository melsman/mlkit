structure FV = FormVar
val email = FV.wrapFail FV.getEmailErr ("email", "Email")

val query = `select person_id from person
             where email = ^(Db.qqq email)`

val _ = 
  case Db.zeroOrOneField query
    of SOME p => 
      (case Int.fromString p
	 of SOME pid =>	 
	   (Auth.sendPassword pid;
	    Page.return "Email has been sent"
	    `Please check your mail-box and proceed to the
	    <a href=^(Auth.loginPage)>login page</a>.`)
	  | NONE => raise Fail "int expected")
     | NONE => 
      Page.return "Email not in database"
      `Please proceed to the
       <a href=^(Auth.loginPage)>login page</a>.`
