structure FV = FormVar

val person_id = 
  case Auth.verifyPerson()
    of SOME p => p
     | NONE => (Ns.returnRedirect Auth.loginPage
		; Ns.exit())

val url  = FV.wrapFail FV.getUrlErr ("url", "URL")
val text = FV.wrapFail FV.getStringErr ("text", "Text")

val insert = 
  `insert into link (link_id, person_id, url, text)
   values (^(Db.seqNextvalExp "link_seq"),
	   ^(Int.toString person_id),
	   ^(Db.qqq url),
	   ^(Db.qqq text))`

val _ = Db.dml insert

val _ = Ns.returnRedirect "index.sml"
