  val person_id = 
    case Auth.verifyPerson()
      of SOME p => p
       | NONE => (Ns.returnRedirect Auth.loginPage
		  ; Ns.exit())

  val link_id = FormVar.wrapFail
    FormVar.getNatErr ("link_id", "Link id")

  val delete =
    `delete from link
     where person_id = ^(Int.toString person_id)
       and link_id = ^(Int.toString link_id)`

  val _ = Db.dml delete

  val _ = Ns.returnRedirect "index.sml"
