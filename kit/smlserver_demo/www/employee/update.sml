  val email  = FormVar.wrapFail FormVar.getStringErr ("email","email")
  val passwd = FormVar.wrapFail FormVar.getStringErr ("passwd","passwd")
  val note   = FormVar.wrapFail FormVar.getStringErr ("note", "note")

  val update = `update employee
                set note = ^(Db.qq' note)
                where email = ^(Db.qq' email)
                  and passwd = ^(Db.qq' passwd)`
  val _ = 
    if Db.dml update = Ns.OK then 
      Ns.returnRedirect ("search.sml?email=" 
			 ^ Ns.encodeUrl email)
    else 
      Ns.return `Update failed`
