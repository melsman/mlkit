  val getString = FormVar.wrapFail FormVar.getStringErr

  val email  = getString ("email","email")
  val passwd = getString ("passwd","passwd")
  val note   = getString ("note", "note")

  val update = `update employee
                set note = ^(Db.qq' note)
                where email = ^(Db.qq' email)
                  and passwd = ^(Db.qq' passwd)`

  val _ = 
    if Db.dml update = Ns.OK then 
      Ns.returnRedirect ("search.sml?email=" 
			 ^ Ns.encodeUrl email)
    else Page.return "Employee Database" `Update failed`
