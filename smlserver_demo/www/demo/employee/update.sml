  val getString = FormVar.wrapFail FormVar.getStringErr

  val email  = getString ("email","email")
  val passwd = getString ("passwd","passwd")
  val note   = getString ("note", "note")

  val update = `update employee
                set note = ^(Db.qqq note)
                where email = ^(Db.qqq email)
                  and passwd = ^(Db.qqq passwd)`

  val _ = 
    (Db.dml update;
     Ns.returnRedirect ("search.sml?email=" 
			 ^ Ns.encodeUrl email))
    handle _ => 
      Page.return "Employee Database" `Update failed`
