  val email  = FormVar.getStringOrFail "email"
  val passwd = FormVar.getStringOrFail "passwd"
  val note   = FormVar.getStringOrFail "note"

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
