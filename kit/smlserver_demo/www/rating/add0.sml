  val comment  = ScsFormVar.wrapFail ScsFormVar.getStringErr ("comment", "comment")
  val fullname = ScsFormVar.wrapFail ScsFormVar.getStringErr ("fullname", "fullname")
  val email    = ScsFormVar.wrapFail ScsFormVar.getStringErr ("email", "email")
  val wid      = Int.toString(ScsFormVar.wrapFail ScsFormVar.getNatErr ("wid","internal number"))
  val rating   = Int.toString(ScsFormVar.wrapFail (ScsFormVar.getIntRangeErr 0 6) ("rating","rating"))

  val _ = Db.dml
    `insert into rating (wid, comment, fullname, 
                         email, rating)
     values (^wid, ^(Db.qq' comment), ^(Db.qq' fullname), 
	     ^(Db.qq' email), ^rating)`

  val _ = Ns.returnRedirect "index.sml"
