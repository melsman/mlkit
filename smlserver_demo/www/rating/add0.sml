  val comment  = FormVar.wrapFail FormVar.getStringErr ("comment", "comment")
  val fullname = FormVar.wrapFail FormVar.getStringErr ("fullname", "fullname")
  val email    = FormVar.wrapFail FormVar.getStringErr ("email", "email")
  val wid      = Int.toString(FormVar.wrapFail FormVar.getNatErr ("wid","internal number"))
  val rating   = Int.toString(FormVar.wrapFail (FormVar.getIntRangeErr 0 6) ("rating","rating"))

  val _ = Db.dml
    `insert into rating (wid, comment, fullname, 
                         email, rating)
     values (^wid, ^(Db.qq' comment), ^(Db.qq' fullname), 
	     ^(Db.qq' email), ^rating)`

  val _ = Ns.returnRedirect "index.sml"
