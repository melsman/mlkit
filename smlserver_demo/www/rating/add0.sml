  val comment  = FormVar.getStringOrFail "comment"
  val fullname = FormVar.getStringOrFail "fullname"
  val email    = FormVar.getStringOrFail "email"
  val wid      = Int.toString(FormVar.getNatOrFail "wid")
  val rating   = Int.toString(FormVar.getIntRangeOrFail 
			      0 6 "rating")

  val _ = Db.dml
    `insert into rating (wid, comment, fullname, 
                         email, rating)
     values (^wid, ^(Db.qq' comment), ^(Db.qq' fullname), 
	     ^(Db.qq' email), ^rating)`

  val _ = Ns.returnRedirect "index.sml"
