  structure FV = FormVar
  val comment = FV.wrapFail FV.getStringErr 
    ("comment", "comment")
  val fullname = FV.wrapFail FV.getStringErr 
    ("fullname", "fullname")
  val email = FV.wrapFail FV.getStringErr 
    ("email", "email")
  val wid = Int.toString(FV.wrapFail FV.getNatErr 
			 ("wid","internal number"))
  val rating = 
    Int.toString(FV.wrapFail (FV.getIntRangeErr 0 6) 
		 ("rating","rating"))

  val _ = Db.dml
    `insert into rating (wid, comments, fullname, 
                         email, rating)
     values (^wid, ^(Db.qq' comment), ^(Db.qq' fullname), 
	     ^(Db.qq' email), ^rating)`

  val _ = Ns.returnRedirect "index.sml"
