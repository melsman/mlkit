  (* Present comments and ratings for a specific wine *)
  val wid = FormVar.wrapFail FormVar.getNatErr 
    ("wid","internal number")

  val query = 
    `select comments, fullname, email, rating
     from rating 
     where wid = ^(Int.toString wid)`

  val lines = Db.fold 
    (fn (g,r) =>
     let val rating = 
	   case Int.fromString (g"rating") of
	     SOME i => i
	   | NONE => raise Fail "Rating not integer"
     in 
       `<tr><th> ^(RatingUtil.bottleImgs rating)
        <td> ^(g"comments")
        <td> ^(RatingUtil.mailto (g"email") (g"fullname"))`
     end ^^ r) nil query

  val body =
    `<table width=95% bgcolor="#dddddd" border=1>
       <tr><th>Rating<th>Comment<th>Rater` ^^ lines ^^ 
    `</table>
     <p>Back to <a href=index.sml>Best Wines</a>`

  val name = Db.oneField 
    `select name from wine
     where wid = ^(Int.toString wid)`

  val _ = RatingUtil.returnPageWithTitle 
    ("Ratings - " ^ name) body