  (* the complex query that calculates the scores *)
  val query = 
    `select wine.wid, name, year, 
	    avg(rating) as average, 
	    count(*) as ratings
     from wine, rating
     where wine.wid = rating.wid
     group by wine.wid, name, year
     order by average desc, name, year`

  fun formatRow (g, acc) = 
    let val avg = g "average"
	val avgInt = 
	  case Int.fromString avg of
	    SOME i => i
	  | NONE => case Real.fromString avg of
	      SOME r => floor r
	    | NONE => raise Fail "Error in formatRow"
	val wid = g "wid"
    in acc ^^ 
      `<tr><td><a href="wine.sml?wid=^wid">^(g "name")</a>
	 (year ^(g "year"))
       <th> ^(RatingUtil.bottleImgs avgInt)
       <td align=center>^(g "ratings")
       <td><a href="add.sml?wid=^wid">rate it</a></tr>`
    end

  val _ = RatingUtil.returnPageWithTitle "Best Wines"
   (`<table width=95% bgcolor="#dddddd" border=1>
     <tr><th>Wine<th>Average Score (out of 6)
         <th>Ratings<th>&nbsp;` ^^
    (Db.fold formatRow `` query) ^^
    `</table>
     <form action=add.sml>
     <h2>Rate new wine - type its name and year</h2>
     <b>Name:</b><input type=text name=name size=30>&nbsp;
     <b>Year:</b><input type=text name=year size=4>&nbsp;
     <input type=submit value="Rate it...">
     </form>`)
