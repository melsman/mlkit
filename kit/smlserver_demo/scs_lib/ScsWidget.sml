signature SCS_WIDGET =
  sig
    val namedBox : {hdcolor: string, bgcolor: string, title: string, body: quot} -> quot
    val lineTable: {hdcolor: string, row_col1: string, row_col2: string,
		     header: quot, align: string, footer: quot} -> ('a -> quot) -> 'a list -> quot
    val grpTable : {hdcolor: string, row_col1: string, row_col2: string,
		    header: quot, align: string, footer: quot} -> 
                   ('a -> quot) -> ('a -> string) -> 'a list -> quot
  end

structure ScsWidget :> SCS_WIDGET =
  struct
    fun namedBox {hdcolor, bgcolor, title, body} : quot = `
      <table border=0 bgcolor="^bgcolor" cellpadding=1 cellspacing=0 width=100%>
      <tr><td><table border=0 bgcolor="^bgcolor" cellpadding=3 cellspacing=0 width=100%>
      <tr><td bgcolor="^hdcolor">
      <font color=white><b>^title</b></font>
      </td></tr><tr><td> `
      ^^ body ^^ ` 
      </td></tr>
      </table>
      </td>
      </tr>
      </table>`

    fun lineTable {hdcolor: string, row_col1: string, row_col2: string,
		   align: string, header: quot, footer: quot} fn_row rows = 
      `<table align="^align" bgcolor="^hdcolor" cellpadding=2 cellspacing=0 border=0>` ^^
       header ^^ 
       #3(List.foldr (fn (r,(rc1,rc2,acc)) => 
		      (rc2,rc1,`<tr bgcolor="^rc1">` ^^ fn_row r ^^ 
		       `</tr>^("\n")` ^^ acc)) (row_col1,row_col2,footer^^`</table>`) rows)

    fun grpTable {hdcolor: string, row_col1: string, row_col2: string,
		  align: string, header: quot, footer: quot} fn_row fn_get_id rows = 
      `<table align="^align" bgcolor="^hdcolor" cellpadding=2 cellspacing=0 border=0>` ^^
       header ^^ 
       #4(List.foldr (fn (r,(rc1,rc2,old_id,acc)) => 
		      let val (rc1,rc2) = if old_id=fn_get_id r then (rc1,rc2) else (rc2,rc1)
		      in (rc1,rc2,fn_get_id r,`<tr bgcolor="^rc1">` ^^ fn_row r ^^ 
		       `</tr>^("\n")` ^^ acc)end) (row_col1,row_col2,"",footer^^`</table>`) rows)

  end