signature SCS_WIDGET =
  sig
    val namedBox : {hdcolor: string, bgcolor: string, title: string, body: quot} -> quot
    val lineTable: {hdcolor: string, row_col1: string, row_col2: string,
		     header: quot, align: string, footer: quot} -> ('a -> quot) -> 'a list -> quot
    val grpTable : {hdcolor: string, row_col1: string, row_col2: string,
		    header: quot, align: string, footer: quot} -> 
                   ('a -> quot) -> ('a -> string) -> 'a list -> quot
    val formBox  : string -> (string * string) list -> quot -> quot
    val ul : quot list -> quot
    val largeTA  : string -> quot -> quot
    val mediumTA : string -> quot -> quot
    val smallTA  : string -> quot -> quot
    val select   : (string * string) list -> string -> quot
    val selectWithDefault : (string * string) list -> string -> string -> quot
    val oneLine : quot * quot -> quot
    val errorOnEmptyList : string -> 'a list -> 'a list
    val pickFromList : string -> (string * string) -> (string * string) list -> 
                       (quot * string * string) list -> string -> quot -> Ns.status
  end

structure ScsWidget :> SCS_WIDGET =
  struct
    val % = ScsDict.d ScsLang.English
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

    fun formBox action bs body = `
      <form method=post action="^action">
      <table border=1>
      <tr valign=top>
      <td>` ^^
      body ^^ `
      </td>
      </table><br>
      ` ^^ (List.foldr (fn ((name,value),acc) => `<input class=submit type=submit name=^name value="^value">` ^^ acc) `</form>` bs)

    fun ul qs = `<ul>` ^^ (List.foldr (fn (q,acc) => `
				       <li>` ^^ q ^^ acc) `</ul>` qs)

    fun TA rows cols n v = `<textarea name="^n" rows=^rows cols=^cols>
      ` ^^ v ^^ `
      </textarea>`

    val largeTA = TA "20" "80"
    val mediumTA = TA "10" "40"
    val smallTA = TA "5" "20"

    fun select opts fv =
      `<select name="^fv">
      ` ^^ (List.foldr (fn ((l,v),acc) => `
			<option value="^v">^l</option>` ^^ acc) `</select>` opts)

    fun selectWithDefault opts default fv =
      `<select name="^fv">
      ` ^^ (List.foldr (fn ((l,v),acc) => `
			<option value="^v" ^(if v=default then "selected" else "")>^l</option>` ^^ acc) `</select>` opts)

    fun oneLine (text,widget) = text ^^ ` ` ^^ widget

    fun errorOnEmptyList (text : string) (ls : 'a list) : 'a list =
      (case ls of
	[] => (ScsPage.returnPg (%"Can't find " ^ text) 
	       (case ScsLogin.user_lang of
		  ScsLang.English => `
		    We had a problem finding ^text.<p>
		    Please back up using your browser, and resubmit your entry<p>
		    Thank you.`
		| ScsLang.Danish => 
		    `Vi kan ikke finde ^text.<p>
		    Vær venlig at klikke på "tilbage"-knappen i din browser, og
		    indsend dine oplysninger igen<p>
		    På forhånd tak.`);
		  Ns.exit())
      | _ => ls)

    fun pickFromList action submit_b hvs items item_text body =
      case items of
	[] => (errorOnEmptyList item_text [];Ns.exit())
      | [(_,n,v)] => Ns.Conn.returnRedirect (Ns.buildUrl action ((n,v)::submit_b::hvs))
      | _ => 
      ScsPage.returnPg (%"Pick From List") (body ^^
      formBox action [submit_b]
      ((List.foldr (fn ((n,v),acc) => `
		    <input type=hidden name="^n" value="^v">` ^^ acc) `` hvs) ^^ `
       <table>` ^^ 
       (List.foldr (fn ((text,n,v),acc) => `
		    <tr><td align=right>`^^text^^`</td><td><input type=radio name="^n" value="^v"></td></tr>` ^^ acc) `` items) ^^ `
       </table>`))
  end