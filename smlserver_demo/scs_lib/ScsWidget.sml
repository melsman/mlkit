signature SCS_WIDGET =
  sig
    (* [namedBox hdcolor bgcolor title body] returns HTML for a box
       with a title and body *)
    val namedBox : string -> string -> string -> quot -> quot

    (* [lineTable layout format_fn rows] returns HTML for a table
       where each row is formatted using the format function
       format_fn. The row colors alternate between row_col1 and
       row_col2 *)
    val lineTable: {hdcolor: string, row_col1: string, row_col2: string,
		     header: quot, align: string, footer: quot} -> ('a -> quot) -> 'a list -> quot

    (* [grpTable layout fn_row fn_get_id rows] is similar to lineTable
       except that on each row with the same color you may have
       serveral rows from the list rows. Rows with the same id are shown on
       the same row in the resulting table.  The function fn_get_id
       must return the id of each row. *)
    val grpTable : {hdcolor: string, row_col1: string, row_col2: string,
		    header: quot, align: string, footer: quot} -> 
                   ('a -> quot) -> ('a -> string) -> 'a list -> quot

    (* [formBox action bs body] returns HTML for a form with content
       body. The value bs is a list of buttons. A button is a pair
       (name,value) where name is the form variable name and value is
       the value printed on the button. *)
    val formBox  : string -> (string * string) list -> quot -> quot

    (* [ul elems] returns HTML for an unordered list. *)
    val ul       : quot list -> quot

    (* [ol elems] returns HTML for an ordered list. *)
    val ol       : quot list -> quot

    (* [ta rows cols fv v] returns HTML for a textarea of size rows
       and cols, named fv and filled out with value v. *)
    val ta       : int -> int -> string -> quot -> quot

    (* [largeTa] presized textarea. *)
    val largeTa  : string -> quot -> quot

    (* [mediumTa] presized textarea. *)
    val mediumTa : string -> quot -> quot

    (* [mediumTa] presized textarea. *)
    val mediumWideTa : string -> quot -> quot

    (* [smallTa] presized textarea. *)
    val smallTa  : string -> quot -> quot

    (* [select opts fv] returns HTML for a selection box with options
       opts. An option is a pair (label,value) where label is shown to
       the user and value is the value transmitted to the
       server. Formvariable name is fv. *)
    val select   : (string * string) list -> string -> quot

    (* [selectWithDefault opts default fv] is similar to select except
       that if the default value default exists as a value in the
       options list opts, then it is pre-selected. *)
    val selectWithDefault : (string * string) list -> string -> string -> quot

    (* [selectYesNoWithDefault default fv] returns a selection box
       with yes and no options. Value returned is either t or f
       matching the encoding we use in Oracle. *)
    val selectYesNoWithDefault : string -> string -> quot

    (* [oneLine] used for what????? *)
    val oneLine : quot * quot -> quot 

    (* [errorOnEmptyList] used for what????? *)
    val errorOnEmptyList : string -> 'a list -> 'a list

    (* [pickFromList action submit_b hvs items item_text body] The
       value items is a list of tuples (text,name,value) each
       representing an option, where text describes the option, name
       is the formvariable name and value is the value assigned to the
       form variable in case the user chooses this option. The value
       submit_b is the button clicked when one of the options has been
       chosen. The variable hvs is a list of pairs (name,value)
       describing hidden values which are also transferred to the
       target script. The value action refers to the target
       script. The value body is a general description of the choises. *)
    val pickFromList : string -> (string * string) -> (string * string) list -> 
                       (quot * string * string) list -> string -> quot -> Ns.status

    (* [ulWithNLinks heads_and_ids fn_cols end_line] creates an
       unordered list, where each item is a name and a list of
       links. The value heads_and_ids is a list of pairs (name,id)
       where name describes the row (e.g., F2003TOM below) and id is a unique
       identifier (e.g., into a table in the database).

       The value fn_cols is a list of functions fn_col mapping an id
       into a ahref. A function fn_col takes an id as argument and returns an
       ahref for that id in that column. The value end_line is
       appended after the unordered list.

       You can use it to build the following interface:
  
         * F2003TOM [ret] [stat] [kurser]
         * F2002TOM [ret] [stat] [kurser]
         * F2001TOM [ret] [stat] [kurser] *)
    val ulWithNLinks     : (string * string) list -> (string -> quot) list -> quot -> quot

    (* [tableWithNCols inputs] returns HTML for a table with N
       columns. The value inputs is a list where each element represent a
       row. A row is again a list of quotations. *)
    val tableWithNCols   : quot list list ->  quot

    (* [tableWithTwoCols inputs] is similar to tableWithNCols except
       that each row is represented as a pair (i.e., a table with two
       columns is returned). *)
    val tableWithTwoCols : (quot * quot) list -> quot 

    (* [tableWithOneCol inputs] is similar to tableWithNCols except
       that the tale contains one column only. *)
    val tableWithOneCol  : quot list -> quot 

    (* [genSize n] returns a size attribute *)
    val genSize         : int -> quot
    
    (* [genMaxSize s m] returns a size (s) and maxlength (m) attribute *)
    val genMaxSize      : int -> int -> quot

    (* [genValue v] returns a value attribute *)
    val genValue        : string -> quot
 
    (* [intext s fv] returns an empty HTML input tag with 
       name fv and size s *)
    val intext          : int -> string -> quot

    (* [intextMaxLen s m fv] is similar to intext except that a max
       length attribute is appended *)
    val intextMaxLen    : int -> int -> string -> quot

    (* [intextVal s v fv] is similar to intext except that the 
       value is prefilled with v *)
    val intextVal       : int -> string -> string -> quot

    (* [intextMaxLenVal s m v fv] is similar to intextVal except that
       a max length attribute is appended. *)
    val intextMaxLenVal : int -> int -> string -> string -> quot

    (* [intextDate d fv] returns an HTML input tag prefilled with a
       date. *)
    val intextDate      : Date.date option -> string -> quot

    (* [maybe fv text] returns text if formvariable fv is non
       empty. If empty, then the empty quotation is returned. *)
    val maybe           : string -> quot -> quot

    (* [selectLang lang default] returns an HTML selection box with
       all languages and the users default language preselected. *)
    val selectLang      : ScsLang.lang -> string -> quot
  end

structure ScsWidget :> SCS_WIDGET =
  struct
    fun namedBox hdcolor bgcolor title body = `
      <table border=0 bgcolor="^hdcolor" cellpadding=1 cellspacing=0>
        <tr><td>
          <table border=0 bgcolor="^bgcolor" cellpadding=3 cellspacing=0 width=100%>
          <tr><td bgcolor="^hdcolor">
          <font color=white><b>^title</b></font>
          </td></tr>
          <tr><td> `
           ^^ body ^^ ` 
          </td></tr>
          </table>
        </td></tr>
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
      </td></tr>
      </table><br>
      ` ^^ (List.foldr (fn ((name,value),acc) => `<input class=submit type=submit name=^name value="^value">` ^^ acc) `</form>` bs)

    fun ul qs = `<ul>` ^^ (List.foldr (fn (q,acc) => `
				       <li>` ^^ q ^^ acc) `</ul>` qs)

    fun ol qs = `<ol>` ^^ (List.foldr (fn (q,acc) => `
				       <li>` ^^ q ^^ acc) `</ol>` qs)

    fun ta rows cols n v = `<textarea name="^n" rows=^(Int.toString rows) cols=^(Int.toString cols) wrap="physical">` ^^ 
                            v ^^ `</textarea>`

    val largeTa = ta 20 80
    val mediumTa = ta 10 40
    val mediumWideTa = ta 10 80
    val smallTa = ta 5 20

    fun select opts fv =
      `<select name="^fv">
      ` ^^ (List.foldr (fn ((l,v),acc) => `
			<option value="^v">^l</option>` ^^ acc) `</select>` opts)

    fun selectWithDefault opts default fv =
      `<select name="^fv">
      ` ^^ (List.foldr (fn ((l,v),acc) => `
			<option value="^v" ^(if v=default then "selected" else "")>^l</option>` ^^ acc) `</select>` opts)

    fun selectYesNoWithDefault default fv = 
      let
        val yes = [(ScsLang.en,`Yes`),(ScsLang.da,`Ja`)]
        val no = [(ScsLang.en,`No`),(ScsLang.da, `Nej`)]
      in
	selectWithDefault [(ScsDict.s yes,"t"),(ScsDict.s no,"f")] default fv
      end

    fun oneLine (text,widget) = text ^^ ` ` ^^ widget

    fun errorOnEmptyList (text : string) (ls : 'a list) : 'a list =
      (case ls of
	[] => (ScsPage.returnPg (ScsDict.s [(ScsLang.en,`Can't find `),(ScsLang.da,`Kan ikke finde `)] ^ text)
	       (case ScsLogin.user_lang of
		  ScsLang.en => `
		    We had a problem finding ^text.<p>
		    Please back up using your browser, and resubmit your entry<p>
		    Thank you.`
		| ScsLang.da => 
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
      ScsPage.returnPg (ScsDict.s [(ScsLang.en,`Pick From List`),(ScsLang.da,`Find Fra Liste`)]) (body ^^
      formBox action [submit_b]
      ((List.foldr (fn ((n,v),acc) => `
		    <input type=hidden name="^n" value="^v">` ^^ acc) `` hvs) ^^ `
       <table>` ^^ 
       (List.foldr (fn ((text,n,v),acc) => `
		    <tr><td align=right>`^^text^^`</td>
		    <td><input type=radio name="^n" value="^v"></td></tr>` ^^ acc) `` items) ^^ `
       </table>`))

    fun ulWithNLinks (heads_and_ids:(string * string) list) (fn_cols:(string -> quot) list) (end_line:quot) : quot = `
      <ul>
      ` ^^ (List.foldr (fn ((h,id),acc) => `
			<li>^h ` ^^ (List.foldr (fn (fn_col,acc) => `[` ^^ fn_col id ^^ `] ` ^^ acc) acc fn_cols)) 
	    `` heads_and_ids) ^^ `
      <p>
      <li>` ^^ end_line ^^ `
      </ul>`

    fun tableWithNCols (inputs:quot list list) : quot = `
      <table border=0 cellspacing=2 cellpadding=2>
      ` ^^ (List.foldr (fn (ts,acc) =>
			Html.tr (List.foldr (fn (t,acc) => Html.td t ^^ acc) `` ts) ^^ acc) `</table>` inputs)
    val tableWithTwoCols = tableWithNCols o (List.map (fn (c1,c2) => [c1,c2]))
    val tableWithOneCol = tableWithNCols o (List.map (fn c => [c]))

    fun genSize s = `size="^(Int.toString s)"`
    fun genMaxSize s m = genSize s ^^ ` maxlength="^(Int.toString m)"`
    fun genValue v = `value="^v"`
    fun intext s fv = Html.intext fv (genSize s)
    fun intextMaxLen s m fv = Html.intext fv (genMaxSize s m)
    fun intextVal s v fv = Html.intext fv (genSize s ^^ ` ` ^^ genValue v)
    fun intextMaxLenVal s m v fv = Html.intext fv (genMaxSize s m ^^ ` ` ^^ genValue v)
    fun intextDate d fv =
      case d of
	NONE => intextMaxLenVal 10 10 "" fv
      | SOME d' => intextMaxLenVal 10 10 (ScsDate.pp d') fv

    fun maybe fv text = if fv = "" then `` else text

    fun selectLang default fv = 
      selectWithDefault (ScsLang.all_for_sel_box ScsLogin.user_lang)
      (ScsLang.toString default) fv
  end
