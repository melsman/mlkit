  val email = FormVar.wrapFail 
    FormVar.getStringErr ("email","email")

  val sql = `select name, note 
	     from employee 
	     where email = ^(Db.qqq email)`

  val _ =
    case Db.zeroOrOneRow sql of
      SOME [name, note] => 
	Page.return "Employee Search Success" 
	`<form action=update.sml method=post>
	   <input type=hidden name=email value="^email">
           <table align=center border=2> 
              <tr><th>Name:</th>
                  <td>^name</td></tr>
              <tr><th>Email:</th>
                  <td>^email</td></tr>
              <tr><th>Note:</th>
                  <td><input name=note type=text value="^note">
                  </td></tr>
              <tr><th>Password:</th>
                  <td><input name=passwd type=password>
                      <input type=submit value="Change Note">
                  </td></tr>
           </table>
         </form><p>
         Try a <a href=index.sml>new search?</a>`
    | _ => 
	 Page.return "Employee Search Failure"
	 `Use the back-button in your web-browser 
	 to go back and enter another email address`
