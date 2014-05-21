
val form = 
  `<form method=post action=guest_add.sml>
    <table>
     <tr><td valign=top colspan=3>New comment<br>
         <textarea name=c cols=65 rows=3
            wrap=virtual>Fill in...</textarea></tr>       
     <tr><td>Name<br><input type=text size=25 name=n>
         <td>Email<br><input type=text size=25 name=e>
         <td><br><input type=submit value="Add">
     </tr>       
    </table>
   </form>`

 fun log x = Web.log(Web.Debug, x)
 
fun layoutRow (f,acc) = 
  case (f "comments", f "name", f "email") of (c, n, e) =>
  (`<li> <i>^(c)</i>
   -- <a href="mailto:^(e)">^(n)</a>
   <p>` ^^ acc)

val rows = Db.fold layoutRow ``
  `select email,name,comments
   from guest
   order by name`

val _ = Page.return "Guest Book"
  (`<ul>` ^^ rows ^^ `</ul>` ^^ form) 
  handle Fail m => Page.return "Error on page" (Quot.fromString m)
