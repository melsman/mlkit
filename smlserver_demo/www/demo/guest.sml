
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
 
fun layoutRow (f,acc) = 
  `<li> <i>^(f "comment")</i>
   -- <a href="mailto:^(f "email")">^(f "name")</a>
   <p>` ^^ acc

val rows = Db.fold layoutRow ``
  `select email,name,comment
   from guest
   order by name`

val _ = Page.return "Guest Book"
  (`<ul>` ^^ rows ^^ `</ul>` ^^ form)
