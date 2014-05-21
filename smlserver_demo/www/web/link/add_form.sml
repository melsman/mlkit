
val _ = 
  if Auth.isLoggedIn() then ()
  else
    (Web.returnRedirect 
       "/web/auth_form.sml?target=/web/link/add_form.sml"
     ; Web.exit())

val _ = Page.return "Submit Web-site that uses SMLserver"
  `You may delete your submission later
  <form action=add.sml>
  <table>
  <tr><td><b>URL</b> <td><input type=text size=40 name=url>
  <tr><td><b>Text</b> <td><input type=text size=40 name=text>
  <tr><td colspan=2 align=center>
      <input type=submit value="Submit Web-site">
  </table>
  </form>`
