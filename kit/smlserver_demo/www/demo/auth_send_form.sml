val _ = Page.return "Obtain Password by Email"
`Submit your <b>email address</b> below.
 <form method=post action=auth_send.sml>
 <table>
 <tr><td><b>Email address</b>
     <td><input type=text name=email><p>
 <tr><td align=center colspan=2>
     <input type=submit value="Send me my Password">
 </table>
 </form>`