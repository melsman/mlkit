Page.return "Send an email"
`<form action=mail.sml>
  <table>
  <tr><th align=left>To:   </th><td align=right><input type=text name=to></td></tr>
  <tr><th align=left>From: </th><td align=right><input type=text name=from></td></tr>
  <tr><th align=left>Subject: </th><td align=right><input type=text name=subject></td></tr>
  <tr><td colspan=2><textarea name=body cols=40 rows=10>Fill in...</textarea></td></tr>
  <tr><td colspan=2 align=center><input type=submit value="Send Email"></td></tr>  
  </table>
 </form>`
