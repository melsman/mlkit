val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => Ns.Conn.location() ^ (ScsConfig.scs_site_index_page()) (* Default target url *)

val _ = ScsPage.returnPg "IT-C.net Authentication" 
  (`<blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "IT-C.net" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Email</b></td>
    <td><input type=text name=auth_login size=20></td>
    </tr>
    <tr><td><b>Password</b></td>
    <td><input type=password name=auth_password size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value=Login></td>
    </tr>
  </table>
   </form>
   `)  ^^ `You can <a href="mail_passwd_form.sml">get your password</a> by email.` ^^ `</blockquote>`)

