val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => Ns.Conn.location() ^ (ScsConfig.scs_site_index_page()) (* Default target url *)

val d = ScsDict.d ScsLang.en "scs/auth" "auth_form.sml"

val _ = ScsPage.returnPg (d"IT-C.net Authentication")
  (`<blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "IT-C.net" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>^(d "Email")</b></td>
    <td><input type=text name=auth_login size=20></td>
    </tr>
    <tr><td><b>^(d "Password")</b></td>
    <td><input type=password name=auth_password size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value=Login></td>
    </tr>
   </table>
   </form>
   `)  ^^ `<p>^(d "You can") <a href="mail_passwd_form.sml">^(d "get your password")</a> ^(d "by email").` ^^ `</blockquote>`)

