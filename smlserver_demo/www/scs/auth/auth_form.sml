val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => ScsConfig.scs_site_index_page() (* Default target url *)

val msg =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "msg" of
    SOME m => m
  | NONE => "" 

val _ = ScsPage.returnPg ("IT-C.net Authentication")
  (`^msg
   <blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "IT-C.net" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Email</b></td>
    <td><input type=text name=auth_login size=10><b>@it-c.dk</b></td>
    </tr>
    <tr><td><b>Password</b></td>
    <td><input type=password name=auth_password size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value=Login></td>
    </tr>
   </table>
   </form>
   `)  ^^ `Klik for at modtage <a href="mail_passwd_form.sml">password</a> pr email.<br>
          (eng. You can <a href="mail_passwd_form.sml">get your password</a> by email.)</blockquote><p>

	  Som bruger på IT-C anvender du forskellige kodeord til
	  forskellige systemer, eksempelvis Unix login, Windows login,
	  login til kursusevalueringssytem osv. I denne service giver
	  vi dig muligheden for selv at vælge det kodeord du ønsker at
	  anvende. Første gang du logger ind skal du bede om at få dit <a href="mail_passwd_from.sml">maskingenererede 
	  kodeord tilsendt</a>. Derefter kan du selv vælge dit kodeord til dette system (eksempelvis dit email kodeord)
	  ved at vælge "Skift kodeord" fra dit arbejdsbord.<p>

	  As a user at IT-C you use several passwords (e.g., one Unix password, one Windoes password and 
          a password for the course evaluation system). You may choose your own password to use with this
	  service (e.g., your email passowrd). The first time you login you ask the service to 
	  <a href="mail_passwd_from.sml">mail it to</a> you. When you are logged in, you change the password by clicking
	  the link "Change password" from your workspace.<p>`)

