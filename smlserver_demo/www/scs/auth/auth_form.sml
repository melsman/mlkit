val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => ScsConfig.scs_site_index_page() (* Default target url *)

val msg =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "msg" of
    SOME m => m
  | NONE => "" 

fun gen_info i = (UcsPage.info (ScsDict.s i)) handle ScsDict.ScsDict _ => ""

val login_info = [(ScsLang.da,`Indtast dit IT-C login`),
		  (ScsLang.en,`Please write your IT-C login`)]

val passwd_info = [(ScsLang.da,` Første gang du logger ind skal du
	     bede om at få dit maskingenererede kodeord
	     tilsendt. Derefter kan du selv vælge dit kodeord til
	     dette system (eksempelvis dit email kodeord) ved at vælge
	     <b>Skift kodeord</b> fra dit arbejdsbord.`),
	     (ScsLang.en,`You may choose your own password to use with
	     this service (e.g., your email passowrd). The first time
	     you login you ask the service to mail it to you. When you
	     are logged in, you change the password by clicking the
	     link <b>Change password</b> from your workspace.`)]

val login_msg = 
`Som brugere af IT-C's IT systemer har vi fået tildelt flere
forskellige logins og kodeord. For at logge på UCS skal du anvende det
login, som er en del af din email på IT-C. Hvis du ikke kender din
IT-C email, så kan du finde det med <a target="find_person"
href="/ucs/people/combined_search.sml">Find Person</a>. Første gang du
logger på skal du anvende et maskingenereret password som du kan få
tilsendt med at klikke <a href="mail_passwd_form.sml">Glemt
password?</a>. Når du først er logget på kan du frit ændre dit password.<p>

(eng.: Each user at IT-C has several logins and passwords for
different systems. To logon to UCS you must use your login that is
part of your IT-C email. If you do not know your IT-C email, then use
the <a target="find_person"
href="/ucs/people/combined_search.sml">Search Person</a>
functionality. We have generated a password for you that you must use
the first time you logon to UCS. You can have the password mailed to
you by clicking <a href="mail_passwd_form.sml">Forgot
password?</a>. You can change your password freely when you are logged
on.)`

val _ = ScsPage.returnPg ("IT-C.net Authentication")
  (`
   <blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Velkommen til UCS! (eng. Welcome to UCS!)" (`

   <b>UCS</b> (<b>U</b>niveristy <b>C</b>ommunity <b>S</b>ystem) er en
   del af IT-højskolens studieadministrative systemer.<br>
   (eng. <b>UCS</b> (<b>U</b>niveristy <b>C</b>ommunity <b>S</b>ystem) is one of the 
    student administrative systems used at The IT University of Copenhagen.)<p>

   ^msg <p>&nbsp;</p>
    <center>` ^^
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Login:" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Email</b></td>
    <td><input type=text name=auth_login size=10>@it-c.dk&nbsp;^(gen_info login_info)</td>
    </tr>
    <tr><td><b>Password</b></td>
    <td><input type=password name=auth_password size=20>&nbsp;^(gen_info passwd_info)</td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value=Login></td>
    </tr>
   </table>
   </form>
   ^(UcsPage.icon_forward) <a href="mail_passwd_form.sml">Glemt password?</a><br>
   ^(UcsPage.icon_forward) (eng. <a href="mail_passwd_form.sml">Forgot password?</a>)<p>`) ^^ 
   `</center><p>` ^^
   login_msg)) ^^ `</blockquote>`)

