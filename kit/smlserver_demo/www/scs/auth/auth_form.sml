val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => ScsConfig.scs_site_index_page() (* Default target url *)

val msg =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "msg" of
    SOME m => m
  | NONE => "" 

val login_info = Quot.toString

`Indtast dit IT-C login<p> (eng.: Please write your IT-C login`

val passwd_info = Quot.toString

`Indtast dit password til UCS.<p>Som brugere af IT-Cs IT systemer har
vi fået tildelt flere forskellige logins og kodeord. For at logge på
UCS skal du anvende det login, som er en del af din email på
IT-C. Hvis du ikke kender din IT-C email, så kan du finde det med Find
Person. Første gang du logger på skal du anvende et maskingenereret
password, som du kan få tilsendt med at klikke Glemt
password?</a>. Når du først er logget på kan du frit ændre dit
password.<p>(Eng.: Indtast dit password til UCS.<p>Each user at IT-C
has several logins and passwords for different systems. To logon to
UCS you must use your login that is part of your IT-C email. If you do
not know your IT-C email, then use the Search Person functionality. We
have generated a password for you that you must use the first time you
logon to UCS. You can have the password mailed to you by clicking
Forgot password?. You can change your password freely when you are
logged on.)`

val _ = ScsPage.returnPg ("IT-C.net Authentication")
  (`
   <blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Velkommen til UCS! (eng. Welcome to UCS!)" (`

   <b>UCS</b> (<b>U</b>niversity <b>C</b>ommunity <b>S</b>ystem) er en
   portal til IT-højskolens studieadministrative systemer.<br><br>
   (eng. <b>UCS</b> (<b>U</b>niversity <b>C</b>ommunity <b>S</b>ystem)
   is a portal to the student administrative systems used at The IT
   University of Copenhagen.)<p>

   ^msg <p>&nbsp;</p>
    <center>` ^^
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Login:" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Email</b></td>
    <td><input type=text name=auth_login size=10>@it-c.dk&nbsp;^(UcsPage.info login_info)</td>
    </tr>
    <tr><td><b>UCS Password</b></td>
    <td><input type=password name=auth_password size=20>&nbsp;^(UcsPage.info passwd_info)</td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value=Login></td>
    </tr>
   </table>
   </form>
   ^(UcsPage.icon_forward) <a href="mail_passwd_form.sml">Glemt password?</a><br>
   ^(UcsPage.icon_forward) (eng. <a href="mail_passwd_form.sml">Forgot password?</a>)<p>`) ^^ 
   `</center><p></blockquote>`)))

