val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => ScsConfig.scs_site_index_page() (* Default target url *)

val msg =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "msg" of
    SOME m => m
  | NONE => "" 

val login_info = Quot.toString

`Indtast den email du anvender på IT-C, eksempelvis <b>login@it-c.dk</b> eller <b>dfs@ruc.dk</b>.<p> 
(Eng.: Please write the email that you use at IT-C (e.g., <b>login@it-c.dk</b> or <b>dfs@ruc.dk</b>))`

val passwd_info = Quot.toString
`<b>Indtast dit password til UCS:</b> Er du ny bruger eller har du glemt password 
 så klik <b>Få password?</b>.<br><br>
 <b>Type your UCS password:</b> If you are a new user or you forgot password then 
 click <b>Get password?</b>.`

(*
Indtast dit password til UCS.<p>Som brugere af IT-Cs IT systemer har
 vi fået tildelt flere forskellige logins og kodeord. For at logge på 
 UCS skal du anvende din email, som enten kan være på formen 
 <b>login@it-c.dk</b> eller en ekstern email,
 f.eks. <b>gjf@ruc.dk</b>. Hvis du er i tvivl om hvilken email du 
 anvender på IT-C, så kan du finde den med Find Person. Første gang du
 logger på UCS skal du anvende et maskingenereret password, som du kan
 få tilsendt med at klikke Få password?</a>. Når du først er logget
 på kan du frit ændre dit password.<p>(Eng.: Type your password to
 UCS.<p>Each user at IT-C has several logins and passwords for
 different systems. To logon to UCS you must use the email that you use
 at IT-C (e.g., <b>login@it-c.dk</b> or <b>gjf@ruc.dk</b>). If you do
 not know your IT-C email, then use the Search Person functionality. We
 have generated a password for you that you must use the first time you
 logon to UCS. You can have the password mailed to you by clicking
 Get password?. You can change your password freely when you are
 logged on.
*)


val _ = ScsPage.returnPg ("IT-C.net Authentication")
  (`
   <blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Velkommen til UCS! (Eng. Welcome to UCS!)" (`

   <b>UCS</b> (<b>U</b>niversity <b>C</b>ommunity <b>S</b>ystem) er en
   portal til IT-højskolens studieadministrative systemer.<br><br>
   (Eng. <b>UCS</b> (<b>U</b>niversity <b>C</b>ommunity <b>S</b>ystem)
   is a portal to the student administrative systems used at The IT
   University of Copenhagen.)<p>

   ^msg <p>&nbsp;</p>
    <center>` ^^
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Login:" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Email</b></td>
    <td><input type=text name=auth_login size=20><!-- (e.g., <b>login@it-c.dk</b> or <b>gjf@ruc.dk</b>)-->&nbsp;^(UcsPage.info login_info)</td>
    </tr>
    <tr><td><b>UCS Password</b></td>
    <td><input type=password name=auth_password size=20>&nbsp;^(UcsPage.info passwd_info)</td>
    </tr>
    <tr><td>&nbsp;</td><td><input type=submit value=Login></td>
    </tr>
   </table>
   </form>
   ^(UcsPage.icon_forward) <a href="mail_passwd_form.sml">Få password?</a><br>
   ^(UcsPage.icon_forward) (Eng. <a href="mail_passwd_form.sml">Get password?</a>)<p>`) ^^ 
   `</center><p></blockquote>`)))

