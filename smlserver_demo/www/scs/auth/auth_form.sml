val target =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "target" of
    SOME t => t
  | NONE => ScsConfig.scs_site_index_page() (* Default target url *)

val msg =
  case ScsFormVar.wrapOpt ScsFormVar.getStringErr "msg" of
    SOME m => m
  | NONE => "" 

val login_info = Quot.toString
`Indtast den email du anvender på ITU, eksempelvis <b>login@itu.dk</b> eller <b>dfs@ruc.dk</b>.
Hvis du har en ITU email (f.eks. <b>foo@itu.dk</b>), kan du nøjes med at 
skrive dit login (dvs. <b>foo</b>)).

<p>(Eng.: Please write the email that you use at ITU (e.g., <b>login@itu.dk</b> or <b>dfs@ruc.dk</b>)). 
If you have an ITU email address (e.g. <b>foo@itu.dk</b>), 
then you only need to write your login (<b>foo</b>)</p>`

val passwd_info = Quot.toString
`<b>Indtast dit password til UCS:</b> Er du ny bruger eller har du glemt password 
 så klik <b>Få password</b>.<br><br>
 <b>Type your UCS password:</b> If you are a new user or you forgot your password then click <b>Get password</b>.`

val _ = ScsPage.returnPg ("ITU.net Authentication")
  (`
   <blockquote>
  ` ^^   
  (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Velkommen til UCS! (Eng. Welcome to UCS!)" (`
 
   <b>UCS</b> (<b>U</b>niversity <b>C</b>ommunity <b>S</b>ystem) er en
   portal til IT-Universitetet i Københavns studieadministrative systemer.<br><br>
   (Eng. <b>UCS</b> (<b>U</b>niversity <b>C</b>ommunity <b>S</b>ystem)
   is a portal to the student administrative systems used at The IT
   University of Copenhagen.)<p>
   ^msg <br>
    <center>` ^^
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Login:" `
   <form action="/scs/auth/auth.sml" method=post>
   <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Email</b></td>
    <td><input type=text name=auth_login size=20><!-- (e.g., <b>login@itu.dk</b> or <b>gjf@ruc.dk</b>)-->&nbsp;^(UcsPage.info login_info)</td>
    </tr>
    <tr><td><b>UCS Password</b></td>
    <td><input type=password name=auth_password size=20>&nbsp;^(UcsPage.info passwd_info)</td>
    </tr>
    <tr><td>&nbsp;</td><td><input type=submit value=Login></td>
    </tr>
   </table>
   </form>
   ^(UcsPage.icon_forward()) <a href="^(Html.genUrl "mail_passwd_form.sml" 
    [("lang",ScsLang.toString ScsLang.da)])">Få password</a><br>
   ^(UcsPage.icon_forward()) (Eng. <a href="^(Html.genUrl "mail_passwd_form.sml" 
			      [("lang",ScsLang.toString ScsLang.en)])">Get password</a>)<p>`) ^^ 
   `</center><p></blockquote>`)))

