val _ = ScsPage.returnPg "IT-C.net Obtain Password" 
  (`<blockquote>
   Indtast din email og du vil modtage den pr. email.<p>
   (eng. Write your email address and we will send you your password by email.)<p>` ^^ 
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Obtain Password" `
   <form action="/scs/auth/mail_passwd.sml" method=post>
   <table>
    <tr><td><b>Email</b></td>
    <td><input type=text name=email size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value="Obtain Password"></td>
    </tr>
  </table>
   </form>`) ^^ `</blockquote>`)
