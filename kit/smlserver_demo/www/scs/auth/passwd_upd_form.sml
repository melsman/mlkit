val _ = ScsPage.returnPg "IT-C.net Password Update" 
  (`<blockquote>
   Write your old password and choose a new password. Then click the update button.<p>` ^^ 
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Password Update" `
   <form action="/scs/auth/passwd_upd.sml" method=post>
   <table>
    <tr><td><b>Current password</b></td>
    <td><input type=text name=cur_passwd size=20></td>
    </tr>
    <tr><td><b>New password</b></td>
    <td><input type=text name=new_passwd size=20></td>
    </tr>
    <tr><td><b>Confirm new password</b></td>
    <td><input type=text name=confirm_new_passwd size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value="Update Password"></td>
    </tr>
  </table>
   </form>`) ^^ `</blockquote>`)
