val user_id = ScsLogin.auth()

val _ = ScsPage.returnPg "IT-C.net Password Update" 
  (`<h1>Update password for ^(ScsPerson.name user_id)</h1>
   <blockquote>
   Write your old password and choose a new password. Then click the update button.<p>` ^^ 
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" "Password Update" `
   <form action="/scs/auth/passwd_upd.sml" method=post>
   <table>
    <tr><td><b>Current password</b></td>
    <td><input type=password name=cur_passwd size=20></td>
    </tr>
    <tr><td><b>New password</b></td>
    <td><input type=password name=new_passwd size=20></td>
    </tr>
    <tr><td><b>Confirm new password</b></td>
    <td><input type=password name=confirm_new_passwd size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value="Update Password"></td>
    </tr>
  </table>
   </form>`) ^^ `</blockquote>`)
