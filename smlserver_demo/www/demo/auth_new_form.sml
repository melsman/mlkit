val _ = Page.return "Register at SMLserver.org" `
Enter your <b>email address</b>, <b>name</b>, 
and <b>home page address</b>.
<form action="/demo/auth_new.sml" method=post>
<table>
 <tr><td><b>Email address</b></td>
     <td><input type=text name=email size=40></td>
 </tr>
 <tr><td><b>Name</b></td>
     <td><input type=text name=name size=40>
     </td>
 </tr>
 <tr><td><b>Home Page URL</b></td>
     <td><input type=text name=url size=40>
     </td>
 </tr>
 <tr><td colspan=2 align=center>
        <input type=submit value="Register">
     </td>
 </tr>
</table>
</form>
When you register, a password is sent to you by email.`
