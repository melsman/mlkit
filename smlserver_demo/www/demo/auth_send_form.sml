val _ = Page.return "Obtain Password by Email"
`You may obtain your password by email by entering 
 your email address below and press the ^`^`Email me 
 my Password'' button.
 <form method=post action=auth_send.sml>
 Email address:
 <input type=text name=email><p>
 <input type=submit value="Email me my Password">
 </form>`