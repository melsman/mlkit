val user_id = ScsLogin.auth()

val title = ScsDict.s [(ScsLang.en,`IT-C.net Password Update`),(ScsLang.da,`IT-C.net Opdatering af Kodeord`)]

val _ = ScsAuth.returnPg title
  (`<h1>^(ScsDict.s [(ScsLang.en,`Update password for`),(ScsLang.da,`Opdater kodeord for`)]) ^(ScsPerson.name user_id)</h1>
   <blockquote>
   ^(ScsDict.s [(ScsLang.en,`Write your old password and choose a new password. Then click the update button.`),
		(ScsLang.da,`Skriv dit gamle kodeord og vælg et nyt. Klik derefter Opdater`)])<p>` ^^ 
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" (ScsDict.s [(ScsLang.en,`Password Update`),
						       (ScsLang.da,`Opdatering af kodeord`)]) `
   <form action="/scs/auth/passwd_upd.sml" method=post>
   <table>
    <tr><td><b>^(ScsDict.s [(ScsLang.en,`Current password`),
			    (ScsLang.da,`Nuværende kodeord`)])</b></td>
    <td><input type=password name=cur_passwd size=20></td>
    </tr>
    <tr><td><b>^(ScsDict.s [(ScsLang.en,`New password`),
			    (ScsLang.da,`Nyt kodeord`)])</b></td>
    <td><input type=password name=new_passwd size=20></td>
    </tr>
    <tr><td><b>^(ScsDict.s [(ScsLang.en,`Confirm new password`),
			    (ScsLang.da,`Bekræft nyt kodeord`)])</b></td>
    <td><input type=password name=confirm_new_passwd size=20></td>
    </tr>
    <tr><td colspan=2 align=center>
      <input type=submit value="^(ScsDict.s [(ScsLang.en,`Update Password`),(ScsLang.da,`Opdater kodeord`)])"></td>
    </tr>
  </table>
   </form>`) ^^ `</blockquote>`)
