val lang = 
  case ScsFormVar.wrapOpt (ScsFormVar.getEnumErr (map ScsLang.toString ScsLang.all)) "lang" of
    SOME l => ScsLang.fromString l
  | NONE => ScsLang.en

val (mitITU_da, mitITU_en) = (
  ScsDict.getString MitItu.Ressources.mitITU_dict ScsLang.da,
  ScsDict.getString MitItu.Ressources.mitITU_dict ScsLang.en
)

val title = ScsDict.getString [
  (ScsLang.da,`Få password til ^mitITU_da`),
  (ScsLang.en,`Get Password for ^mitITU_en`)
] lang


val body =
  ScsDict.getString 
  [(ScsLang.da,`Indtast din email og du vil modtage dit  password til ^mitITU_da pr. email.`),
   (ScsLang.en,`Write your email address and we will send you your  password for ^mitITU_en by email.`)]
  lang

val _ = ScsPage.returnPg title
  (`<blockquote>
   ^body<p>` ^^ 
   (ScsWidget.namedBox "#ff6600" "#FFFFFF" title `
   <form action="/scs/auth/mail_passwd.sml" method=post>
   <input type="hidden" name="lang" value="^(ScsLang.toString lang)">
   <table>
    <tr><td><b>Email</b></td>
    <td><input type=text name=email size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value="^title"></td>
    </tr>
  </table>
   </form>`) ^^ `</blockquote>`)
