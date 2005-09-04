structure FV = FormVar
structure LM = Web.LowMail

val input = (Web.log(Web.Debug, "just before FV.getStringErr");
FV.wrapOpt FV.getStringErr "email")

val data = Quot.fromString (
    case input of NONE => ""
               |  SOME(indata) => 
         let val a = String.fields (fn c => c = #"@") indata
             val text = if List.length a <> 2 
                        then String.concat [indata, " Not a valid email address"]
                        else 
                        let 
                        val b = LM.getFQDN_MX (List.nth (a,1))
                        fun bb ((pref,ttl,server),s) = String.concat 
                           ["<br />Priority: ", Int.toString pref, ", Time To Live: ", 
                            Int.toString ttl, ", Server: ",
                            LM.FQDN_MX_toString (server), s]
                        in indata ^ (
                           case List.length b of 0 => " gave no result"
                                | 1 => " gave this result " ^ foldr bb "" b 
                                | _ => " gave these results " ^ foldr bb "" b )
                        end
         in text end
    )

val _ = 
   Page.return "DNS Mail eXchange record lookup example" (`
   Enter an email address:
   <form method=get action=dnsmx.sml>
    <input type=text name=email>
    <input type=submit value="Get MX record">
   </form>` ^^ data 
  )
