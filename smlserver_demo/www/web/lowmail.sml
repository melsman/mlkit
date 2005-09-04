
val (a,b,c) = List.nth (Web.LowMail.getFQDN_MX "varming.gjk.dk", 0)
val _ = Web.log(Web.Debug, "DNS OK")
val conn = fn () => Web.LowMail.initConn c 
val _ = Web.log(Web.Debug, "Initconn OK")
fun pp (((id,res),(b,0))) = ("<br />This mail was okeyed: " ^ id ^ ", with response: " ^ res ^ b,0)
 |  pp (((id,res),(b,1))) = ("<br />This mail was tempfail: " ^ id ^ ", with response: " ^ res ^ b,1)
 |  pp (((id,res),(b,_))) = ("<br />This mail was permfail: " ^ id ^ ", with response: " ^ res ^ b,2)

fun ss mail = (let 
          val _ = Web.log(Web.Debug, "Sendmail")
          val (ok, tmp, perm) = 
          Web.LowMail.sendmail ([("varming@diku.dk"),("varming@itu.dk")], 
            "varming@gjk.dk", "From: Carsten Varming <varming@acm.org>\r\nTo: CV " ^ 
            "<varming@diku.dk>\r\n.\r\n", mail) 
          val _ = Web.log(Web.Debug, "Sendmail OK")
            val _ = Web.LowMail.closeConn (mail)
          val _ = Web.log(Web.Debug, "connClose OK")
            val (oktext,_) = foldr pp("",0) ok
            val (tmptext,_) = foldr pp ("",1) tmp
            val (permtext,_) = foldr pp ("",2) perm
            in (oktext ^ tmptext ^ permtext) 
            end )
          handle Web.LowMail.ConnectionErr (msg, ok, tmp, perm) => 
           (let 
          val _ = Web.log(Web.Debug, "handling exception")
            val (oktext,_) = foldr pp("",0) ok
            val (tmptext,_) = foldr pp ("",1) tmp
            val (permtext,_) = foldr pp ("",2) perm
            in ("Exception raised: " ^ msg ^ 
                " " ^ oktext ^ tmptext ^ permtext) 
            end)

val _ = Page.return "Results of sending the mail" 
         (Quot.fromString (
           let val (mail,str) = (SOME(conn()),"")
                   handle Web.LowMail.ConnectionErr(msg,_,_,_) => (NONE,"No connection: " ^msg)
           in case mail of NONE => str
                         | SOME(mail') => ss mail'
           end
           handle Web.LowMail.ConnectionErr (s,_,_,_) => "No mail sent:" ^ s))
              
