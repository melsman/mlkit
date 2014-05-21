fun unfold NONE = SOME ({to = ["varming@diku.dk","varming@itu.dk"], from = "varming@acm.org",
                    subject = "Testing mails", cc = [], bcc = [], body = "Hej nu tester vi 5\r\n.ssd" ^ ((String.str o chr) 163),
                    extra_headers = []}, SOME (), Web.Mail.ISO88591)
  | unfold (SOME _) = NONE

fun fail (_,l,b) = l @ b

val (_,b) = Web.Mail.mail unfold fail NONE []

fun ppfail pf sf (c,d) = pf ^ "Address: " ^ c ^ " failed with message: " ^ d ^ sf

val _ = Page.return "Results of sending the mail" 
         (Quot.fromString (String.concat (map (ppfail "<br />" "") b)))
