structure FV = FormVar

val _ = 
  if Login.person_id = Login.default_id then 
    (Ns.returnRedirect "/demo/auth_form.sml";
     Ns.exit())
  else ()

val url  = FV.wrapFail FV.getUrlErr ("url", "URL")
val text = FV.wrapFail FV.getStringErr ("text", "Text")

val insert = 
  `insert into link (link_id, person_id, url, text)
   values (^(Db.seqNextvalExp "link_seq"),
	   ^(Int.toString Login.person_id),
	   ^(Db.qq' url),
	   ^(Db.qq' text))`

val _ = Db.dml insert

val _ = Ns.returnRedirect "index.sml"
