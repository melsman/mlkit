(* $Id$ *)

val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (person_id,errs) = 
  ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val (email, errs) = ScsFormVar.getEmailErr( "email", "email", errs)
val _ = ScsFormVar.anyErrors errs

val _ = ScsDb.panicDml `
  update scs_parties 
     set email = ^(Db.qqq email)
   where party_id = ^((Db.qqq o Int.toString) person_id)`

(* we use the big hammer and flushes the cache when changing passwords or 
   nullifying passwords
   because its easier than figuring out the language and new password. *)
val _ = Ns.Cache.flush ScsPersonData.getEmailCache

val _ = ScsConn.returnRedirect "/scs/admin/user/index.sml" [("person_id",Int.toString person_id)]
