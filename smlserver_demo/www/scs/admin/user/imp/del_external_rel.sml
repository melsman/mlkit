val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val (person_id,errs) = 
  ScsPerson.getPersonIdErr("person_id",ScsFormVar.emptyErr)
val (on_what_table,errs) = 
  ScsUserImp.getExtSourceErr ("on_what_table",errs)
val (on_which_id,errs) =
  ScsUserImp.getOnWhichIdErr("on_which_id",errs)
val _ = ScsFormVar.anyErrors errs

val _ = 
  ScsError.wrapPanic
  Db.dml `delete from scs_person_rels
           where person_id = '^(Int.toString person_id)'
             and on_what_table = ^(Db.qqq on_what_table)
             and on_which_id = '^(Int.toString on_which_id)'`

val _ = ScsConn.returnRedirect "show_person_with_all_sources.sml" [("person_id",Int.toString person_id)]