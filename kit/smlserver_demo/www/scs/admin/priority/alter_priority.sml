(* $Id$ *)

val _ = ScsLogin.auth_roles [ScsRole.SiteAdm]

val (mode,errs) = ScsPriority.getModeErr ("mode",ScsFormVar.emptyErr)
val (rel_id, errs) = 
  ScsFormVar.getNatErr("rel_id", "Hidden", ScsFormVar.emptyErr)
val (target_url,errs) = 
  ScsFormVar.getStringLenErr 1024 ("target_url", "Hidden", errs)
val _ = ScsFormVar.anyErrors errs

val adjustOrderFn = 
  if mode = ScsPriority.INCREASE_PRIORITY then "increasePriority"
  else if mode = ScsPriority.DECREASE_PRIORITY then "decreasePriority"
  else (*should not come here*) "FEJL"

val adjustOrderSQL = `scs_priority.^(adjustOrderFn)(^(Int.toString rel_id))`

val _ = ScsError.wrapPanic Db.execSp [adjustOrderSQL] ;
      
val _ = ScsConn.returnRedirect target_url

