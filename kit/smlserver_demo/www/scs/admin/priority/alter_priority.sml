(* $Id$ *)

val (mode,errs) = ScsPriority.getModeErr ("mode",ScsFormVar.emptyErr)
val (rel_id, errs) = 
  ScsFormVar.getNatErr("rel_id", "Hidden", errs)
val (target_url,errs) = 
  ScsFormVar.getStringLenErr 1024 ("target_url", "Hidden", errs)
val _ = ScsFormVar.anyErrors errs

val (prio_opt,errs) = ScsPriority.getPriorityFromDbErr (rel_id,errs)
val _ = ScsFormVar.anyErrors errs

val prio = valOf prio_opt

val roles = 
  if #on_what_parent_table prio = "ucs_ob_applications" then [ScsRole.StudAdm]
  else if #on_what_child_table prio = "ucs_cb_sign_ons" then 
    let
      val user_id = ScsLogin.auth() 
      val sign_on_id = #on_which_child_id prio
      val (sign_on_opt,errs) = UcsCb.Data.getSignOnFromDbErr (sign_on_id,errs)
      val _ = ScsFormVar.anyErrors errs
      val sign_on = ScsError.valOf sign_on_opt

      val (course_opt,errs) = UcsCb.Data.getCourseFromDbErr (#course_id sign_on,errs)
      val _ = ScsFormVar.anyErrors errs
      val course = (ScsError.valOf' "course_opt") course_opt
      val (semester_opt,errs) = 
	UcsCb.Data.getSemesterFromDbErr (#semester_id course,errs)
      val _ = ScsFormVar.anyErrors errs
      val sem = (ScsError.valOf' "semester_opt") semester_opt

      val enr_opt = UcsEdu.Data.getEnrolment (#on_which_parent_id prio)
      val _ = ScsFormVar.anyErrors errs
      val enr = (ScsError.valOf' "enr_opt") enr_opt

      (* access control *)
      val errs = UcsCb.Priv.auth_may_delete_sign_on((user_id,sign_on,sem,enr),errs)
      val _ = ScsFormVar.anyErrors errs
    in
      []
    end	
  else [ScsRole.SiteAdm]

val _ = ScsLogin.auth_roles roles

val adjustOrderFn = 
  if mode = ScsPriority.INCREASE_PRIORITY then "increasePriority"
  else if mode = ScsPriority.DECREASE_PRIORITY then "decreasePriority"
  else (*should not come here*) "FEJL"

val adjustOrderSQL = `scs_priority.^(adjustOrderFn)(^(Int.toString rel_id))`

val _ = ScsError.wrapPanic Db.execSp [adjustOrderSQL] ;
      
val _ = Ns.returnRedirect target_url

