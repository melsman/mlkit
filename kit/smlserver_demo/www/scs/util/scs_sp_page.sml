val user_id = ScsLogin.user_id()

val (sp_id,errs) = ScsFormVar.getNatErr("sp_id","sp_id",ScsFormVar.emptyErr)
val _ = ScsFormVar.anyErrors errs

val _ = ScsShowProgress.returnPage sp_id
