val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]
val (enum_id,errs) = ScsEnum.getEnumerationErr ("enum_id","enumeration",ScsFormVar.emptyErr)
val (text_id,errs) = ScsFormVar.getNatErr("text_id","text_id",errs)
val (text_da,errs) = ScsFormVar.getNonEmptyStringErr("text_da","text_da",errs)
val (text_eng,errs) = ScsFormVar.getNonEmptyStringErr("text_eng","text_eng",errs)
val _ = ScsFormVar.anyErrors errs

val _ = 
  Db.execSp [`scs_text.updateTextProc('^(Int.toString text_id)',
                                      '^(ScsLang.toString ScsLang.da)',
                                      '^(text_da)')`,
	     `scs_text.updateTextProc('^(Int.toString text_id)',
                                      '^(ScsLang.toString ScsLang.en)',
                                      '^(text_eng)')`]

val _ = ScsConn.returnRedirect "edit_enum.sml" [("enum_id",Int.toString enum_id)]
