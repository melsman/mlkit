(* $Id$ *)

val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm]

(* checking form variables *)
val (first_names, errs) = 
	UcsPerson.getFirstNamesErr ("first_names", ScsFormVar.emptyErr)
val (last_name, errs) = UcsPerson.getLastNameErr ("last_name", errs)
val (email, errs) = UcsPerson.getEmailErr ("email", errs)
val (cpr,errs) = UcsPerson.getCprErr ("cpr", errs)
val _ = ScsFormVar.anyErrors errs

fun new_user_trans db = 
  let
    val new_user_id = (Int.toString o UcsData.getOracleIdTrans) db
    val new_user_sp = `
      scs_user.new_proc(
	^(new_user_id),
	scs_random.rand_string(10),
	scs_random.rand_string(40),
	^(Int.toString user_id),
	^(Db.qqq email),
	^(Db.qqq first_names),
	^(Db.qqq last_name),
	^(Db.qqq cpr)
      )    
    `
  in
    Db.Handle.execSpDb db [new_user_sp];
    new_user_id
  end

val new_user_id = ScsError.wrapPanic Db.Handle.dmlTrans new_user_trans

val _ = ScsConn.returnRedirect "index.sml" [("person_id",new_user_id)]
