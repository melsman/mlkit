signature SCS_APPROVAL =
  sig
    type on_what_table = string
    type on_which_id = int
    type approval_record = {
      approval_id   : int,
      on_what_table : string,
      on_which_id   : int,
      user_id       : int,
      decision      : bool, 
      note_text     : string,
      created_on    : Date.date,
      last_modified : Date.date,
      modifying_user: int }

    val help_link : unit -> string

    (* [getAllApprovals (on_what_table,on_which_id)] returns a list of all 
	approval rows from DB if they exist.*)
    val getAllApprovals : on_what_table * on_which_id -> approval_record list

    (* [getAllApprovalsFromDbErr ((on_what_table,on_which_id), errs)] fetches 
       all approval rows from DB given a (on_what_table,on_which_id) pair. 
       Fails if the DB call and an error
       message is appended to errs *)
(*    val getAllApprovalsFromDbErr : 
      (on_what_table * on_which_id) * ScsFormVar.errs ->
        (approval_record list) * ScsFormVar.errs 2003-04-07, nh *)

    (* [is_approved_p (on_what_table,on_which_id,user_id) approvals] 
       returns true if user_id has approved on_what_table,on_which_id
       else false *)
    val is_approved_p : (string * int * int) -> (approval_record list) -> bool 

    (* [is_all_approved_p table_id_user_triples approvals] returns true if all
    users in the triple list has approved their on_what_table,on_which_id
    else false *)
    val is_all_approved_p : ((string * int * int) list) -> (approval_record list) -> bool 
    
    (* [log (on_what_table,on_which_id)] returns HTML for the log of
       all approvals/declines for a particular object in the
       database. Returns NONE if no one exists. *)
    val log : on_what_table * on_which_id -> quot option
  end

structure ScsApproval :> SCS_APPROVAL =
  struct
    type on_what_table = string
    type on_which_id = int
    type approval_record = {
      approval_id   : int,
      on_what_table : string,
      on_which_id   : int,
      user_id       : int,
      decision      : bool, 
      note_text     : string,
      created_on    : Date.date,
      last_modified : Date.date,
      modifying_user: int }

    fun help_link () = "help_url"

    fun getAllApprovals (on_what_table,on_which_id) = 
      let
        val approvals_sql = `
 	  select *
	    from scs_approvals
	   where on_what_table = ^( Db.qqq on_what_table )
	     and on_which_id = ^(Int.toString on_which_id )`
        fun f g = {
          approval_id    = (valOf o Int.fromString o g) "approval_id",
	  on_what_table  = g "on_what_table",
	  on_which_id    = (valOf o Int.fromString o g) "on_which_id",
	  user_id        = (valOf o Int.fromString o g) "user_id",
	  decision       =  (valOf o Db.toBool o g) "decision",
	  note_text      = g "note_text",
	  created_on     = (valOf o Db.toDate o g) "created_on",
          last_modified  = (valOf o Db.toDate o g) "last_modified",
          modifying_user = (valOf o Int.fromString o g) "modifying_user"
        }
      in
	ScsError.wrapPanic 
        (Db.list f) approvals_sql
      end

(*
    fun getAllApprovalsFromDbErr ((on_what_table,on_which_id), errs) =
      let 
	val err_msg = ScsDict.s' [
        (ScsLang.da, `Godkendelserne findes 
	              desv&aelig;rre ikke i databasen. Dette er 
		      en fejl, s&aring; henvend dig venligst hos 
	     	      ^(help_link()).`),
        (ScsLang.en, `The approvals does not exist in the 
	    	      database. This is an error, please 
		      contact ^(help_link()).`)]
      in
	case getAllApprovals(on_what_table,on_which_id) of
	    SOME approvals => (approvals,errs)
	  | NONE           => ([], ScsFormVar.addErr( err_msg, errs) )
      end
2003-04-07, nh *)
    fun is_approved_p ((on_what_table:string),(on_which_id:int),(user_id:int)) 
      (approvals:(approval_record list)) =
      let
	fun f (app_record:approval_record) = 
	  (#on_what_table app_record) = on_what_table andalso
	  (#on_which_id app_record) = on_which_id andalso
	  (#user_id app_record) = user_id andalso
	  (#decision app_record) = true
      in
	List.exists f approvals
      end

    fun is_all_approved_p 
      (table_id_user_triples:((string*int*int) list)) (approvals:(approval_record list)) =
      let
        fun f ((on_what_table,on_which_id, user_id),acc) = 
          acc andalso is_approved_p (on_what_table,on_which_id, user_id) approvals
      in 
        List.foldr f true table_id_user_triples
      end


    fun log (on_what_table,on_which_id) =
      let
	val sql =
	  `select user_id,
                  decision,
                  note_text,
                  ^(Db.toTimestampExp "created_on") as created_on_t,
                  scs_person.name(user_id) as name,
                  scs_party.email(user_id) as email
             from scs_approvals
            where on_what_table = ^(Db.qqq on_what_table)
              and on_which_id = ^(Int.toString on_which_id)
            order by created_on_t desc`
	val approve_dict = [(ScsLang.da,`Godkendt af %0`),(ScsLang.en,`Approved by %0`)]
	val decline_dict = [(ScsLang.da,`Afvist af %0`),(ScsLang.en,`Declined by %0`)]
	fun decision_text decision_p name =
	  (if decision_p = "t" then ScsDict.sl approve_dict else ScsDict.sl decline_dict) [name]
	val xs = 
	  ScsError.wrapPanic
	  (Db.list (fn g =>
		    `<li><b>^(ScsDate.ppTimestamp((ScsError.valOf o Db.toTimestamp) (g "created_on_t")))</b>: 
		    ^(decision_text (g "decision") (g "name"))<br>
		    ^(g "note_text")`)) sql
      in
	case xs of
	  [] => NONE
	| xs => SOME(`<ul> ` ^^ (List.foldl (fn (x,acc) => acc ^^ x) `` xs) ^^ ` </ul>`)
      end
  end
