signature SCS_APPROVAL =
  sig
    type on_what_table = string
    type on_which_id = int
      
   (* [log (on_what_table,on_which_id)] returns HTML for the log of
       all approvals/declines for a particular object in the
       database. Returns NONE if no one exists. *)
    val log : on_what_table * on_which_id -> quot option
  end

structure ScsApproval :> SCS_APPROVAL =
  struct
    type on_what_table = string
    type on_which_id = int

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
