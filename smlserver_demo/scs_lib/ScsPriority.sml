(* $Id$ *)

signature SCS_PRIORITY = 
  sig

    type priority_record = {
      rel_id		   : int,
      on_what_parent_table : string,
      on_which_parent_id   : int,
      on_what_child_table  : string,
      on_which_child_id	   : int,
      priority		   : int
    }

    datatype mode = 
        INCREASE_PRIORITY
      | DECREASE_PRIORITY

    val getModeErr   : string * ScsFormVar.errs -> mode * ScsFormVar.errs
    val modeToString : mode -> string

    val compare : priority_record*priority_record -> order

    val getAllPriorities : string * int * string -> priority_record list

    val newPriorityRelTrans : string * int * string * int -> Db.Handle.db 
      -> int

    val delPriorityRelTrans : string * int * string -> Db.Handle.db -> unit
  end


structure ScsPriority :> SCS_PRIORITY =
  struct

    type priority_record = {
      rel_id		   : int,
      on_what_parent_table : string,
      on_which_parent_id   : int,
      on_what_child_table  : string,
      on_which_child_id	   : int,
      priority		   : int
    }

    datatype mode = 
        INCREASE_PRIORITY
      | DECREASE_PRIORITY

    fun getModeErr (mode_fv,errs) = 
      case ScsFormVar.wrapOpt (ScsFormVar.getStringLenErr 50) mode_fv of
	SOME s => (case s of
	     "increase_priority"    => (INCREASE_PRIORITY,errs)
	   | "decrease_priority"    => (DECREASE_PRIORITY,errs)
	   | _			    => ScsError.panic `ScsPriority.getModeErr:
						      unknown mode`
	)
	| NONE => ScsError.panic `ScsPriority.getModeErr: valOf NONE`

    fun modeToString m = case m of
        INCREASE_PRIORITY => "increase_priority"
      | DECREASE_PRIORITY => "decrease_priority"


    fun getAllPriorities (on_what_parent_table, 
			  on_which_parent_id, on_what_child_table) = 
      let
        fun f g = {
	  rel_id	       = ScsData.gToInt g "rel_id",
	  on_what_parent_table = g "on_what_parent_table",
	  on_which_parent_id   = ScsData.gToInt g "on_which_parent_id",
	  on_what_child_table  = g "on_what_child_table",
	  on_which_child_id    = ScsData.gToInt g "on_which_child_id",
	  priority	       = ScsData.gToInt g "priority"
	}
	val prio_sql = `
	  select rel_id, on_what_parent_table, on_which_parent_id,
		 on_what_child_table, on_which_child_id, priority
	    from scs_priority_rels
	   where on_what_parent_table = ^(Db.qqq on_what_parent_table)
	     and on_which_parent_id   = ^(Int.toString on_which_parent_id)
	     and on_what_child_table  = ^(Db.qqq on_what_child_table)
	   order by priority
	`
      in
        ScsError.wrapPanic (Db.list f) prio_sql
      end

      fun newPriorityRelTrans (on_what_parent_table, on_which_parent_id, 
			       on_what_child_table, on_which_child_id) db = 
	let
	  val rel_id = ScsData.getOracleIdTrans db
	  val priority = Db.Handle.oneFieldDb db 
	    `select max(priority)+1 
	       from scs_priority_rels
	      where on_what_parent_table = ^(Db.qqq on_what_parent_table)
	        and on_which_parent_id   = ^(Int.toString on_which_parent_id)
	        and on_what_child_table  = ^(Db.qqq on_what_child_table)
	`
          val priority = if priority = "" then "1" else priority
	  fun new_prio_sql () = `
	    insert into scs_priority_rels(
	      rel_id, 		    
	      on_what_parent_table,
	      on_which_parent_id,  
	      on_what_child_table, 
	      on_which_child_id,   
	      priority,	    
	      created_on,	    
	      last_modified,	    
	      modifying_user 
	    ) values(
	      ^(Int.toString rel_id),
	      ^(Db.qqq on_what_parent_table),
	      ^(Int.toString on_which_parent_id),
	      ^(Db.qqq on_what_child_table),
	      ^(Int.toString on_which_child_id),
	      ^priority,
	      sysdate,
	      sysdate,
	      ^(Int.toString (ScsLogin.user_id()))
	    )`
	in      
	  Db.Handle.dmlDb db (new_prio_sql());
	  rel_id
	end (* of newPriorityRelTrans *)

      fun delPriorityRelTrans (on_what_parent_table, on_which_parent_id, 
			       on_what_child_table) db = 
	let
	  val del_prio_sql = `
	    delete from scs_priority_rels
	     where on_what_parent_table = ^(Db.qqq on_what_parent_table)
	        and on_which_parent_id   = ^(Int.toString on_which_parent_id)
	        and on_what_child_table  = ^(Db.qqq on_what_child_table)
	  `
	in      
	  Db.Handle.dmlDb db del_prio_sql
	end (* of delPriorityRelTrans *)


    fun compare (prio1:priority_record, prio2:priority_record) = 
      Int.compare( #priority prio1, #priority prio2)

  end
