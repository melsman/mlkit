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

    (* [getPriority rel_id] returns a record from DB if exists;
        otherwise returns NONE *)
    val getPriority : int -> priority_record option

    (* [getPriorityFromDbErr (rel_id,errs)] returns a 
	priority_record option and adds an error message if it is NONE *)
    val getPriorityFromDbErr : int * ScsFormVar.errs 
      -> (priority_record option) * ScsFormVar.errs

    (* [getAllPriorities (on_what_parent_table, on_which_parent_id, 
          on_what_child_table)] returns a list of records from DB *)
    val getAllPriorities : string * int * string -> priority_record list

    val newPriorityRelTrans : string * int * string * int -> Db.Handle.db 
      -> int

    val delPriorityRelTrans : string*int*string*int -> Db.Handle.db -> unit
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


    local
        fun f g = {
	  rel_id	       = ScsData.gToInt g "rel_id",
	  on_what_parent_table = g "on_what_parent_table",
	  on_which_parent_id   = ScsData.gToInt g "on_which_parent_id",
	  on_what_child_table  = g "on_what_child_table",
	  on_which_child_id    = ScsData.gToInt g "on_which_child_id",
	  priority	       = ScsData.gToInt g "priority"
	}
	val pre_prio_sql = `
	  select rel_id, on_what_parent_table, on_which_parent_id,
		 on_what_child_table, on_which_child_id, priority
	    from scs_priority_rels
	`
    in
      fun getPriority rel_id = 
	let
	  val post_prio_sql = `
	     where rel_id = ^(Int.toString rel_id)
	  `
	in
	  SOME( Db.oneRow' f (pre_prio_sql ^^ post_prio_sql) )
	  handle _ => NONE
	end

      fun getPriorityFromDbErr (rel_id, errs) =
        let
	  val err_msg = ScsDict.s' (UcsDict.fromDbErrMsg UcsDict.priority_dict)
	in
	  case getPriority rel_id of 
	      NONE	 => (NONE, ScsFormVar.addErr(err_msg,errs))
	    | SOME prio  => (SOME prio, errs)
	end

      fun getAllPriorities (on_what_parent_table, 
			    on_which_parent_id, on_what_child_table) = 
	let
	  val post_prio_sql = `
	     where on_what_parent_table = ^(Db.qqq on_what_parent_table)
	       and on_which_parent_id   = ^(Int.toString on_which_parent_id)
	       and on_what_child_table  = ^(Db.qqq on_what_child_table)
	     order by priority
	  `
	in
	  ScsError.wrapPanic (Db.list f) (pre_prio_sql ^^ post_prio_sql)
	end
    end (* of local *)

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
			       on_what_child_table, on_which_child_id) db = 
	let
	  val del_prio_sql = `
	    delete from scs_priority_rels
	     where on_what_parent_table = ^(Db.qqq on_what_parent_table)
	        and on_which_parent_id   = ^(Int.toString on_which_parent_id)
	        and on_what_child_table  = ^(Db.qqq on_what_child_table)
		and on_which_child_id	 = ^(Int.toString on_which_child_id)
	  `
	in      
	  Db.Handle.dmlDb db del_prio_sql
	end (* of delPriorityRelTrans *)


    fun compare (prio1:priority_record, prio2:priority_record) = 
      Int.compare( #priority prio1, #priority prio2)

  end
