signature SCS_AUDIT =
  sig
    (* This module is based on the Audit package found in ACS version 3.4.4 (www.arsdigita.com) *)

    (* [trail columns_not_reported start_date end_date table_name ids limit] returns a HTML fragment
       showing all changes between start_date and end_date on the rows in table table_name
       identified by the keys ids (a list of pairs with key name and key value) *)
    val trail : string list -> Date.date option -> Date.date option -> string -> (string*string) list -> quot

    val trail_for_table : string list -> Date.date option -> Date.date option 
                          -> string -> string list -> int option -> unit
  end

structure ScsAudit :> SCS_AUDIT =
  struct
    fun trail_columns columns_not_reported audit_count ((column:string,value:string),
							(acc:quot,old_values:(string,string) Splaymap.dict,
							 modification_count:int)) =
      if List.exists (fn s => ScsString.lower s = ScsString.lower column) columns_not_reported then
	(acc,old_values,modification_count)
      else
	let
	  val (audit_entry,modification_count) =
	    if audit_count > 0 then (* This is not an insert *)
	      case Splaymap.peek (old_values,column) of 
		NONE => (* Added new column to existing table *)
		  (`<tr><td  valign=top>Added ^column:</td>
		   <td>^(Quot.wrapString Html.htmlencode value)</td></tr>`,modification_count+1)
	      | SOME old_value =>
		  if value <> old_value then (* Column has changed *)
		    if old_value <> "" then
		      (`<tr><td  valign=top>Modified ^column:</td>
		       <td>^(Quot.wrapString Html.htmlencode value)</td></tr>`,modification_count+1)
		    else 
		      (`<tr><td  valign=top>Added ^column:</td>
		       <td>^(Quot.wrapString Html.htmlencode value)</td></tr>`,modification_count+1)
		  else (* Column has not changed *)
		    (``,modification_count)
	    else (* This is an insert *)
	      if value <> "" then
		(`<tr><td  valign=top>Added ^column:</td>
		 <td>^(Quot.wrapString Html.htmlencode value)</td></tr>`,modification_count)
	      else 
		(``,modification_count)
	in
	  (acc ^^ audit_entry,Splaymap.insert(old_values,column,value),modification_count)
	end

    fun trail_row (columns_not_reported:string list) 
      (s:Ns.Set.set,(acc:quot,old_values:(string,string) Splaymap.dict,audit_count:int)) : 
      (quot * (string,string) Splaymap.dict * int) =
      let
	(* Loop through each column key and value in the selection *)
	fun g n = Db.getCol s n (* delete_p does not exist for the main table *)
	fun show_user () = g "modifying_user_name" ^ "(" ^ (g "last_modifying_user") ^ ")"
	val trail_columns = trail_columns columns_not_reported audit_count
      in
	if g "delete_p" = "t" then
	  (* Entry in the audit table is for a deleted row. Set the audit entry to a single line *)
	  (acc ^^ `<h4>Delete on ^(g "last_modified") by ^(show_user())</h4>`,
	   Splaymap.mkDict String.compare:(string,string) Splaymap.dict,0)
	else
	  let val (acc,old_values,modification_count) =
	    if audit_count = 0 then (* No previous audit entry for this row *)
	      Ns.Set.foldl trail_columns (acc ^^ `<h4>Insert on ^(g "last_modified") by ^(show_user())
					  </h4><table>`,old_values,0) s
	    else (* This audit entry represents an update to the main row *)
	      Ns.Set.foldl trail_columns (acc ^^ `<h4>Update on ^(g "last_modified") by ^(show_user())
					  </h4><table>`,old_values,0) s
	  in 
	    (acc ^^ `</table>`,old_values,audit_count+1) 
	  end
      end

    (* The id pairs (id,val) must correspond to the compound key in table table_name *)
    fun trail columns_not_reported start_date end_date table_name ids =
      let
	val audit_table_name = table_name ^ "_audit"

        (* These values will be part of an audit entry description
           and do not need to be reported seperately *)
	val columns_not_reported = "modifying_user_name" :: "last_modifying_user" :: 
	  "last_modified" :: "delete_p" :: columns_not_reported

	val wh = case end_date of
	  NONE => `1=1 `
	| SOME d => `last_modified < ^(Db.fromDate d) `

        (* The first record displayed may not represent an insert if
           start_date is not empty. So display the first record as an
           update, if start_date is not empty. *)
	val (wh,audit_entry) = 
	  case start_date of
	    NONE => (wh,0)
	  | SOME d => (wh ^^ ` and last_modified > ^(Db.fromDate d)`,1)

        (* Generate main and audit table restrictions for ids *)
	val wh_main = List.foldr (fn ((id,v),wh) => wh ^^ ` and ^table_name.^id = '^(v)'`) wh ids
	val wh_audit = List.foldr (fn ((id,v),wh) => wh ^^ ` and ^audit_table_name.^id = '^(v)'`) wh ids

	(* Get the entries in the audit table *)
        val sql = `
	  select ^audit_table_name.*,
   	         to_char(^audit_table_name.last_modified,'Mon DD, YYYY HH12:MI AM') as last_modified,
                 auth_user.name as modifying_user_name
            from ^audit_table_name, auth_user
           where auth_user.user_id(+) = ^audit_table_name.last_modifying_user
             and ` ^^ wh_audit ^^ `
           order by ^audit_table_name.last_modified asc`

	(*val _ = Ns.log(Ns.Notice,Quot.toString (`AUDIT SQL: ` ^^ sql))*)

	val (audit_html:quot,old_values:(string,string)Splaymap.dict,audit_count:int) = 
	  Db.foldSet (trail_row columns_not_reported)
		      (``:quot, Splaymap.mkDict String.compare:(string,string) Splaymap.dict,audit_entry) sql

	val sql = `
	  select ^table_name.*,
                 to_char(^table_name.last_modified,'Mon DD, YYYY HH12:MI AM') as last_modified,
                 auth_user.name as modifying_user_name
            from ^table_name, auth_user
           where auth_user.user_id(+) = ^table_name.last_modifying_user
             and ` ^^ wh_main ^^ `
           order by ^table_name.last_modified asc`

	(*val _ = Ns.log(Ns.Notice,Quot.toString (`MAIN SQL: ` ^^ sql))*)

	val (all_html:quot,old_values:(string,string)Splaymap.dict,audit_count:int) = 
	  Db.foldSet (trail_row columns_not_reported) (audit_html,old_values,audit_count) sql
      in
	all_html
      end

    (* The id pairs (id,val) must correspond to the compound key in table table_name *)
    fun trail_for_table columns_not_reported start_date end_date table_name id_columns limit =
      let
	val audit_table_name = table_name ^ "_audit"

	val wh = case end_date of
	  NONE => `1=1 `
	| SOME d => `last_modified < ^(Db.fromDate d) `

	val wh = 
	  case start_date of
	    NONE => wh
	  | SOME d => wh ^^ ` and last_modified > ^(Db.fromDate d)`

	val wh =
	  case limit of
	    NONE => wh
	  | SOME l => wh ^^ ` and rownum < '^(Int.toString l)'`

        (* Generate SQL for main and audit table restrictions for ids *)
	val ids = String.concatWith ", " id_columns
	val sql_main = `select distinct ^ids from ^table_name where ` ^^ wh
	val sql_audit = `select distinct ^ids from ^audit_table_name where delete_p = 't' and ` ^^ wh
      in
	List.app (fn (heading,sql) => 
		  (ScsPage.write `<h2>^heading</h2>`;
		   Db.app (fn g => 
			   let
			     val id_vals = List.map (fn id => (id,g id)) id_columns
			     val vals = List.map #2 id_vals
			     val in_link=String.concatWith "&" 
			       (List.map (fn (id,v) => (*ids^*)"id="^id^"&id_"^id^"="^v) id_vals)

			   in
			     ScsPage.write (`<h4><a href="audit_row.sml?table_name=^(table_name)&^(in_link)">Key (^ids)=(^(String.concatWith ", " vals))</a></h4>` ^^ 
					    trail columns_not_reported start_date end_date table_name id_vals)
			   end) sql))
	[("Modified rows",sql_main),("Deleted rows",sql_audit)]
      end
  end

(*

proc_doc ad_audit_delete_row { id_list id_column_list audit_table_name } "Inserts an entry to the audit table to log a delete. Each id is inserted into its id_column as well as user_id, IP address, and date." {

    # VARIABLES
    # audit_table_name - table that holds the audit records
    # id_column_list - column names of the primary key(s) in 
    #      audit_table_name 
    # id_list -  ids of the record you are processing

    set id_column_join [join $id_column_list ", "]
    set id_column_bind_join ":[join $id_column_list ", :"]"

    # Create the bind variables
    set bind_vars [ns_set create]

    for { set i 0 } { $i < [llength $id_column_list] } { incr i } {
	ns_set put $bind_vars [lindex $id_column_list $i] [lindex $id_list $i]
    }
    # Add the final audit columns to the ns_set
    ns_set put $bind_vars last_modifying_user [ad_get_user_id]
    ns_set put $bind_vars modified_ip_address [ns_conn peeraddr]
    ns_set put $bind_vars delete_p t

    db_dml audit_table_insert "insert into $audit_table_name
    ($id_column_join, last_modified, 
    last_modifying_user, modified_ip_address, delete_p)
    values
    ($id_column_bind_join, sysdate, 
    :last_modifying_user, :modified_ip_address, :delete_p)
    " -bind $bind_vars

    ns_set free $bind_vars

}au*)