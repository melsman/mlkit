val (table_name,errs) = ScsFormVar.getStringErr("table_name","table name",ScsFormVar.emptyErr)
val ids = ScsFormVar.getStrings "id"
val (id_vals,errs) = List.foldl (fn (id,(id_vals,errs)) => 
				 let
				   val (id_v,errs) = ScsFormVar.getStringErr("id_"^id,"column value",errs)
				 in
				   ((id,id_v)::id_vals,errs)
				 end) ([],errs) ids
val start_date = ScsFormVar.wrapOpt ScsFormVar.getDateErr "start_date"
val end_date = ScsFormVar.wrapOpt ScsFormVar.getDateErr "end_date"
val column_names = ScsFormVar.getStrings "column_names"
val _ = ScsFormVar.anyErrors errs

val columns_in_table = Db.list (fn g => g "column_name", `select column_name 
                                                     from user_tab_columns 
                                                    where table_name='^(table_name)'`)

val columns_not_reported = 
  case column_names of
    [] => []
  | xs => 
      List.filter (fn c_i_t =>  
		   not (List.exists (fn c => ScsString.upper(c) = ScsString.upper(c_i_t)) column_names)) 
      columns_in_table

val _ = List.app (fn t => Ns.log(Ns.Notice, t)) columns_not_reported

val _ = (ScsPage.returnPg "Audit" 
	 (ScsAudit.trail columns_not_reported start_date end_date table_name id_vals))
