val (id,errs)   = ScsFormVar.getStringErr("id","column identifier",ScsFormVar.emptyErr)
val (table_name,errs) = ScsFormVar.getStringErr("table_name","table name",errs)
val start_date = ScsFormVar.wrapOpt ScsFormVar.getDateErr "start_date"
val end_date = ScsFormVar.wrapOpt ScsFormVar.getDateErr "end_date"
val limit = ScsFormVar.wrapOpt ScsFormVar.getIntErr "limit"
val _ = ScsFormVar.anyErrors errs

val _ = (ScsPage.returnTop "Audit" ;
	 ScsPage.write `<blockquote>`;
	 (ScsAudit.trail_for_table [] start_date end_date table_name [id] limit);
	 ScsPage.write `</blockquote>`;
	 ScsPage.returnBot())
