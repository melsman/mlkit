val print_id = ScsFormVar.wrapPanic ScsError.panic (ScsFormVar.wrapIntAsString ScsFormVar.getNatErr) "print_id"

val (clob_id,doc_type,note,on_what_table,on_what_id) = Db.oneRow' (fn g => (g "clob_id", g "doc_type", g "note",
									    g "on_what_table", g "on_what_id"),
					 `select clob_id,doc_type,note,on_what_table,on_what_id 
                                            from scs_print_log where print_id = ^(Db.qq' print_id)`)

val source = DbClob.select clob_id

val _ =
  ScsPage.returnPg "Show Document"
  (ScsPrint.printForm ("Printed from Log") ("(log id:" ^ print_id ^ ")" ^ note) on_what_table on_what_id 
   (ScsPrint.docTypeFromString doc_type) source)