signature SCS_DB =
  sig
    val dbClickDml    : string -> string -> string -> string -> quot -> unit
    val panicDml      : quot -> unit
    val errorDml      : quot -> quot -> unit

    val oneFieldErrPg : quot * quot -> string
    val oneRowErrPg'  : (((string -> string)->'a) * quot * quot) -> 'a
  end

structure ScsDb :> SCS_DB =
  struct
    fun dbClickDml table_name id_column_name generated_id return_url insert_sql =
      if Db.dml insert_sql = Ns.OK then
        (Ns.returnRedirect return_url; Ns.exit())
      else if Db.existsOneRow `select 1 as num from ^table_name where ^id_column_name = '^(Db.qq generated_id)'` 
	     then (Ns.returnRedirect return_url; Ns.exit()) (* it's a double click, so just redirect the user to the index page *)
	   else ScsError.panic (`DbFunctor.dbClickDml choked. DB returned error on SQL ` ^^ insert_sql)
	     handle X => ScsError.panic (`DbFunctor.dbClickDml choked. DB returned error on SQL ` ^^ insert_sql ^^ `^(General.exnMessage X)`)

    val panicDml = Db.panicDml ScsError.panic 
    fun errorDml emsg sql = (Db.errorDml (fn () => (Ns.log (Ns.Notice, "hej");ScsPage.returnPg "Databasefejl" emsg)) sql;())

    fun oneRowErrPg' (f,sql,emsg) =
      Db.oneRow' (f,sql) handle _ => (ScsPage.returnPg "" emsg;Ns.exit())

    fun oneFieldErrPg (sql,emsg) =
      Db.oneField sql handle _ => (ScsPage.returnPg "" emsg;Ns.exit())
  end