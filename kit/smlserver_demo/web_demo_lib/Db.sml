(* Default Db structure *)

structure Db : WEB_DB = DbFunctor(struct
                                  structure DbBackend = Web.DbPostgreSQLBackend
							(* Web.DbOraBackend *)
                                  val name = "DefaultDatabase"
                                  end)
