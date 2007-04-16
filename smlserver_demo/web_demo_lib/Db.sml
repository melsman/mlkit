(* Default Db structure *)

structure Db = DbFunctor(struct
                           structure DbBackend = Web.DbPostgreSQLBackend
						(* Web.DbOraBackend *)
                           val name = "DefaultDatabase"
                         end)
