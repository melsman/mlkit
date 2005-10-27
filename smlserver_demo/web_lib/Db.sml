(* For The Oracle User *)

(*signature WEB_DB = WEB_DB where type 'a Type = 'a Web.Info.Type.Type*)

structure Db : WEB_DB = DbFunctor(struct
                                  structure DbBackend = Web.DbMySqlBackend
                                  val name = "DefaultDatabase"
                                  end)

(* For The PgSQL User *)
(*
structure Db : NS_DB = Ns.DbPg
val _ = Db.Handle.initPools ["pg_main","pg_sub"]
*)
(* For The MySQL User *)
(*
structure Db : NS_DB = Ns.DbMySQL
val _ = Db.Handle.initPools ["mysql_main","mysql_sub"]
*)
