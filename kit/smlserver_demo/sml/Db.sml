
(* For The Oracle User *)
(*structure Db : DB = Ns.DbOra
val _ = Db.Pool.initPoolsL ["ora_main","ora_sub"]*)

(* For The PgSQL User *)
structure Db : NS_DB = Ns.DbPg
val _ = Db.Pool.initPoolsL ["pg_main","pg_sub"]
