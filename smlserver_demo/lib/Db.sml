
(* For The Oracle User *)
(*
structure Db : NS_DB = Ns.DbOra
val _ = Db.Handle.initPools ["ora_main","ora_sub"]
*)

(* For The PgSQL User *)

structure Db : NS_DB = Ns.DbPg
val _ = Db.Handle.initPools ["pg_main","pg_sub"]

(* For The MySQL User *)
(*
structure Db : NS_DB = Ns.DbMySQL
val _ = Db.Handle.initPools ["mysql_main","mysql_sub"]
*)
