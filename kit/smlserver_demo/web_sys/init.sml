val _ = Web.log (Web.Notice, "executing init.sml...")
(*val _ = Web.registerTrap "/demo/trap.txt" *)
(*val _ = Web.Info.configSetValue(Web.Info.Type.Int, "SchedulePort", 8040) 
val _ = Web.scheduleDaily "/web/log_time.sml" NONE {hour = 15, minute = 2} 
val _ = Web.scheduleScript "/web/log_time.sml" NONE 20 *)
val _ = Web.Info.configSetValue(Web.Info.Type.String, "MailRelay", "mail.itu.dk") 
(*
val _ = Db.config(Web.Info.Type.Bool, "LazyConnect", true)
val _ = Db.config(Web.Info.Type.String, "UserName", "testuser")
val _ = Db.config(Web.Info.Type.String, "TNSname", "//localhost/test")
val _ = Db.config(Web.Info.Type.String, "PassWord", "test")
val _ = Db.config(Web.Info.Type.Int, "SessionMaxDepth", 3)
val _ = Db.config(Web.Info.Type.Int, "MinimumNumberOfConnections", 4)
val _ = Db.config(Web.Info.Type.Int, "MaximumNumberOfConnections", 10)
*)

local
    fun conf t (k,v) =
	(Web.log (Web.Notice, " Db.config: setting " ^ k);
	 Db.config(t,k,v))
in
   (* Postgresql configuration *)
   val _ = conf Web.Info.Type.String ("DSN", "psql")
   val _ = conf Web.Info.Type.String ("UserName", "mael")
   val _ = conf Web.Info.Type.String ("PassWord", "hi")
   val _ = conf Web.Info.Type.Int ("SessionMaxDepth", 3)
end

val _ = Web.log (Web.Notice, "...done executing init.sml")
