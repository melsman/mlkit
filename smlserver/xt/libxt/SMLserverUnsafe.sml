structure SMLserverUnsafe : SMLSERVER_UNSAFE =
  struct
    open NsBasics

    exception MissingConnection
    type conn = int

    local
	fun getConn0 () : conn = 
	    prim("@Ns_TclGetConn", (0:int))
	type LogSeverity = int
	val Notice=0 and Warning=1 and Error=2 and Fatal=3
	and Bug=4 and Debug=5    
	fun log' (ls: LogSeverity, s: string) : unit =
	    prim("@Ns_Log", (ls, s))
	fun log s = log' (Notice, s)
    in
	fun getConn() : conn =
	    let val c = getConn0()
	    in if c = 0 then 
		(log "SMLserverUnsafe: missing connection";
		 raise MissingConnection)
	       else c
	    end
    end

    fun write(s: string) : status =
	prim("@Ns_ConnPuts", (getConn(), s))

    fun returnFileMime (mimetype:string) (file:string) : status =
      prim("nssml_returnFile", (getConn(),mimetype,file))

    fun mimeType(s: string) : string =
      prim("nssml_GetMimeType", s)

    fun returnFile (file:string):status =
      returnFileMime (mimeType file) file

    local
	fun getQuery() : NsSet.set option =
	    let val s : NsSet.set = prim("@Ns_ConnGetQuery", getConn())
	    in if s = 0 then NONE
	       else SOME s
	    end
    in
	fun formvar s = 
	    case getQuery() of 
		SOME set => NsSet.get(set,s)
	      | NONE => NONE
	fun formvarAll s = 
	  case getQuery() of 
	      SOME set => NsSet.getAll(set,s)
	    | NONE => []
    end

    fun registerTrap (u:string) : unit = 
      prim("nssml_registerTrap", u)

    fun scheduleScript (f: string) (i:int) : unit =
      prim("nssml_scheduleScript", (f,i))	

    fun scheduleDaily (f:string) {hour:int,minute:int} : unit =
      prim("nssml_scheduleDaily", (f,hour,minute))	

    fun scheduleWeekly (f:string) {day:int,hour:int,minute:int} : unit =
      prim("nssml_scheduleWeekly", (f,day,hour,minute))	

    structure Form = SMLserverFormUnsafe
  end
    
