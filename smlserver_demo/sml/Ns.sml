structure Ns :> NS =
  struct
    type LogSeverity = int
    val Notice=0 and Warning=1 and Error=2 and Fatal=3
    and Bug=4 and Debug=5

    fun log (ls: LogSeverity, s: string) : unit =
      prim("nssml_log", "nssml_log", (ls, s))

    type conn = int
    fun getConn () : conn = 
      prim("nssml_GetConn", "nssml_GetConn", ())

    type status = int        (* see nsthread.h *)
    val OK = 0 and ERROR = ~1 and END_DATA = 4

    type set = int

    structure Conn =
      struct
	fun returnHtml(c: conn, status: int, s: string) : status =
	  prim("nssml_ConnReturnHtml", "nssml_ConnReturnHtml", (c,status,s))
	fun returnRedirect(c: conn, s: string) : status =
	  prim("nssml_ConnReturnRedirect", "nssml_ConnReturnRedirect", (c,s))
	fun getQuery(c: conn) : set option =
	  let val s : set = prim("Ns_ConnGetQuery", "Ns_ConnGetQuery", c)
	  in if (prim("id","id",s) : int) = 0 then NONE
	     else SOME s
	  end
	fun puts(c: conn, s: string) : status =
	  prim("nssml_ConnPuts", "nssml_ConnPuts", (c, s))
	fun setRequiredHeaders(c: conn, contentType: string, contentLength: int) : unit =
	  prim("nssml_ConnSetRequiredHeaders", "nssml_ConnSetRequiredHeaders", (c, contentType, contentLength))
      end

    structure Set =
      struct
	fun get (s :set, key: string): string option =
	  let
	    fun isNull(s : string) : bool = prim("nssml_isNullString", "nssml_isNullString", s)
	    val res : string = prim("nssml_SetGet", "nssml_SetGet", (s,key))
	  in if isNull res then NONE
	     else SOME res
	  end
	fun getOpt (s:set, key:string, dflt:string): string =
	  Option.getOpt(get (s, key), dflt)
	fun put (s: set, key: string, value: string) : unit = 
	  prim("nssml_SetPut", "nssml_SetPut", (s,key,value))
	fun free (s: set) : unit =
	  prim("Ns_SetFree", "Ns_SetFree", s)
	fun create (name: string) : set =
	  prim("nssml_SetCreate", "nssml_SetCreate", name)
	fun size (s: set) : int =
	  prim("nssml_SetSize", "nssml_SetSize", s)
	fun unique (s: set, key: string) : bool =
	  prim("nssml_SetUnique", "nssml_SetUnique", (s,key))
      end

    type db = int
    type poolname = string
    structure Db =
      struct
	fun poolGetHandle (poolname : poolname) : db =
	  prim("nssml_DbPoolGetHandle", "nssml_DbPoolGetHandle", poolname)
	fun poolPutHandle (db : db) : unit =
	  prim("nssml_DbPoolPutHandle", "nssml_DbPoolPutHandle", db)
	fun dmlDb (db : db, s: string) : status =
	  prim("nssml_DbDML", "nssml_DbDML", (db, s))

	fun dml (s: string) : status =
	  let val db = poolGetHandle "main"
	  in dmlDb (db,s) before poolPutHandle db
	  end
	fun select (db : db, s : string) : set =
	  prim("nssml_DbSelect", "nssml_DbSelect", (db, s))
	fun getRow (db : db, s : set) : status =
	  prim("nssml_DbGetRow", "nssml_DbGetRow", (db, s))

	fun foldDb (db:db, f:(string->string)*'a->'a, acc:'a, sql:string) : 'a =
	  let val s : set = select(db, sql)
	    fun g n = Set.getOpt(s, n, "##")
	    fun loop (acc:'a) : 'a =
	      if (getRow(db,s) <> END_DATA) then loop (f(g,acc))
	      else acc
	  in loop acc
	  end

	fun fold (f:(string->string)*'a->'a, acc:'a, sql:string) : 'a =
	  let val db = poolGetHandle "main"
	  in foldDb (db,f,acc,sql) before poolPutHandle db
	  end
      end

    fun return (s : string) : status =
      Conn.returnHtml(getConn(), 200, s)

    fun returnRedirect(s : string) : status =
      Conn.returnRedirect(getConn(), s)      

    fun write (s : string) : status =
      Conn.puts(getConn(), s)

    fun returnHeaders () : unit =
      Conn.setRequiredHeaders(getConn(), "text/html", 0)

    fun pageRoot() : string =
      prim("nssml_PageRoot", "nssml_PageRoot", ())

    fun flattenQuot (fl : string frag list) : string =
      concat(map (fn QUOTE s => s | ANTIQUOTE s => s) fl)
		   
    fun returnQuot (fl : string frag list) : status =
      return (flattenQuot fl)

    fun writeQuot (fl : string frag list) : status =
      write (flattenQuot fl)
      
    val _ = OS.FileSys.chDir (pageRoot())

  end