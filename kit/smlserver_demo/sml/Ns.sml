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

    fun isNull(s : string) : bool = prim("nssml_isNullString", "nssml_isNullString", s)

    structure Set =
      struct
	fun get (s :set, key: string): string option =
	  let val res : string = prim("nssml_SetGet", "nssml_SetGet", (s,key))
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

	fun key (s: set, index: int) : string option =
	  let val res : string = prim("nssml_SetKey", "nssml_SetKey", (s, index))
	  in if isNull res then NONE
	     else SOME res
	  end

	fun value (s: set, index: int) : string option =
	  let val res : string = prim("nssml_SetValue", "nssml_SetValue", (s, index))
	  in if isNull res then NONE
	     else SOME res
	  end

	fun getPair(s,n) = 
	  case (key(s,n), value(s,n))
	    of (SOME k,SOME v) => (k,v)
	     | _ => raise Fail "Ns.getPair"
	fun foldr (f:(string * string) * 'a -> 'a) (b:'a) (s:set) : 'a =
	  let fun loop (n,acc) = if n < 0 then acc
				 else loop (n-1, f(getPair(s,n),acc))
	  in loop (size s - 1, b)
	  end
	fun foldl (f:(string * string) * 'a -> 'a) (b:'a) (s:set) : 'a =
	  let fun loop (n,acc) = if n < 0 then acc
				 else f(getPair(s,n), loop(n-1,acc))
	  in loop (size s - 1, b)
	  end
	fun list s = foldl (op ::) nil s
	fun filter p s = foldl (fn (pair,a) => if p pair then pair :: a else a) nil s
      end

    structure Conn =
      struct
	fun returnHtml(status: int, s: string) : status =
	  prim("nssml_ConnReturnHtml", "nssml_ConnReturnHtml", (getConn(),status,s))
	fun returnRedirect(s: string) : status =
	  prim("nssml_ConnReturnRedirect", "nssml_ConnReturnRedirect", (getConn(),s))
	fun getQuery() : set option =
	  let val s : set = prim("Ns_ConnGetQuery", "Ns_ConnGetQuery", getConn())
	  in if s = 0 then NONE
	     else SOME s
	  end
	fun formvar s = case getQuery()
			  of SOME set => Set.get(set,s)
			   | NONE => NONE
	fun headers() : set =
	  prim("Ns_ConnHeaders", "Ns_ConnHeaders", getConn())

	fun host() : string =
	  prim("nssml_ConnHost", "nssml_ConnHost", getConn())

	fun location() : string =
	  let val res : string = prim("nssml_ConnLocation", "nssml_ConnLocation", getConn())
	  in if isNull res then "<unknown>"
	     else res
	  end

	fun peer() : string =
	  prim("nssml_ConnPeer", "nssml_ConnPeer", getConn())

	fun peerPort() : int =
	  prim("Ns_ConnPeerPort", "Ns_ConnPeerPort", getConn())

	fun port() : int =
	  prim("Ns_ConnPort", "Ns_ConnPort", getConn())

	fun redirect(url: string) : status =
	  prim("nssml_ConnRedirect", "nssml_ConnRedirect", (getConn(),url))

	fun server() : string =
	  prim("nssml_ConnServer", "nssml_ConnServer", getConn())

	fun puts(s: string) : status =
	  prim("nssml_ConnPuts", "nssml_ConnPuts", (getConn(), s))

	fun setRequiredHeaders(contentType: string, contentLength: int) : unit =
	  prim("nssml_ConnSetRequiredHeaders", "nssml_ConnSetRequiredHeaders", (getConn(), contentType, contentLength))

	fun url () : string =
	  prim("nssml_ConnUrl", "nssml_ConnUrl", getConn())
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
	  in (dmlDb (db,s) before poolPutHandle db)
	    handle X => (poolPutHandle db; raise X)
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
	  in (foldDb (db,f,acc,sql) before poolPutHandle db)
	    handle X => (poolPutHandle db; raise X)
	  end
      end

    structure Info =
      struct
	fun configFile() : string =
	  prim("nssml_InfoConfigFile", "nssml_InfoConfigFile", ())

	fun configGetValue {sectionName: string, key: string} : string option =
	  let val res : string = prim("nssml_configGetValue", "nssml_configGetValue", 
				      (sectionName, key))
	  in if isNull res then NONE
	     else SOME res
	  end

	fun configGetValueExact {sectionName: string, key: string} : string option =
	  let val res : string = prim("nssml_configGetValueExact", "nssml_configGetValueExact", 
				      (sectionName, key))	  
	  in if isNull res then NONE
	     else SOME res
	  end
	    
	fun errorLog() : string =
	  prim("nssml_InfoErrorLog", "nssml_InfoErrorLog", ())

	fun homePath() : string =
	  prim("nssml_InfoHomePath", "nssml_InfoHomePath", ())

	fun hostname() : string =
	  prim("nssml_InfoHostname", "nssml_InfoHostname", ())

	fun pid() : int =
	  prim("Ns_InfoPid", "Ns_InfoPid", ())

	fun serverVersion() : string =
	  prim("nssml_InfoServerVersion", "nssml_InfoServerVersion", ())

	fun uptime() : int =
	  prim("Ns_InfoUptime", "Ns_InfoUptime", ())
	  
	fun pageRoot() : string =
	  prim("nssml_PageRoot", "nssml_PageRoot", ())

      end

    fun return (s : string) : status =
      Conn.returnHtml(200, s)

    fun returnRedirect(s : string) : status =
      Conn.returnRedirect s      

    fun write (s : string) : status = Conn.puts s

    fun returnHeaders () : unit =
      Conn.setRequiredHeaders("text/html", 0)

    fun getMimeType(s: string) : string =
      prim("nssml_GetMimeType", "nssml_GetMimeType", s)

    fun getHostByAddr(s: string) : string option =
      let val res : string = prim("nssml_GetHostByAddr", "nssml_GetHostByAddr", s)
      in if isNull res then NONE
	 else SOME res
      end

    fun encodeUrl(s: string) : string =
      prim("nssml_EncodeUrl", "nssml_EncodeUrl", s)

    fun decodeUrl(s: string) : string =
      prim("nssml_DecodeUrl", "nssml_DecodeUrl", s)

    structure Quot =
      struct
	fun flatten (fl : string frag list) : string =
	  concat(map (fn QUOTE s => s | ANTIQUOTE s => s) fl)
	  
	val return = fn fl => return (flatten fl)
	  
	val write = fn fl => write (flatten fl)
      end

    structure Mail =
      struct
	fun sendmail {to: string list, cc: string list, bcc: string list,
		      from: string, subject: string, body: string,
		      extra_headers: string list} : unit =
	  let
	    fun sl2s sep [] = ""
              | sl2s sep l = concat (tl (foldr (fn (s,acc)=>sep::s::acc) [] l))	
	    fun header s nil = ""
	      | header s l = s ^ ": " ^ sl2s "," l ^ "\n"
	    val mails = concat
	      ["From: ", from, "\n",
	       "To: ", sl2s "," to, "\n",
	       header "Cc" cc,
	       header "Bcc" bcc,   (* stripped by sendmail before sending! *)
	       concat(map (fn s => s ^ "\n") extra_headers),
	       "Subject: ", subject, "\n\n",
	       body]
	    fun writeFile (filename, str) = 
	      let val os = TextIO.openOut filename
	      in (TextIO.output (os, str); TextIO.closeOut os)
		handle X => (TextIO.closeOut os; raise X)
	      end
	    val tmpf = FileSys.tmpName()
	    val cmd = "/usr/sbin/sendmail -t < " ^ tmpf
	  in (writeFile (tmpf, mails);
	      if OS.Process.system cmd = OS.Process.success then ()
	      else raise Fail "")
	    handle X => 
	      (FileSys.remove tmpf;
	       raise Fail ("Failed to send email from " ^ from ^ " using Ns.sendmail."))
	  end

	fun send {to: string, from: string, subject: string, body: string} : unit =
	  sendmail {to=[to],from=from,cc=nil,bcc=nil,subject=subject,
		    extra_headers=nil,body=body}
      end

    fun exit() = raise Interrupt  
    (* By raising Interrupt, the web-server is not killed as it
     * would be if we call OS.Process.exit. Also, handlers can
     * protect the freeing of resources such as file descriptors
     * and database handles. Moreover, region pages are freed as 
     * they should be. *)

    val _ = OS.FileSys.chDir (Info.pageRoot())

  end