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

    fun isNull(s : string) : bool = prim("nssml_isNullString", "nssml_isNullString", s)

    structure Set : NS_SET = NsSet 
    type set = Set.set
 
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

    structure Cache =
      struct
	type cache = int
	fun find (n : string) : cache option =
	  let val res : int = prim("nssml_CacheFind", "nssml_CacheFind", n)
	  in if res = 0 then NONE
	     else SOME res
	  end
	fun create(n : string, t: int) : cache =
	  prim("nssml_CacheCreate", "nssml_CacheCreate", (n,t))
	fun createSz(n : string, sz: int) : cache =  (* sz is in bytes *)
	  prim("nssml_CacheCreateSz", "nssml_CacheCreateSz", (n,sz))
	fun flush(c:cache) : unit =
	  prim("Ns_CacheFlush", "Ns_CacheFlush", c)
	fun set(c:cache, k:string, v:string) : bool =
	  let val res : int = prim("nssml_CacheSet", "nssml_CacheSet", (c,k,v))
	  in res = 1
	  end
	fun get(c:cache, k:string) : string option =
	  let val res : string = prim("nssml_CacheGet", "nssml_CacheGet", (c,k))
	  in if isNull res then NONE
	     else SOME res
	  end
      end

    structure Info : NS_INFO = NsInfo

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

        val op ^^ = op @
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

    fun fetchUrl (url : string) : string option =
      let val res:string = prim("nssml_FetchUrl", "nssml_FetchUrl", url)
      in if isNull res then NONE else SOME res
      end

    fun exit() = raise Interrupt  
    (* By raising Interrupt, the web-server is not killed as it
     * would be if we call OS.Process.exit. Also, handlers can
     * protect the freeing of resources such as file descriptors
     * and database handles. Moreover, region pages are freed as 
     * they should be. *)

    val _ = OS.FileSys.chDir (Info.pageRoot())

    (* Creating the two supported database interfaces *)
    structure DbOra : DB = DbFunctor(structure DbBasic = DbBasicOra
				     structure Set = Set
				     structure Info = Info)
    structure DbPg  : DB = DbFunctor(structure DbBasic = DbBasicPG
				     structure Set = Set
				     structure Info = Info)
  end

infixr 5 ^^
val op ^^ = Ns.Quot.^^
