structure Ns :> NS =
  struct
    open NsBasics

    type LogSeverity = int
    val Notice=0 and Warning=1 and Error=2 and Fatal=3
    and Bug=4 and Debug=5

    fun log (ls: LogSeverity, s: string) : unit =
      prim("nssml_log", "nssml_log", (ls, s))

    type conn = int
    fun getConn () : conn = 
      prim("nssml_GetConn", "nssml_GetConn", ())

    fun isNull(s : string) : bool = prim("nssml_isNullString", "nssml_isNullString", s)

    structure Set : NS_SET = NsSet 
    type set = Set.set

    fun encodeUrl(s: string) : string =
      prim("nssml_EncodeUrl", "nssml_EncodeUrl", s)

    fun decodeUrl(s: string) : string =
      prim("nssml_DecodeUrl", "nssml_DecodeUrl", s)

    structure Conn =
      struct
	fun returnHtml(status: int, s: string) : status =
	  prim("nssml_ConnReturnHtml", "nssml_ConnReturnHtml", (getConn(),status,s))
	fun return s = returnHtml(200,s)
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

	fun write s = puts s
      end

    structure Cookie =
      struct
	(* This is an modified implementation of Cookies found in MoscowML. 
  	   This is from the MoscowML source:

	     (c) Hans Molin, Computing Science Dept., Uppsala University, 1999.
	     http://www.csd.uu.se/~d97ham/                     d97ham@csd.uu.se

	     Documentation, cleanup and efficiency improvements by sestoft@dina.kvl.dk 

	     Anyone is granted the right to copy and/or use this code, provided
	     that this note is retained, also in modified versions.  The code is
	     provided as is with no guarantee about any functionality.  I take no
	     responsibility for its proper function. *)
	local
	  fun concatOpt s NONE     = ""
	    | concatOpt s (SOME t) = s ^ t
	in
	  exception CookieError of string

	  type cookiedata = 
	    {name   : string, 
	     value  : string, 
	     expiry : Date.date option, 
	     domain : string option, 
	     path   : string option, 
	     secure : bool}

	  val allCookies : (string * string) list =
	    case Set.filter (fn (k,_) => k = "Cookie") (Conn.headers()) of
	      [] => []
	    | ([(k,cv)]) =>
		let
		  fun splitNameAndValue sus = 
		    let
		      val (pref,suff) = Substring.position "=" sus
		    in
		      (decodeUrl (Substring.concat (Substring.fields (fn c => c = #" ") pref)),
		       decodeUrl (Substring.concat (Substring.fields (fn c => c = #" ") (Substring.triml 1 suff))))
		    end
		in
		  List.map splitNameAndValue (Substring.tokens (fn c => c = #";") (Substring.all cv))
		end
	    | _ => raise CookieError "More than one Cookie line in the header"

	  fun getCookie cn = List.find (fn (name,value) => cn = name) allCookies

	  fun getCookieValue cn = 
	    case getCookie cn of
	      NONE => NONE
	    | SOME (n,v) => SOME v

	  (* Date must be GMT time, that is, use Date.fromTimeUniv *)
	  fun setCookie {name : string, value : string, expiry : Date.date option, 
			 domain : string option, path : string option, secure : bool} =
	    let 
	      fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date
	    in
	      if name = "" orelse value= ""
		then raise CookieError "Name or value empty in call to setCookie"
	      else String.concat
		["Set-cookie: ", encodeUrl name, "=", encodeUrl value,
		 concatOpt "; expires=" (Option.map datefmt expiry),
		 concatOpt "; domain=" domain,
		 concatOpt "; path=" path,
		 "; secure", Bool.toString secure]
	    end

	  fun setCookies cookies = String.concat (List.map setCookie cookies)

	  fun deleteCookie { name : string, path : string option } : string =
	    String.concat["Set-cookie: ", encodeUrl name, "=deleted;",
			  "expires=Friday, 11-Feb-77 12:00:00 GMT",
			  concatOpt "; path=" path]
	end
      end

    structure Cache =
      struct
	type cache = int
	fun createTm(n : string, t: int) : cache =
	  prim("nssml_CacheCreate", "nssml_CacheCreate", (n,t))
	fun createSz(n : string, sz: int) : cache =  (* sz is in bytes *)
	  prim("nssml_CacheCreateSz", "nssml_CacheCreateSz", (n,sz))
	fun find (n : string) : cache option =
	  let val res : int = prim("nssml_CacheFind", "nssml_CacheFind", n)
	  in if res = 0 then NONE
	     else SOME res
	  end
	fun findTm(cn: string, t: int) : cache =
	  case find cn of
	    NONE => createTm(cn,t)
	  | SOME c => c
	fun findSz(cn: string, s: int) : cache =
	  case find cn of
	    NONE => createSz(cn,s)
	  | SOME c => c
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

	local
	  fun cache_fn (f:string->string, cn: string,t: int) set get =
	    (fn k =>
  	     case find cn of 
	       NONE => let val v = f k in (set (createTm(cn,t),k,v);v) end
	     | SOME c => (case get (c,k) of 
			    NONE => let val v = f k in (set (c,k,v);v) end 
			  | SOME v => v))
	in
	  fun cacheWhileUsed (arg as (f:string->string, cn: string, t: int)) = cache_fn arg set get
	  fun cacheForAwhile (arg as (f:string->string, cn: string, t: int)) =
	    let
	      val ts_sz = 12
	      fun set'(c,k,v) = set(c,k,StringCvt.padLeft #" " ts_sz (LargeInt.toString(Time.toSeconds(Time.now()))))
	      fun get'(c,k) = 
		case get(c,k) of
		  NONE => NONE
		| SOME vts => 
		    let 
		      val (ts,v) = (Option.valOf(LargeInt.fromString(String.substring(vts,0,ts_sz))),
				    String.extract(vts,ts_sz,NONE))
		      val now_sec = Time.toSeconds(Time.now())
		    in
		      if now_sec > ts+t then NONE else SOME v
		    end
	    in
	      cache_fn arg set' get'
	    end
	end
      end

    structure Info : NS_INFO = NsInfo

    type quot = string frag list
    fun quotToString (q : quot) : string =
      concat(map (fn QUOTE s => s | ANTIQUOTE s => s) q)
    val op ^^ : quot * quot -> quot = op @

    fun return (q : quot) : status =
      Conn.returnHtml(200, quotToString q)

    fun returnRedirect(s : string) : status =
      Conn.returnRedirect s      

    fun write (q : quot) : status = 
      Conn.puts (quotToString q)

    fun returnHeaders () : unit =
      Conn.setRequiredHeaders("text/html", 0)

    fun getMimeType(s: string) : string =
      prim("nssml_GetMimeType", "nssml_GetMimeType", s)

    fun getHostByAddr(s: string) : string option =
      let val res : string = prim("nssml_GetHostByAddr", "nssml_GetHostByAddr", s)
      in if isNull res then NONE
	 else SOME res
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
    structure DbOra = DbFunctor(structure DbBasic = NsDbBasicOra
				structure Set = Set
				structure Info = Info)
    structure DbPg = DbFunctor(structure DbBasic = NsDbBasicPG
			       structure Set = Set
			       structure Info = Info)
  end

infixr 5 ^^
val op ^^ = Ns.^^
