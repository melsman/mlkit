structure Ns :> NS =
  struct
    open NsBasics

    type LogSeverity = int
    val Notice=0 and Warning=1 and Error=2 and Fatal=3
    and Bug=4 and Debug=5

    fun log (ls: LogSeverity, s: string) : unit =
      prim("@Ns_Log", (ls, s))

    type conn = int
    fun getConn0 () : conn = 
      prim("@Ns_TclGetConn", (0:int))

    exception MissingConnection
    fun getConn() : conn =
      let val c = getConn0()
      in if c = 0 then 
	  (log (Notice, "Ns.getConn: missing connection");
	   raise MissingConnection)
	 else c
      end

    fun isNull(s : string) : bool = prim("nssml_isNullString", s)

    structure Set : NS_SET = NsSet 
    type set = Set.set

    fun encodeUrl(s: string) : string =
      prim("nssml_EncodeUrl", s)

    fun decodeUrl(s: string) : string =
      prim("nssml_DecodeUrl", s)

   fun buildUrl action hvs =
     action ^ "?" ^ (String.concatWith "&" (List.map (fn (n,v) => n ^ "=" ^ encodeUrl v) hvs))

   structure Info : NS_INFO = NsInfo(struct
				       type conn = int
				       val getConn = getConn
				     end)

    structure Conn =
      struct
	type status = status
	type set = set
	fun returnFile(status: int, mt: string, f: string) : status =
	  prim("@Ns_ConnReturnFile", (getConn(),status,mt,f))

	fun returnHtml(status: int, s: string) : status =
	  prim("@Ns_ConnReturnHtml", (getConn(),status,s,size s))
	fun return s = returnHtml(200,s)
	fun returnRedirect(s: string) : status =
	  prim("@Ns_ConnReturnRedirect", (getConn(),s))

	fun url () : string =
	  prim("nssml_ConnUrl", getConn())

	fun method () : string =
	  prim("nssml_ConnMethod", getConn())

	fun contentLength () : int =
	  prim("@nssml_ConnContentLength", getConn())

	fun headers() : set =
	  prim("@Ns_ConnHeaders", getConn())

	fun server() : string =
	  prim("nssml_ConnServer", getConn())

	(* ML implementation of Multipart/form-data *)
	local
	  val (form_data : set option option ref) = ref NONE
	  datatype FV =
	    large_fv of {fv_name : Substring.substring,
			 filename : Substring.substring,
                         filesize : int,
			 content : Substring.substring,
			 content_types : (Substring.substring * Substring.substring) list}
	  | small_fv of {fv_name : Substring.substring,
			 content : Substring.substring,
			 content_types : (Substring.substring * Substring.substring) list}
	  val multiform_data : FV list ref = ref []

	  fun ppFV fv =
	    case fv of
	      large_fv {fv_name,filename,filesize,content,content_types} => 
		"fv_name: " ^ (Substring.string fv_name) ^ 
		"\n filename: " ^ (Substring.string filename) ^ 
		"\n filesize: " ^ (Int.toString filesize) ^
		"\n content: " ^ (Substring.string content) ^ 
		"\n content types: " ^ (List.foldl (fn ((t1,t2),acc) => acc ^ 
						    (Substring.string t1) ^ ":" ^ 
						    (Substring.string t2) ^ "\n") "" content_types)
	    | small_fv {fv_name,content,content_types} =>
		"fv_name: " ^ (Substring.string fv_name) ^ 
		"\n content: " ^ (Substring.string content) ^
		"\n content types: " ^ (List.foldl (fn ((t1,t2),acc) => acc ^ 
						    (Substring.string t1) ^ ":" ^ 
						    (Substring.string t2) ^ "\n") "" content_types)

          (* Local utility functions *)
	  fun lower' NONE = ""
	    | lower' (SOME s) = CharVector.fromList (List.map Char.toLower (explode s))

	  fun getQuery'() : set option =
	    (* Equals Tcl command: ns_conn form *)
	    let 
	      val s : set = prim("@Ns_ConnGetQuery", getConn())
	    in if s = 0 then NONE
	       else SOME s
	    end

	  fun connCopy () : string =
	    let
	      val maxsize = 
		(case (Info.configGetValueExact {sectionName="ns/server/"^server(),
						 key="MaxContentLength"}) of
		   NONE => Option.valOf Int.maxInt
		 | SOME v => (Option.valOf o Int.fromString) v)
		   handle _ => Option.valOf Int.maxInt
	      (* val _ = log (Notice, "Max content length: " ^ Int.toString maxsize) *)
	    in
	      if contentLength() > maxsize then 
		"" (* empty string if content size is too large *)
	      else
		let
		  val s : string = prim("nssml_ConnCopy", getConn())
		in
		  if isNull s then "" else s (* empty string if no content *)
		end
	    end

	  fun skipNewline substr =
	    let
	      val substr = 
		if Substring.first substr = SOME #"\r" then Substring.slice (substr,1,NONE) else substr
	      val substr =
		if Substring.first substr = SOME #"\n" then Substring.slice (substr,1,NONE) else substr
	    in
	      substr
	    end

	  fun skipEndNewline sus =
	    let
	      fun last sus = SOME (Substring.sub(sus,Substring.size sus - 1))
		handle _ => NONE
	      val sus =
		if last sus = SOME #"\n" then Substring.slice (sus,0,SOME (Substring.size sus - 1)) else sus
	      val sus =
		if last sus = SOME #"\r" then Substring.slice (sus,0,SOME (Substring.size sus - 1)) else sus
	    in
	      sus
	    end

	  fun triml sus = Substring.dropl Char.isSpace sus

	  (* Returns (true,rest) or (false,orig_rest) *)
	  fun match case_sensitive_p pattern orig_rest =
	    let
	      fun m c1 c2 = if case_sensitive_p then c1 = c2 else Char.toLower c1 = Char.toLower c2
	      fun match' pattern rest =
		case pattern of
		  [] => (true,rest)
		| p::ps => 
		    (case Substring.getc rest of
		       NONE => (false,orig_rest)
		     | SOME(c,rest) => 
			 if m c p then
			   match' ps rest
			 else
			   (false,orig_rest))
	    in
	      match' (String.explode pattern) orig_rest
	    end

	  (* Content-Type: text/plain *)
	  fun parseContentTypes (content,acc) =
	    let
	      val (content_type,after_content_type) = Substring.position "\n" content
	      val (content_type,content_type_value) = Substring.position ":" content_type
	    in
	      if Substring.size content_type_value = 0 then 
	        (* no : and therefore no contenttype *)
		(acc,content)
	      else
		let
		  val content_type_value = Substring.slice(content_type_value,1,NONE)
		  val content_type_value = triml content_type_value
		  val content_type_value = #1(Substring.splitl (fn c => c <> #"\r" andalso 
								c <> #"\n") content_type_value)
		in
		  ((content_type,content_type_value)::acc, 
		   skipNewline after_content_type)
		end
	    end

	  fun parseNextBoundary boundary content =
	    let
	      (* Search for next boundary *)
	      val (_,next_boundary) = Substring.position boundary content

	      val next_field = Substring.slice(next_boundary,String.size boundary,NONE)
	      val next_field = skipNewline next_field

	      val (disp,after_disp) = Substring.position "\n" next_field
	      val after_disp = skipNewline after_disp

	      (* Fetch the disposition line and field name *)
	      val eat_disposition =
		case match false "Content-Disposition: form-data;" disp of
		  (true,rest) => rest
		| _ => raise Fail "Ns.eat_disposition: can't find disposition"
	      val eat_disposition = triml eat_disposition

	      val (fv_name,fv_filename) = Substring.position ";" eat_disposition
	      val fv_name = 
		case match false "name=\"" fv_name of
                  (true,rest) => rest
		  | _ => raise Fail "Ns.parseNextBoundary: can't find fv_name"
              val fv_name = skipEndNewline fv_name
	      val fv_name = Substring.dropl (fn ch => ch = #"\"") fv_name
	      val fv_name = Substring.dropr (fn ch => ch = #"\"") fv_name

	      val filename_opt =
		case match false "; filename=\"" fv_filename of
                  (true,rest) =>
   	             let
		       val fv_filename = skipEndNewline rest
		       val fv_filename = Substring.dropl (fn ch => ch = #"\"") fv_filename
		       val fv_filename = Substring.dropr (fn ch => ch = #"\"") fv_filename
		     in
		       SOME fv_filename
		     end
		| _ => NONE

	      val after_disp = skipNewline after_disp
	      val after_disp = triml after_disp

	      (* See if there are extra content information *)
	      val (content_types,after_content_types) = parseContentTypes (after_disp,[])

	      (* Parse value *)
             val before_eat_value = skipNewline after_content_types
             val (value_incl_end_newline,after_value) = Substring.position boundary before_eat_value
             val value_excl_end_newline = skipEndNewline value_incl_end_newline
	    in 
	      case filename_opt of
		NONE => SOME (small_fv{fv_name = fv_name,
				       content = value_excl_end_newline,
				       content_types = content_types},after_value)
	      | SOME filename => SOME (large_fv {fv_name = fv_name,
						 filename = filename,
                                                 filesize = Substring.size value_excl_end_newline,
						 content = value_excl_end_newline,
						 content_types = content_types},after_value)
	    end
	  handle Fail _ => NONE

	  (* Parsing multipart/form-data: we have large and small chunks of data *)
	  (* File Format: Content-Disposition: form-data; name="clientfile"; filename="to_do.txt" *)
	  (* Ordinary FV: Content-Disposition: form-data; name="description" *)
	  fun parseContent (content,contentType) =
	    let
	      (* Fetch boudary out of contentTypeHeader *)
	      val boundary =
		case RegExp.extract (RegExp.fromString ".*[bB][oO][uU][Nn][Dd][Aa][Rr][Yy]=(.*)") contentType of
		  SOME [pat] => pat
		| _ => raise Fail ("Ns.Conn.parseContent: can't get boundary out of contentType: " ^ 
				   contentType)
	      val boundary = "--" ^ boundary
	      val content = Substring.all content
	      fun loop content =
		case parseNextBoundary boundary content of
		  NONE => []
		| SOME (fv,rest) => fv :: loop rest
	    in 
	      (loop content,boundary)
	    end
	  handle Fail _ => ([],"boundary")
	in
	  fun getQuery () =
	    case !form_data of 
	      (* Form data has already been calculated *)
	      SOME data => data
	    | NONE =>
		let 
		  val content_type = NsSet.iget (headers(),"content-type")
		in
		  if method() = "POST" andalso
		    RegExp.match (RegExp.fromString ".*multipart/form-data.*") (lower' content_type)
		    then
		      let
			val content = connCopy()
			val (fvs,boundary) = parseContent(content,valOf content_type)
			val _ = multiform_data := fvs
		      in
			case fvs of
			  [] => NONE
			  | fvs => 
			    let
			      (* Create Tcl set for form variables *)
			      val form = NsSet.create boundary (* todo: where is this set freed ? *)
			      fun storeContentType fv_name (key,value) = 
				NsSet.put(form,
					  Substring.string fv_name ^ "." ^ 
					  lower' (SOME (Substring.string key)),
					  Substring.string value)
			      fun storeFV fv =
				case fv of
				  large_fv {fv_name,filename,filesize,content,content_types} =>
				    (NsSet.put (form, Substring.string fv_name,Substring.string filename);
				     NsSet.put (form, Substring.string fv_name ^ ".filesize", 
						Int.toString filesize);
				     List.app (storeContentType fv_name) content_types)
				| small_fv {fv_name,content,content_types} =>
				    (NsSet.put(form,Substring.string fv_name,Substring.string content);
				     List.app (storeContentType fv_name) content_types)
			      val _ = List.app storeFV fvs
			    in
			      (form_data := SOME (SOME form);
			       getQuery())
			    end
		      end
		  else
		    (form_data := SOME (getQuery'());
		     getQuery())
		end
	      handle _ => NONE
	  fun storeMultiformData (fv,filename) =
	    let
	      val _ = getQuery() (* Make sure that formvariables have been read *)
	      val os = BinIO.openOut filename
	      fun r exn = (BinIO.closeOut os; raise exn)
	      fun storeFV [] = r (Fail ("Ns.storeMultiformData. FV " ^ fv ^ " does not exists"))
		| storeFV (x::xs) =
		case x of
		  large_fv{fv_name,filename,content,...} =>
		    if Substring.string fv_name = fv then 
		      BinIO.output (os,Substring.string content) 
		    else 
		      storeFV xs
		| small_fv{fv_name,...} =>
   	            if Substring.string fv_name = fv then 
		      r (Fail ("Ns.storeMultiformData. FV " ^ fv ^ " does not contain a file"))
		    else 
		      storeFV xs
	    in
	      storeFV (!multiform_data);
	      BinIO.closeOut os
	    end
	  handle _ => raise Fail ("Ns.storeMultiformData. can't open filename: " ^ filename)

	end

	fun formvar s = case getQuery()
			  of SOME set => Set.get(set,s)
			   | NONE => NONE
	fun formvarAll s = 
	  case getQuery()
	    of SOME set => Set.getAll(set,s)
	  | NONE => []

	fun host() : string =
	  prim("nssml_ConnHost", getConn())

	fun location() : string =
	  let val res : string = prim("nssml_ConnLocation", getConn())
	  in if isNull res then "<unknown>"
	     else res
	  end

	fun peer() : string =
	  prim("nssml_ConnPeer", getConn())

	fun peerPort() : int =
	  prim("@Ns_ConnPeerPort", getConn())

	fun port() : int =
	  prim("@Ns_ConnPort", getConn())

	fun redirect(url: string) : status =
	  prim("@Ns_ConnRedirect", (getConn(),url))

	fun puts(s: string) : status =
	  prim("@Ns_ConnPuts", (getConn(), s))

	fun flushHeaders(status: int) : status =
	  prim("@Ns_ConnFlushHeaders", (getConn(),status))

	fun setRequiredHeaders(contentType: string, contentLength: int) : unit =
	  prim("@Ns_ConnSetRequiredHeaders", (getConn(), contentType, contentLength))

	fun hasConnection() = getConn0() <> 0

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

	  fun allCookies() : (string * string) list =
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

	  fun getCookie cn = List.find (fn (name,value) => cn = name) (allCookies())

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
		 "; ", if secure then "secure" else ""]
	    end

	  fun setCookies cookies = String.concat (List.map setCookie cookies)

	  fun deleteCookie { name : string, path : string option } : string =
	    String.concat["Set-cookie: ", encodeUrl name, "=deleted;",
			  "expires=Fri, 11-Feb-77 12:00:00 GMT",
			  concatOpt "; path=" path]
	end
      end

    structure StringCache :> NS_STRING_CACHE =
      struct
	type cache = int
	fun createTm(n : string, t: int) : cache =
	  prim("nssml_CacheCreate", (n,t))
	fun createSz(n : string, sz: int) : cache =  (* sz is in bytes *)
	  prim("nssml_CacheCreateSz", (n,sz))
	fun find (n : string) : cache option =
	  let val res : int = prim("@Ns_CacheFind", n)
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
	  prim("@Ns_CacheFlush", c)
	fun set(c:cache, k:string, v:string) : bool =
	  let 
	    val res : int = prim("nssml_CacheSet", (c,k,v))
	  in res = 1
	  end
	fun get(c:cache, k:string) : string option =
	  let val res : string = prim("nssml_CacheGet", (c,k))
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
	  fun cacheWhileUsed (arg as (f:string->string, cn: string, t: int)) = 
	    cache_fn arg set get
	  fun cacheForAwhile (arg as (f:string->string, cn: string, t: int)) =
	    let
	      open Time
	      fun set'(c,k,v) = set(c,k, toString (now()) ^ ":" ^ v)
	      fun get'(c,k) = 
		case get(c,k) of
		  NONE => NONE
		| SOME t0_v => 
		    (case scan Substring.getc (Substring.all t0_v)
		       of SOME (t0,s) => 
			 (case Substring.getc s
			    of SOME (#":",v) => 
			      if now() > t0 + (fromSeconds t)
				then NONE 
			      else SOME (Substring.string v)
			     | _ => NONE)
			| NONE => NONE)
	    in
	      cache_fn arg set' get'
	    end
	end
      end

    structure Cache :> NS_CACHE =
      struct
	(* This module uses the basic cache functionalities
   	   implemented in NS_STRING_CACHE *)

	val max_cache_name_size = 31 (* Max size of cache name supported by AOLserver pre version 4 is 
                                        32 and we leave one slot for the terminating \0. *)

	datatype kind =
	  WhileUsed of int
	| TimeOut of int
	| Size of int

	type name = string

	type 'a Type = {name: string,
			to_string: 'a -> string,
			from_string: string -> 'a}

	type ('a,'b) cache = {name: string,
			      kind: kind,
			      domType: 'a Type,
			      rangeType: 'b Type,
			      cache: StringCache.cache}

	(* Cache info *)
	fun pp_kind kind =
	  case kind of
	    WhileUsed t => "WhileUsed(" ^ (Int.toString t) ^ ")"
	  | TimeOut t => "TimeOut(" ^ (Int.toString t) ^ ")"
	  | Size n => "Size(" ^ (Int.toString n) ^ ")"

	fun pp_type (t: 'a Type) = #name t
	fun pp_cache (c: ('a,'b)cache) = 
	  "[name:" ^ (#name c) ^ ",kind:" ^ (pp_kind(#kind c)) ^ 
	  ",domType: " ^ (pp_type (#domType c)) ^ 
	  ",rangeType: " ^ (pp_type (#rangeType c)) ^ "]"
	  
	fun get (domType:'a Type,rangeType: 'b Type,name,kind) =
	  let
	    fun pp_kind kind =
	      case kind of
		WhileUsed t => "W"
	      | TimeOut t => "T"
	      | Size n => "S"
	    val c_name = name ^ (pp_kind kind) ^ #name(domType) ^ #name(rangeType)
	    val _ = 
	      if String.size c_name > max_cache_name_size then
		raise Fail ("Ns.Cache.get: Can't create cache because cache name " ^ 
			    c_name ^ " is larger than "  ^ (Int.toString max_cache_name_size))
	      else ()
	    val cache = 
	      case kind of
		Size n => StringCache.findSz(c_name,n)
	      | WhileUsed t => StringCache.findTm(c_name,t)
	      | TimeOut t => StringCache.findTm(c_name,t)

	  in
	    {name=c_name,
	     kind=kind,
	     domType=domType,
	     rangeType=rangeType,
	     cache=cache}
	  end

	local
	  open Time
	  fun getWhileUsed (c: ('a,'b) cache) k =
	    StringCache.get(#cache c,#to_string(#domType c) k)
	  fun getTimeOut (c: ('a,'b) cache) k t =
	    case StringCache.get(#cache c,#to_string(#domType c) k) of
	      NONE => NONE
	    | SOME t0_v => 
		(case scan Substring.getc (Substring.all t0_v)
		   of SOME (t0,s) => 
		     (case Substring.getc s
			of SOME (#":",v) => 
			  if now() > t0 + (fromSeconds t)
			    then NONE 
			  else SOME (Substring.string v)
		      | _ => NONE)
		 | NONE => NONE)
	in
	  fun lookup (c:('a,'b) cache) (k: 'a) =
	    let
	      val v = 
		case #kind c of
		  Size n => StringCache.get(#cache c,#to_string(#domType c) k)
		| WhileUsed t => getWhileUsed c k 
		| TimeOut t => getTimeOut c k t
	    in
	      case v of
		NONE => NONE
	      | SOME s => SOME ((#from_string (#rangeType c)) s)
	    end
	end

	fun insert (c: ('a,'b) cache, k: 'a, v: 'b) =
	  case #kind c of
	    Size n => StringCache.set(#cache c,
				      #to_string (#domType c) k,
				      #to_string (#rangeType c) v)
	  | WhileUsed t => StringCache.set(#cache c,
					   #to_string (#domType c) k,
					   #to_string (#rangeType c) v)
	  | TimeOut t => StringCache.set(#cache c,
					 #to_string(#domType c) k,
					 Time.toString (Time.now()) ^ ":" ^ ((#to_string (#rangeType c)) v))

	fun flush (c: ('a,'b) cache) = StringCache.flush (#cache c)
	  
	fun memoize (c: ('a,'b) cache) (f:('a -> 'b)) =
	  (fn k =>
	   (case lookup c k of 
	      NONE => let val v = f k in (insert (c,k,v);v) end 
	    | SOME v => v))

	fun Pair (t1 : 'a Type) (t2: 'b Type) =
	  let
	    (* Type pair is printed: (type1,type2) *)
	    val name = "(" ^ (#name t1) ^ "," ^ (#name t2) ^ ")"
	    fun to_string (a,b) = 
	      let
		val a_s = (#to_string t1) a
		val a_sz = Int.toString (String.size a_s)
		val b_s = (#to_string t2) b
	      in
		a_sz ^ ":" ^ a_s ^ b_s
	      end
	    fun from_string s =
	      let
		val s' = Substring.all s
		val (a_sz,rest) = 
		  Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
		val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
		val (a_s,b_s) = (Substring.slice(rest,0,SOME a_sz),Substring.slice(rest,a_sz,NONE))
		val a = (#from_string t1) (Substring.string a_s)
		val b = (#from_string t2) (Substring.string b_s)
	      in
		(a,b)
	      end
	  in
	    {name=name,
	     to_string=to_string,
	     from_string=from_string}
	  end

	fun Option (t : 'a Type) =
	  let
	    (* Option type is printed: O(type) *)
	    val name = "O(" ^ (#name t) ^ ")" 
	    fun to_string a = 
	      case a of
		NONE => "0:N()"
	      | SOME v => 
		  let
		    val v_s = (#to_string t) v
		    val v_sz = Int.toString (String.size v_s)
		  in
		    v_sz ^ ":S(" ^ v_s ^ ")"
		  end
	    fun from_string s =
	      let
		val s' = Substring.all s
		val (v_sz,rest) = 
		  Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
		val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
		val (N_S,rest) = Option.valOf (Substring.getc rest) (* read N og S *)
		val rest = #2(Option.valOf (Substring.getc rest)) (* skip "(" *)
	      in
		if N_S = #"S" then
		  SOME ((#from_string t) (Substring.string (Substring.slice(rest,0,SOME v_sz))))
		else
		  NONE
	      end
	  in
	    {name=name,
	     to_string=to_string,
	     from_string=from_string}
	  end

	fun List (t : 'a Type ) =
	  let
	    (* List type is printed: L(type) *)
	    val name = "L(" ^ (#name t) ^ ")"
	    (* Format: [x1_sz:x1...xN_sz:xN] *)
	    fun to_string xs = 
	      let
		fun to_string_x x =
		  let
		    val v_x = (#to_string t) x
		  in
		    Int.toString (String.size v_x) ^ ":" ^ v_x
		  end
		val xs' = List.map to_string_x xs
	      in
		"[" ^ (String.concat xs') ^ "]"
	      end
	    fun from_string s =
	      let
		fun read_x (rest,acc) = 
		  if Substring.size rest = 1 (* "]" *) then
		    List.rev acc
		  else
		    let
		      val (x_sz,rest) = Option.valOf (Int.scan StringCvt.DEC Substring.getc rest)
		      val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
		      val (x_s,rest) = (Substring.slice(rest,0,SOME x_sz),Substring.slice(rest,x_sz,NONE))
		    in
		      read_x (rest,((#from_string t) (Substring.string x_s)) :: acc)
		    end
		val s' = Substring.all s
		val rest = #2(Option.valOf (Substring.getc s')) (* skip "[" *)
	      in
		read_x (rest,[])
	      end
	  in
	    {name=name,
	     to_string=to_string,
	     from_string=from_string}
	  end

	fun Triple (t1 : 'a Type) (t2: 'b Type) (t3: 'c Type) =
	  let
	    (* Type triple is printed (type1,type2,type3) *)
	    val name = "(" ^ (#name t1) ^ "," ^ (#name t2) ^ "," ^ (#name t3) ^ ")"
	    fun to_string (a,b,c) = 
	      let
		val a_s = (#to_string t1) a
		val a_sz = Int.toString (String.size a_s)
		val b_s = (#to_string t2) b
		val b_sz = Int.toString (String.size b_s)
		val c_s = (#to_string t3) c
	      in
		a_sz ^ ":" ^ a_s ^ b_sz ^ ":" ^ b_s ^ c_s
	      end
	    fun from_string s =
	      let
		val s' = Substring.all s
		val (a_sz,rest) = 
		  Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
		val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
		val (a_s,rest) = (Substring.slice(rest,0,SOME a_sz),Substring.slice(rest,a_sz,NONE))
		val (b_sz,rest) = 
		  Option.valOf (Int.scan StringCvt.DEC Substring.getc rest)
		val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)		  
		val (b_s,c_s) = (Substring.slice(rest,0,SOME b_sz),Substring.slice(rest,b_sz,NONE))
		val a = (#from_string t1) (Substring.string a_s)
		val b = (#from_string t2) (Substring.string b_s)
		val c = (#from_string t3) (Substring.string c_s)
	      in
		(a,b,c)
	      end
	  in
	    {name=name,
	     to_string=to_string,
	     from_string=from_string}
	  end

	(* Pre defined cache types *)
	val Int    = {name="I",to_string=Int.toString,from_string=Option.valOf o Int.fromString}
	val Real   = {name="R",to_string=Real.toString,from_string=Option.valOf o Real.fromString}
	val Bool   = {name="B",to_string=Bool.toString,from_string=Option.valOf o Bool.fromString}
	val Char   = {name="C",to_string=Char.toString,from_string=Option.valOf o Char.fromString}
	val String = {name="S",to_string=(fn s => s),from_string=(fn s => s)}
      end

    type quot = Quot.quot
    fun return (q : quot) : status =
      Conn.returnHtml(200, Quot.toString q)

    fun returnRedirect(s : string) : status =
      Conn.returnRedirect s      

    fun write (q : quot) : status = 
      Conn.puts (Quot.toString q)

    fun returnHeaders () : unit = 
      (Conn.setRequiredHeaders("text/html", 0);
       Conn.flushHeaders(200);
       ())

    fun returnFileMime (mimetype:string) (file:string) : status =
      prim("nssml_returnFile", (getConn(),mimetype,file))

    fun getMimeType(s: string) : string =
      prim("nssml_GetMimeType", s)

    fun returnFile (file:string):status =
      returnFileMime (getMimeType file) file

    fun getHostByAddr(s: string) : string option =
      let val res : string = prim("nssml_GetHostByAddr", s)
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
      let val res:string = prim("nssml_FetchUrl", url)
      in if isNull res then NONE else SOME res
      end

    fun exit() = (raise Interrupt)  
    (* By raising Interrupt, the web-server is not killed as it
     * would be if we call OS.Process.exit. Also, handlers can
     * protect the freeing of resources such as file descriptors
     * and database handles. Moreover, region pages are freed as 
     * they should be. *)

    fun registerTrap (u:string) : unit = 
      prim("nssml_registerTrap", u)

    fun scheduleScript (f: string) (i:int) : unit =
      prim("nssml_scheduleScript", (f,i))	

    fun scheduleDaily (f:string) {hour:int,minute:int} : unit =
      prim("nssml_scheduleDaily", (f,hour,minute))	

    fun scheduleWeekly (f:string) {day:int,hour:int,minute:int} : unit =
      prim("nssml_scheduleWeekly", (f,day,hour,minute))	

    (* Calling Info.pageRoot requires that the execution knows of a
       connection, which it does not if the execution is for an
       init-script, executed at server start. *)
    val _ = 
      if Conn.hasConnection() then
	OS.FileSys.chDir (Info.pageRoot())
      else
	()

    (* Creating the two supported database interfaces *)
    structure DbOra = DbFunctor(structure DbBasic = NsDbBasicOra)
    structure DbPg = DbFunctor(structure DbBasic = NsDbBasicPG)
    structure DbMySQL : NS_DB = 
      (* We redefine the stucture here because we need a db-handle to
         simulate sequences in MySQL *)
      struct
	local
	  structure Db = DbFunctor(structure DbBasic = NsDbBasicMySQL)
	in
	  open Db
	  (* seqNextval assumes a table simulating the sequence with one auto-increment field:
              create table seqName (
                seqId integer primary key auto_increment
              ); *)
	  fun seqNextval (seqName:string) : int = 
	    let 
	      val _ = Db.dml `insert into ^seqName (seqId) values (null)`
	      val s = Db.oneField `select max(seqId) from ^seqName`
	    in case Int.fromString s of
	      SOME i => i
	    | NONE => raise Fail "Db.seqNextval.nextval not an integer (MySQL)"	
	    end
	  handle _ => raise Fail "Db.seqNextval.nextval database error (MySQL)"	

	  fun seqCurrval (seqName:string) : int = 
	    let val s = oneField `select max(seqId) from ^seqName`
	    in case Int.fromString s of
	      SOME i => i
	    | NONE => raise Fail "Db.seqCurrval.nextval not an integer (MySQL)"	
	    end
	  handle _ => raise Fail "Db.seqCurrval.nextval database error (MySQL)"	
	end
      end 
  end

(* Stuff not used when implementing multipart/formdata, Niels *)

(*


(*			 val tmp_file = upload_dir ^ "/" ^ (uniqueFile upload_dir 10)
val _ = debug("tmpfile: " ^ tmp_file)
			 val status = connCopyToFile tmp_file
val _ = debug("connCopyToFile status: " ^ ppStatus status)
                         val _ = parseContent(tmp_file,valOf content_type)*)


(*		         val _ = FileSys.remove tmp_file*)

*)

	 (*   if you delete this, the remember to remove nssml_ConnCopy in 
	    nssmllib.c and BuildInCFunctions*)

(*
fun match getc case_sensitive_p pattern orig_rest =
  let
    fun m c1 c2 = if case_sensitive_p then c1 = c2 else Char.lower c1 = Char.lower c2
    fun match' pattern rest =
      let
	val (x,rest) = getc 1 rest
      in  
	case pattern of
	  [] => (true,rest)
	| p::ps => 
	    if m x p then
	      match' ps rest
	    else
	      (false,orig_rest)
      end
  in
    match' (String.explode pattern) orig_rest
  end

val (disp_p,rest) = match getc true "Content-Disposition: form-data; "
val (name,                

	  fun connCopyToFile (filename : string) : status =
	    prim("@nssml_ConnCopyToFile", (getConn(),filename))


(*	  fun parseContent (tmp_file,contentType) =
	    let
	      val fid = TextIO.openIn tmp_file
	      val content = TextIO.inputAll fid
val _ = debug content*)
	      (* Fetch boudary out of contentTypeHeader *)
(*	(*      val boundary =
		case RegExp.extract (RegExp.fromString ".*[bB][oO][uU][Nn][Dd][Aa][Rr][Yy]=(.*)") contentType of
		  SOME [pat] => pat
		| _ => raise Fail ("Ns.Conn.parseContent: can't get boundary out of contentType: " ^ 
				   contentType)
	      val boundary = "--" ^ boundary

	      val _ = debug ("parseContent.boundary = " ^ boundary)*)

*)

(*			  val eat_filename = 
			    case match true "filename=\"" eat_filename of
   			      (true,rest) => rest
			    | _ => raise Fail "eat_filename: can't find filename"
                          val _ = debug false ("Eat filename (after eat filename=\"): " ^ (Substring.string eat_filename))
			  val (fv_filename,eat_fv_filename) = Substring.position "\"" eat_filename
			  val _ = debug false ("Fv name: " ^ (Substring.string fv_filename))
			  val eat_fv_filename = 
			    case match false "\"" eat_fv_filename of
			      (true,rest) => rest
			    | _ => raise Fail "Ns.eat_fv_name: can't find last \" in fv_name"*)


(*		  val eat_name =
		    case match true "name=\"" eat_disposition of
                      (true,rest) => rest
		    | _ => raise Fail "Ns.eat_disposition: can't find name in disposition"
                  val _ = debug false ("After eat name: " ^ (Substring.string eat_name))
		  val (fv_name,eat_fv_name) = Substring.position "\"" eat_name
		  val _ = debug false ("Fv name: " ^ (Substring.string fv_name))
		  val eat_fv_name = 
		    case match false "\"" eat_fv_name of
		      (true,rest) => rest
		    | _ => raise Fail "Ns.eat_fv_name: can't find last \" in fv_name"*)

	  (* Eat a field like fieldname="value" returning value without the two "s *)
	  (* Raise Fail if string is not on that form *)
(*	  fun eatField fieldname content = (*todo: problem with " at start, end and middle of field value 
try read to \n first and then process line - same as in parseContentTypes*)
	    let
	      val eat_fieldname =
		case match true (fieldname ^ "=\"") content of
  	          (true,rest) => rest
		| _ => raise Fail ("Ns.eatField: can't find name in " ^ fieldname)
	      val eat_fieldname = Substring.dropl (fn ch => ch = #"\"") eat_fieldname
	      val (value,eat_value) = Substring.position "\"" eat_fieldname
	      val rest = 
		case match false "\"" eat_value of
  		  (true,rest) => rest
		| _ => raise Fail ("Ns.eatField: can't find last \" in value" ^ fieldname)
(*	      val rest = Substring.dropl (fn ch => ch = #"\"") rest*)
	    in
	      (value,rest)
	    end*)

	      (* Fetch the disposition line and field name *)
(*	      val eat_disposition =
		case match true "Content-Disposition: form-data;" next_field of
		  (true,rest) => rest
		| _ => raise Fail "Ns.eat_disposition: can't find disposition"
	      val eat_disposition = triml eat_disposition

	      val (fv_name,eat_fv_name) = eatField "name" eat_disposition

	      (* Maybe there is a third field (filename) *)
	      val (filename_opt,eat_fv_filename) =
		case Substring.first eat_fv_name of
		  SOME #";" => 
		    let
		      val eat_filename = Substring.slice (eat_fv_name,1,NONE)
		      val eat_filename = triml eat_filename
		      val (fv_filename,eat_fv_filename) = eatField "filename" eat_filename
		    in
		      (SOME fv_filename,eat_fv_filename)
		    end
		| _ => (NONE,eat_fv_name)*)
(*
	  fun uniqueFile dir c =
	    if c > 10 then raise raise Fail (Quot.toString `Ns.uniqueFile ^dir : can't create unique file`)
	    else
	      let val is = Random.rangelist (97,122) (8, Random.newgen())
		val f = "file" ^ implode (map Char.chr is)
	      in 
		if FileSys.access(dir ^ "/" ^ f,[]) then uniqueFile dir (c+1)
		else f
	      end
	    handle OS.SysErr s => raise Fail (Quot.toString `Ns.uniqueFile: dir= ^dir.`)

*)