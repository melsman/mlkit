structure SMLserver :> SMLSERVER =
  struct
    structure Unsafe = SMLserverUnsafe

    local
	type LogSeverity = int
	val Notice=0 and Warning=1 and Error=2 and Fatal=3
	and Bug=4 and Debug=5    
	fun log' (ls: LogSeverity, s: string) : unit =
	    prim("@Ns_Log", (ls, s))
    in 
	fun log s = log' (Notice, s)
    end

    (* exit(): execute registered ``at exit'' functions and halt
     * the evaluation. The byte code backend translates the
     * OS.Process.terminate function into a HALT instruction,
     * which takes care of freeing region pages. The functions 
     * atExit and exitUnreg may be used, for instance, to close 
     * database connections in case of errors. 
     *)

    structure Exit =
	struct
	    type exitId = int
	    local 
		fun terminate () = OS.Process.terminate OS.Process.success
		val tasks : (int * (unit -> unit)) list ref = ref nil
		val c : int ref = ref 0
	    in
		fun atExit f : exitId =
		    let val i = !c
		    in   c := i + 1 
			; tasks := (i,f) :: !tasks
			; i
		    end
		fun exitUnreg (eid: exitId) : unit =
		    let fun loop ((x as (e,_))::xs) = if e = eid then xs else x :: loop xs
			  | loop nil = nil
		    in tasks := loop (!tasks)
		    end
		fun exit() =
		    (  List.app (fn (_,f) => f()) (!tasks)
		     ; terminate())
	    end
	end
    open Exit

    fun isNull(s : string) : bool = prim("nssml_isNullString", s)

    fun fetchUrl (url : string) : string option =
      let val res:string = prim("nssml_FetchUrl", url)
      in if isNull res then NONE else SOME res
      end

    fun hostAddr(s: string) : string option =
      let val res : string = prim("nssml_GetHostByAddr", s)
      in if isNull res then NONE
	 else SOME res
      end

    local
	type conn = int
	fun getConn0 () : conn = 
	    prim("@Ns_TclGetConn", (0:int))
    in
	fun hasConnection() : bool =
	    getConn0() <> 0

	fun getConn() : conn =
	    let val c = getConn0()
	    in if c = 0 then 
		(log "SMLserverUnsafe: missing connection";
		 raise Unsafe.MissingConnection)
	       else c
	    end
    end


    local
	fun headers0() : NsSet.set =
	    prim("@Ns_ConnHeaders", getConn())
    in 
	fun headers() : (string * string) list =
	    NsSet.list (headers0())
    end

    fun mimeType(s: string) : string =
	prim("nssml_GetMimeType", s)

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

	  fun encodeUrl(s: string) : string =
	      prim("nssml_EncodeUrl", s)

	  fun decodeUrl(s: string) : string =
	      prim("nssml_DecodeUrl", s)

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
	    case List.filter (fn (k,_) => k = "Cookie") (headers()) of
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
			  "expires=Friday, 11-Feb-77 12:00:00 GMT",
			  concatOpt "; path=" path]
	end
      end

    structure StringCache (* : NS_STRING_CACHE *) =
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

    structure Cache : SMLSERVER_CACHE =
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

    structure Form : SMLSERVER_FORM = Unsafe.Form

    structure Info : SMLSERVER_INFO =
	struct
	    fun hostname() : string =
		prim("nssml_InfoHostname", ())
	    fun host() : string =
		prim("nssml_ConnHost", getConn())
	    fun port() : int =
		prim("@Ns_ConnPort", getConn())
	    fun pageRoot() : string =
		prim("nssml_PageRoot", (getConn()))
	    fun location() : string =
		let val res : string = prim("nssml_ConnLocation", getConn())
		in if isNull res then "<unknown>"
		   else res
		end
	    fun url () : string =
		prim("nssml_ConnUrl", getConn())
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

    (* Calling Info.pageRoot requires that the execution knows of a
       connection, which it does not if the execution is for an
       init-script, executed at server start. *)

    val _ = if hasConnection() then OS.FileSys.chDir (Info.pageRoot())
	    else ()

    (* Creating the three supported database interfaces *)
    structure DbOra = 
	SMLserverDbFunctor(structure DbBasic = SMLserverDbBasicOra
			   structure Exit = Exit)

    structure DbPg = 
	SMLserverDbFunctor(structure DbBasic = SMLserverDbBasicPG
			   structure Exit = Exit)

    structure DbMySQL : SMLSERVER_DB = 
      (* We redefine the stucture here because we need a db-handle to
         simulate sequences in MySQL *)
      struct
	local
	  structure Db = 
	      SMLserverDbFunctor(structure DbBasic = SMLserverDbBasicMySQL
				 structure Exit = Exit)
	      
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

structure Form = SMLserver.Form