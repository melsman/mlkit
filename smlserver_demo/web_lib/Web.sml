structure Web :> WEB = 
  struct 
  exception InternalSmlServerError;
  exception InternalSmlServerErrorOutOfMemory;
  
  fun getReqRec () : int  = prim("__serverGetCtx", ())
  fun getReqRecP () : int = prim("apsml_GetReqRec", (getReqRec()))

    (* defined in http_log.h *)
  structure WebBasics : WEB_BASICS =
  struct 
    type LogSeverity = int
    val Emergency=0 and Alert=1 and Critical=2 and Error=3
    and Warning=4 and Notice=5 and Info=6 and Debug=7
    local 
      val pid = (SysWord.toInt o Posix.Process.pidToWord o Posix.ProcEnv.getpid) ()
    in
    fun log (ls: LogSeverity, s: string) : unit = 
      prim("apsml_log", (ls, "[pid: " ^ (Int.toString pid) ^ "] " ^ s, getReqRec(), InternalSmlServerError))
    end
    fun advLog (ls, data, ppdata) = (log(ls, ppdata data);data)
    exception Forbidden
  end
  

  open WebBasics

  exception MissingConnection;
  fun encodeUrl (s : string) : string = prim("apsml_encodeUrl", (s,getReqRec()))
  fun decodeUrl (s : string) : string = prim("apsml_decodeUrl", (s,getReqRec()))

  fun buildUrl action hvs =
    action ^ "?" ^ (String.concatWith "&" (List.map (fn (n,v) => n ^ "=" ^ encodeUrl v) hvs))
  
  fun isNull(s : string) : bool = prim("apsml_isNullString", s)

    structure Info : WEB_INFO = WebInfo(struct
               type conn = int
               val getReqRec = getReqRec
             val isNull = isNull
             val log = (fn x => log(Debug, x))
             exception Forbidden = Forbidden
             end)

    structure WebSet (* : WEB_SET *)=
      struct
        type set = int

        fun isNull(s : string) : bool = prim("apsml_isNullString", s)

        fun get (s :set, key: string): string option =
        let val res : string = prim("apsml_setGet", (s,key))
        in if isNull res then NONE
           else SOME res
        end

      val iget = get

        fun getOpt (s:set, key:string, dflt:string): string =
          Option.getOpt(get (s, key), dflt)

        fun put (s: set, key: string, value: string) : unit = 
          prim("@apr_table_add", (s,key,value))
      
        fun free (s: set) : unit = ()

      fun create (rr : int) : set = 
          prim("@apsml_setCreate", (rr))
      
        fun size (s: set) : int = prim("@apsml_SetSize", s) 

        fun unique (s: set, key: string) : bool =
          prim("@apsml_setUnique", (s,key))

        fun key (s: set, index: int) : string option =
          let val res : string =  prim("apsml_SetKey", (s, index))
          in if isNull res then NONE
       else SOME res
          end

        fun value (s: set, index: int) : string option =
          let val res : string =  prim("apsml_SetValue", (s, index))
          in if isNull res then NONE
       else SOME res
          end

        fun getPair(s,n) = 
          case (key(s,n), value(s,n))
      of (SOME k,SOME v) => (k,v)
          | _ => raise Fail "Web.getPair" 

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
        fun getAll (s:set, key:string): string list = 
          foldl (fn ((k,v),a) => if k = key then v :: a else a) nil s
        fun list s = foldl (op ::) nil s
        fun filter p s = foldl (fn (pair,a) => if p pair then pair :: a else a) nil s
    end

  structure Set : WEB_SET = WebSet 

    structure StringCache :> WEB_STRING_CACHE =
      struct
      type cache = int
        fun create(n : string, t: int, sz : int) : cache =  (* sz is in bytes, t is in seconds *)
          let val c : cache = prim("apsml_cacheCreate", (n,sz, t, getReqRec()))
          in if c = 0 then raise InternalSmlServerErrorOutOfMemory else c
          end
          
        fun find (n : string) : cache option = (*log(Debug, "cacheFind"); *)
          let val res : int = prim("@apsml_cacheFind", (n, getReqRec()))
          in if res = 0 then (*log(Debug, "cacheFind NONE pid:" ^ Int.toString(Info.pid())) ;*) NONE
             else (*log(Debug, "cacheFind SOME, pid:"^ Int.toString(Info.pid()));*) SOME res
          end
        fun findTmSz(cn: string, t: int, sz : int) : cache =
          case find cn of
            NONE => create(cn,t,sz)
          | SOME c => c
        fun flush(c:cache) : unit = (*log(Debug, "cacheFlush");*)
          let val c : cache = prim("@apsml_cacheFlush", (c,getReqRec(), 1))
          in if c = 0 then raise InternalSmlServerErrorOutOfMemory else ()
          end
        fun set(c:cache, k:string, v:string, t:int) : (bool * string option) = 
          let 
            (*val _ = log(Debug, "key: "^ k ^", value: " ^ v) *)
            val (res,value) : (int * string) = prim("apsml_cacheSet", (c,(k,v,t),getReqRec()))
            (*val _ = log(Debug, "apsml_cacheSet DONE") *)
          in if res = 1 
             then (if isNull(value) then (false,NONE) else (false, SOME(value)))
           else if res = 2 
           then (true, NONE)
           else raise InternalSmlServerErrorOutOfMemory
          end 
        fun get(c:cache, k:string) : string option = (*log(Notice, "cacheGet"); *)
          let val res : string = prim("apsml_cacheGet", (c,k, getReqRec()))
          in if isNull res then NONE
             else SOME res
          end

        local
          fun cache_fn (f:string->string, cn: string,t: int, sz : int) set get =
            (fn k =>
               case find cn of 
               NONE => let val v = f k in (set (create(cn,t,sz),k,v,0);v) end
             | SOME c => (case get (c,k) of 
                NONE => let val v = f k in (set (c,k,v,t);v) end 
              | SOME v => v))
        in
          fun cacheWhileUsed (arg as (f:string->string, cn: string, t: int, sz : int)) = 
            cache_fn arg set get
          fun cacheForAwhile (arg as (f:string->string, cn: string, t: int, sz : int)) =
            let
              open Time
              fun set'(c,k,v,t) = set(c,k, toString (now()) ^ ":" ^ v,t)
              fun get'(c,k) = 
                case get(c,k) of
                  NONE => NONE
                | SOME t0_v => 
                    (case scan Substring.getc (Substring.full t0_v)
                       of SOME (t0,s) => 
                        (case Substring.getc s
                            of SOME (#":",v) => 
                              if now() > t0 + (fromSeconds (Int.toLarge t))
                              then NONE 
                              else SOME (Substring.string v)
                             | _ => NONE)
                        | NONE => NONE)
            in
              cache_fn arg set' get'
            end
        end
      end

    structure Cache : WEB_CACHE =
      struct
  (* This module uses the basic cache functionalities
       implemented in WEB_STRING_CACHE *)
  open WebSerialize

  type name = string

(*  val max_cache_name_size = 31 (* Max size of cache name supported by AOLserver pre version 4 is 
                                  *     32 and we leave one slot for the terminating \0. 
                  * In the current implementation there is no limit                            *) *)

  datatype kind =
    WhileUsed of Time.time option * int option
  | TimeOut of Time.time option * int option
(*  | Size of int *)


  type ('a,'b) cache = {name: string,
            kind: kind,
            domType: 'a Type,
            rangeType: 'b Type,
            cache: StringCache.cache}

  (* Cache info *)
  fun pp_kind kind =
    case kind of
      WhileUsed (t,s) => 
        let val time = case Option.map Int.toString (Option.map (LargeInt.toInt o Time.toSeconds) t)
                       of NONE => "No timeout" | SOME(t') => "timeout: " ^ t'
            val size = case Option.map Int.toString s 
                       of NONE => "No size limit" | SOME(s') => "Size limit: " ^ s'
        in 
          "WhileUsed(" ^ time ^ " " ^ size ^ ")"
        end
    | TimeOut (t,s) => 
        let val time = case Option.map Int.toString (Option.map (LargeInt.toInt o Time.toSeconds) t)
                       of SOME(t') => "Time to live: " ^ t' | NONE => "No time limit"
            val size = case Option.map Int.toString s 
                       of NONE => "No size limit" | SOME(s') => "Size limit: " ^ s'
            in
              "TimeOut(" ^ time ^ " " ^ size ^ ")"
            end
(*    | Size n => "Size(" ^ (Int.toString n) ^ ")" *)

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
(*        | Size n => "S" *)
      val c_name = name ^ (pp_kind kind) ^ #name(domType) ^ #name(rangeType)
(*      val _ = 
        if String.size c_name > max_cache_name_size then
    raise Fail ("Ns.Cache.get: Can't create cache because cache name " ^ 
          c_name ^ " is larger than "  ^ (Int.toString max_cache_name_size))
        else () *)
      val cache = 
        case kind of
          WhileUsed (t,s) =>  
             StringCache.findTmSz(c_name, getOpt(Option.map (LargeInt.toInt o Time.toSeconds) t,0),
                                  getOpt(s, ~1))
        | TimeOut (t,s) => 
             StringCache.findTmSz(c_name, getOpt(Option.map (LargeInt.toInt o Time.toSeconds) t,0),
                                  getOpt(s, ~1))
(*        | Size n => StringCache.findTmSz(c_name,0,n)  *)

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
      fun getTimeOut' t' t0_v = 
        let fun getT (s1, t0, t1) = 
             (case Substring.getc s1
               of SOME(#":",v) => (
                 case (t',t1) of (NONE,SOME(t1')) => 
                            (*log(Debug, "NONE,SOME("^ Time.toString t1' ^")");*)
                             if now() > t0 + t1'
                             then NONE
                             else SOME(Substring.string v)
                          | (SOME(t),SOME(t1')) =>
                            (*(log(Debug,"SOME("^Time.toString t^"),SOME("^Time.toString t1' ^")");*)
                             if now() > t0 + t orelse now() > t0 + t1'
                             then NONE
                             else SOME (Substring.string v)
                          | (NONE,NONE) => (*log(Debug,"NONE,NONE");*)SOME(Substring.string v)
                          | (SOME(t),NONE) => 
                           (*log(Debug, "SOME("^ Time.toString t ^"), NONE");*)
                            if now() > t0 + t then NONE
                            else SOME (Substring.string v))
                | _ => NONE)
         in 
        case scan Substring.getc (Substring.full t0_v)
          of SOME (t0,s) => 
             (case Substring.getc s
               of SOME (#":",s') => 
                  (case scan Substring.getc s'
                    of SOME(t1,s1) => getT(s1,t0,SOME(t1))
                     | _ => (
                       case Substring.getc s'
                       of SOME(#"-", s1) => getT(s1, t0, NONE)
                        | _ => NONE))
                | _ => NONE)
           | _ => NONE
          end

    fun getTimeOut (c: ('a,'b) cache) k t v = case v of 
      NONE => (
        case StringCache.get(#cache c,#to_string(#domType c) k) of
          NONE => NONE
        | SOME t0_v =>  getTimeOut' t t0_v)
    | SOME(t0_v) => getTimeOut' t t0_v
  in
    fun lookup (c:('a,'b) cache) (k: 'a) =
      let
        val v = 
          case #kind c of
            WhileUsed (t,_) => getWhileUsed c k 
          | TimeOut (t,_) => getTimeOut c k t NONE
(*          | Size n => StringCache.get(#cache c,#to_string(#domType c) k) *)
      in
        case v of
          NONE => NONE
        | SOME s => SOME ((#from_string (#rangeType c)) s)
      end

  fun insert (c: ('a,'b) cache, k: 'a, v: 'b, to : Time.time option) =
    let fun min (a,b) = if a < b then a else b
        fun zeroTo1 a = if Time.toSeconds a = 0 then Time.fromSeconds ~1 else a
        val t' = (case to of NONE => 0
                           | SOME(s) => if Time.toSeconds s = 0 then ~1 else (LargeInt.toInt o Time.toSeconds) s) : int
    in
    case #kind c of
      WhileUsed (_,_) => #1(StringCache.set(#cache c,
             #to_string (#domType c) k,
             #to_string (#rangeType c) v,
             t'))
    | TimeOut (t,_) => case (StringCache.set(#cache c,
           #to_string(#domType c) k,
           Time.toString (Time.now()) ^ ":" ^ 
           (case (to,t) of (NONE,SOME(t')) => Time.toString (zeroTo1 t')
                     | (SOME(to'),SOME(t')) => Time.toString (min(to',zeroTo1 t'))
                     | (NONE,NONE) => "-"
                     | (SOME(to'),NONE) => Time.toString to')
               ^ ":" ^ ((#to_string (#rangeType c)) v), t'))
           of (true,_) => true
            | (false, NONE) => false
            | (false, SOME(oldvalue)) => 
              case getTimeOut' t oldvalue of NONE => true
                                           | SOME(_) => false
     end
  end

  fun flush (c: ('a,'b) cache) = StringCache.flush (#cache c)
    
  fun memoizePartialTime (c: ('a,'b) cache) (f:('a -> ('b * Time.time option) option)) =
    (fn k =>
     (case lookup c k of 
        NONE => let val v' = f k in (Option.map (fn (v,time) => insert (c,k,v,time)) v';
                                     Option.map (fn (v,_) => v) v') end 
      | SOME v => SOME v))

  fun memoizePartial c (f: ('a -> 'b option)) = 
            memoizePartialTime c (fn x => Option.map (fn y => (y,NONE)) (f x)) 

  fun memoizeTime c f = fn k => valOf(memoizePartialTime c (fn x => SOME (f x)) k)

  fun memoize c f = fn k => valOf(memoizePartial c (fn x => SOME (f x)) k)

      end


  structure Mime :> WEB_MIME =
    struct
      structure BM = Binarymap
      val empty = fn () => BM.mkDict Substring.compare

      exception ParseErr of string
      fun toMap m l =
            let
              val l = Substring.dropl Char.isSpace (Substring.full l)
              val (lv,def) = Substring.splitl (not o Char.isSpace) l
              val def = Substring.tokens Char.isSpace def
            in case (Substring.size lv,def)
               of (0,[]) => m
                | (_,[]) => raise ParseErr (Substring.string lv ^ " has no extensions") 
                |  _     => List.foldl (fn (x,m) => BM.insert (m, x, lv)) m def
            end

      exception FileNotFound

      local
        exception ParseErrLine of (int * string)
      in
      fun readfile f : (Substring.substring, Substring.substring) BM.dict =
            let
              val fh =  (TextIO.openIn f) handle IO.Io _ => raise FileNotFound
              fun close () = (TextIO.closeIn fh) handle _ => ()
              val _ = WebBasics.log (WebBasics.Notice,"Reading Mime-Type configuration file: " ^ f)
              fun loop m lc = (case (TextIO.inputLine fh)
                             of SOME s => (loop (if CharVector.sub(s,0) = #"#" then m else toMap m s) (lc + 1))
                              | NONE => (close() ; m))
                           handle ParseErr s => raise ParseErrLine (lc,s)
                                | IO.Io {cause,...} => raise ParseErrLine (lc,exnMessage cause)
            in (loop (empty ()) 1) handle ParseErrLine (i,s) => (close () ; raise ParseErr ("Parse Error on line " ^ (Int.toString i) ^ ": " ^ s))
            end
      end

      fun readConf f = (readfile f) handle FileNotFound => empty ()
                                         | ParseErr s => raise Fail ("In file: " ^ f ^ ", " ^ s)

      fun mimeMap x = case Info.configGetValue(Cache.String, "MimeTypeFile")
                      of NONE => (WebBasics.log(WebBasics.Notice, "SMLServer.MimeFinder: MimeTypeFile not defined, using application/octet-stream")
                                  ; SOME "application/octet-stream")
                       | SOME f => Option.map Substring.string (BM.peek (readConf f, Substring.full x))
      
      structure WC = Cache

      val mimeCache = WC.get (WC.String, WC.String, "smlserver_mimetype_cache",
                              WC.WhileUsed (SOME (Time.fromSeconds (60*60)), NONE))
      val mimeMap' = WC.memoizePartial mimeCache mimeMap

      fun getMime x = case Option.join (Option.map mimeMap' (OS.Path.ext x))
                      of NONE => (WebBasics.log(WebBasics.Notice, "SMLServer.MimeFinder: File " ^ x ^ " not defined, unsing application/octet-stream")
                                 ; "application/octet-stream")
                       | SOME mime => mime
      fun addEncoding x = let
                            val enc = "; charset=" ^ (Option.getOpt (Info.configGetValue(Info.Type.String, "standardFileEncoding"), "ISO-8859-1"))
                            fun ae (a as "text/html") = a ^ enc
                              | ae (a as "text/plain") = a ^ enc
                              | ae (a as "application/xhtml+xml") = a ^ enc
                              | ae a = a
                          in ae x end
    end


  structure Conn : WEB_CONN = 
    struct 
    type status = int
    type set = Set.set 

    val GET = 0
    val FORM = 1

    val log = WebBasics.log
    
    fun method () : string  = (prim("apsml_method", getReqRec())) handle Overflow => raise MissingConnection

    fun contentLength () : int = (prim("apsml_contentlength", getReqRec())) handle Overflow => raise MissingConnection

    fun hasConnection () : bool = let val b :int = prim("@apsml_hasconnection", getReqRec())
                                  in (b = 1)
                    end

    fun headers () : set = (prim("apsml_headers", getReqRec())) handle Overflow => raise MissingConnection

    fun add_headers (key,value) : unit = prim("@apsml_add_headers_out", (key : string, size key : int, value : string,
                                                                         size value : int, getReqRec())) : unit
    
    fun setMimeType(s : string) : unit = prim("@apsml_setMimeType",(s, size s, getReqRec()))

    fun returnFile(status: int, mt: string, f: string) : status =
      prim("@apsml_returnFile", (status, mt, f, getReqRec()))

    fun returnHtml (i:int, s: string) : status = 
      let
        val _ = add_headers("Cache-Control","no-cache")
      in
        prim("@apsml_returnHtml", (i,s,size s, Mime.addEncoding "text/html" : string, getReqRec()))
      end

    fun returnXhtml (i : int,s : string) : status = 
      let
        val _ = add_headers("Cache-Control","must-revalidate")
      in
        prim("@apsml_returnHtml", (i,s,size s, Mime.addEncoding "application/xhtml+xml" : string, getReqRec()))
      end

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
      

          (* Local utility functions *)
        fun lower' NONE = ""
        | lower' (SOME s) = CharVector.fromList (List.map Char.toLower (explode s))

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

        fun connCopy () : string =
        let
          val maxsize = getOpt(Info.configGetValue(Info.Type.Int, "maxFormDataSize"),50000)
          (* val _ = log (Notice, "Max content length: " ^ Int.toString maxsize) *)
        in
          if contentLength() > maxsize then 
        "" (* empty string if content size is too large *)
          else
        let
          val s : string = prim("apsml_getQueryData", (maxsize, FORM, getReqRec()))
        in
          if isNull s then "" else s (* empty string if no content *)
        end
        end


        (* Parsing multipart/form-data: we have large and small chunks of data *)
        (* File Format: Content-Disposition: form-data; name="clientfile"; filename="to_do.txt" *)
        (* Ordinary FV: Content-Disposition: form-data; name="description" *)
        fun parseContent (content,contentType) =
        let
          (* Fetch boudary out of contentTypeHeader *)
          val boundary =
        case RegExp.extract (RegExp.fromString ".*[bB][oO][uU][Nn][Dd][Aa][Rr][Yy]=(.*)") contentType of
          SOME [pat] => pat
        | _ => raise Fail ("Web.Conn.parseContent: can't get boundary out of contentType: " ^ 
               contentType)
          val boundary = "--" ^ boundary
          val content = Substring.full content
          fun loop content =
        case parseNextBoundary boundary content of
          NONE => []
        | SOME (fv,rest) => fv :: loop rest
        in 
          (loop content,boundary)
        end
        handle Fail _ => ([],"boundary")

          fun getQueryData(i : int) : string option = 
            let val res : string = prim("apsml_getQueryData", (~1, i, getReqRec()))
            in if isNull res then NONE
             else SOME res
            end

      fun parseFormData(NONE : set option, s : string) : set = 
          parseFormData(SOME(WebSet.create(getReqRec())),s)
        | parseFormData(SOME(set), s) = 
           let (*val _ = log(Notice, "parseFormData: " ^ s) *)
             val f0 = Substring.tokens (fn c => c = #"&")
             val f1 = Substring.splitl (fn c => not(c = #"="))
             val f2 = fn (p1,p2) => (decodeUrl (Substring.string p1), 
                decodeUrl (Substring.string (Substring.dropl (fn c => c = #"=") p2)))
             val pairs = map (f2 o f1) (f0 (Substring.full s))
           in ((app (fn (p1,p2) => WebSet.put(set, p1, p2))) pairs; set)
           end
          
    in 
      fun getQuery() : set option = 
        case !form_data of SOME data => data 
                 | NONE => 
          let 
            val content_type = Set.iget (headers(),"content-type")
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
                val form = WebSet.create (getReqRec()) 
                fun storeContentType fv_name (key,value) = 
              WebSet.put(form,
                  Substring.string fv_name ^ "." ^ 
                  lower' (SOME (Substring.string key)),
                  Substring.string value)
                fun storeFV fv =
              case fv of
                large_fv {fv_name,filename,filesize,content,content_types} =>
                (WebSet.put (form, Substring.string fv_name,Substring.string filename);
                 WebSet.put (form, Substring.string fv_name ^ ".filesize", 
                  Int.toString filesize);
                 List.app (storeContentType fv_name) content_types)
              | small_fv {fv_name,content,content_types} =>
                (WebSet.put(form,Substring.string fv_name,Substring.string content);
                 List.app (storeContentType fv_name) content_types)
                val _ = List.app storeFV fvs
              in
                (form_data := SOME (SOME form);
                 getQuery())
              end
              end
            else
            let val formdata = getQueryData(FORM)
              val formset = case formdata of 
                     NONE => NONE
                     | SOME(s) => SOME(parseFormData(NONE, s))
              val getdata = getQueryData(GET)
              val finalset = case getdata of
                     NONE => formset
                     | SOME(s) => SOME(parseFormData(formset, s))
            in (form_data := SOME(finalset); getQuery())
            end
          end

      fun storeMultiformData (fv,filename) =
      let
        val _ = getQuery() (* Make sure that formvariables have been read *)
        val os = BinIO.openOut filename
        fun r exn = (BinIO.closeOut os; raise exn)
        fun storeFV [] = r (Fail ("Web.storeMultiformData. FV " ^ fv ^ " does not exists"))
      | storeFV (x::xs) =
      case x of
        large_fv{fv_name,filename,content,...} =>
        if Substring.string fv_name = fv then 
          BinIO.output (os,Substring.string content) 
        else 
          storeFV xs
      | small_fv{fv_name,...} =>
          if Substring.string fv_name = fv then 
          r (Fail ("Web.storeMultiformData. FV " ^ fv ^ " does not contain a file"))
        else 
          storeFV xs
      in
        storeFV (!multiform_data);
        BinIO.closeOut os
      end
      handle _ => raise Fail ("Web.storeMultiformData. can't open filename: " ^ filename)


    end 

    fun return (s: string) : status = returnHtml(~1,s)
    (*fun return (s: string) : status = 
      prim("@apsml_returnHtml", (~1,s,size s, getReqRec())) *)

    fun returnRedirectWithCode(i: int, s: string) : status = 
      prim("@apsml_returnRedirect",(i, s, getReqRec()))

    fun returnRedirect s = returnRedirectWithCode (302,s)
    (*fun returnRedirect(s: string) : status = 
      prim("@apsml_returnRedirect",(~1, s, getReqRec()))*)

    fun url () : string list = 
      prim("apsml_geturl", getReqRec())

    type status = status
    
    fun redirect(url: string) : status = 
     (log(WebBasics.Notice, "Web.Conn.redirect"); 
     prim("@ap_internal_redirect", (url, getReqRecP()))
     )

    fun port () : int = (prim("apsml_getport", (getReqRec()))) handle Overflow => raise MissingConnection

    fun host () : string = (prim("apsml_gethost",(getReqRec()))) handle Overflow => raise MissingConnection
    
    fun server () : string = (prim("apsml_getserver",(getReqRec()))) handle Overflow => raise MissingConnection

    fun scheme () : string = (prim("apsml_scheme",getReqRec())) handle Overflow => raise MissingConnection

    fun location () = scheme() ^ "://" ^ server() ^ ":" ^ (Int.toString (port()))

    fun peer () : string = prim("apsml_getpeer", (getReqRec()))

    fun write (s: string) : status =
       prim("@ap_rputs", (s,getReqRecP()))
       

    fun formvar s =  case getQuery()
       of SOME set => Set.get(set,s)
        | NONE => NONE

    fun formvarAll s =
      case getQuery() of
        SOME set => Set.getAll(set,s)
      | NONE => []


  end



  
    structure LowMail : WEB_LOW_MAIL = WebLowMail(struct
               type conn = int
               val getReqRec = getReqRec 
               structure Info = Info
               val log = fn s => log(Debug, s) 
               exception Forbidden = Forbidden
               end) 

    structure Cookie : WEB_COOKIE =
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

    fun allCookies() : (string * string) list = (*WebBasics.log(Notice, "allCookies"); *)(
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
      List.map splitNameAndValue (Substring.tokens (fn c => c = #";") (Substring.full cv))
    end
      | _ => raise CookieError "More than one Cookie line in the header")

    fun getCookie cn = List.find (fn (name,value) => cn = name) (allCookies())

    fun getCookieValue cn = 
      case getCookie cn of
        NONE => NONE
      | SOME (n,v) => SOME v

    (* Date must be GMT time, that is, use Date.fromTimeUniv *)
    fun setCookie {name : string, value : string, expiry : Date.date option, 
       domain : string option, path : string option, secure : bool} : unit =
      let 
        fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date
      in
        if name = "" orelse value= ""
    then raise CookieError "Name or value empty in call to setCookie"
        else let val header_out_key = "Set-cookie"
          val header_out_value = String.concat
      [encodeUrl name, "=", encodeUrl value,
       concatOpt "; expires=" (Option.map datefmt expiry),
       concatOpt "; domain=" domain,
       concatOpt "; path=" path,
       "; ", if secure then "secure" else ""]
             in Conn.add_headers(header_out_key,header_out_value)
          end
      end

    val setCookies = app setCookie 

    fun deleteCookie { name : string, path : string option } : unit =
      let val key_out = "Set-cookie"
        val val_out = String.concat[encodeUrl name, "=deleted;",
                "expires=Fri, 11-Feb-77 12:00:00 GMT",
                concatOpt "; path=" path]
    in 
      Conn.add_headers (key_out,val_out) 
    end
  end
      end
  
  structure WebDynlib = Dynlib(Cache)
   


  structure Mail : WEB_MAIL = 
  struct
    type email = {to: string list, cc: string list, 
                  bcc: string list, from: string, 
                  subject: string, body: string,
                  extra_headers: string list}
    datatype CharSet = UTF8 | ISO88591 | USASCII
    val current_server = ref NONE : (LowMail.mailer * LowMail.MX_FQDN) option ref
    val servercache = Cache.get(Cache.String, 
                                Cache.List (Cache.Pair Cache.Int Cache.String),
                                "__WEB_MAIL_DNS_CACHE__",
                                Cache.TimeOut(SOME(Time.fromSeconds(86400)), SOME(1000000)))
    val getserveraddr = Cache.memoizeTime servercache (fn addr => 
            let 
                fun reduce l = 
                  List.foldl 
                  (fn ((_,a1,a2),(nl,time)) => ((a1,LowMail.FQDN_MX_toString a2)::nl,Int.min (time,a1))) 
                  ([],86400) l
                val (li,time) = reduce (LowMail.getFQDN_MX(addr))
            in if li = [] 
               then ([],SOME(Time.fromSeconds(0)))
               else ((List.rev li),(SOME(Time.fromSeconds (LargeInt.fromInt time))))
            end)

    (* Limits; addresses must be like as@as.dk not Varming <ca@a.dk> *)
    fun checkaddr addr = (log(Debug, addr);
      case String.fields (fn x => x = #"@") addr
      of (name::server::[]) => (
          if String.size name > 63 orelse String.size server > 255 
          then false 
          else 
            case getserveraddr server
              of [] => false
               | (x::xr) => true
          )
       | _ => false )

     fun composeheader _ _ [] [] = []
       | composeheader _ _ [] (a::acc) = rev ((a ^ "\r\n") :: acc)
       | composeheader name delim (x::xr) [] = composeheader "" delim xr [name ^ x]
       | composeheader _ delim (x::xr) (a::ar) = 
           if size x + size a + 2 * (size delim) < 998 
           then composeheader "" delim xr ((a ^ delim ^ x) :: ar)
           else composeheader "" delim xr ((" " ^ x) :: (a ^ delim ^ "\r\n") :: ar)

    
    structure Encodings :> sig val headerEncode : int -> string -> (string * int) end = 
      struct 
        fun createLookupAllow n = not (n >= 32 andalso n <= 126) 
        val tableA = ref NONE
        fun lookupAllowTable () = case !tableA 
                                  of NONE => (tableA := SOME(Vector.tabulate(256,createLookupAllow));
                                                      lookupAllowTable ())
                                   | SOME v => v

        fun shouldencode z x = let val table = lookupAllowTable()
                                 exception Done
                             in (Word8Vector.foldl (fn (y,z') => 
                                                    if Vector.sub(table,Word8.toInt y) orelse z' > z
                                                    then raise Done
                                                    else z'+1) 0 
                                               (Byte.stringToBytes x) ; false) handle Done => true
                             end

        fun createLookupChrs n' = let val n = chr n'
                                  in
                                  if (n >= #"a" andalso n <= #"z") orelse
                                     (n >= #"A" andalso n < #"Z" ) orelse
                                     (n >= #"0" andalso n < #"9" ) orelse 
                                      n = #"!" orelse n = #"*" orelse n = #"+"
                                      orelse n = #"-" 
                                  then (Char.toString n,1)
                                  else if n = #" " then ("_",1)
                                  else ((if n' < 16 then "=0" else "=") ^ 
                                         Word8.fmt StringCvt.HEX (Word8.fromInt n'),3)
                                  end

        val tableB = ref NONE
        fun lookupBSTable () = case !tableB
                             of NONE => (tableB := (SOME(Vector.tabulate(256,createLookupChrs)));
                                                      lookupBSTable ())
                              | SOME v => v


        fun enlength x =  let val w8v  = Byte.stringToBytes x
                              val table = lookupBSTable ()
                          in Word8Vector.foldl (fn (x,l) => #2(Vector.sub(table,Word8.toInt x)) + l) 0 w8v
                          end

        fun encode' x = let val table = lookupBSTable () 
                      in String.translate (fn c => #1(Vector.sub(table,(Word8.toInt(Byte.charToByte c))))) x
                      end

        val st = "=?ISO-8859-1?Q?"
        val lst = String.size st
        fun encode sp (d:Substring.substring) = if sp + lst > 70 
                         then let val (s,l) = encode 2 d in ("\r\n  " ^ s,l) end
                         else let val d' = Substring.string d 
                              in if enlength d' < (73 - sp - lst)
                         then let val s = st ^ (encode' d') ^ "?=" in (s, String.size s) end
                         else (fn (x,y) => let val s = st ^ (encode' (Substring.string x)) 
                                           in if Substring.size y > 0 
                                              then let val (s',l) = encode 2 y 
                                                   in (s ^ "=?=\r\n  " ^ s', l)
                                                   end
                                              else (s ^ "?=", String.size s + 2)
                                           end)
                              (Substring.splitAt(d,73-sp-lst))
                              end
        fun headerEncode s x = if shouldencode (76-s) x 
                               then encode s (Substring.full x) 
                               else (x, s + (String.size x))
      end


     fun genheads {to, cc, bcc, from, subject, body, extra_headers} = 
          let fun genhead hname li = foldl (fn (x,y) => y ^ x) "" (composeheader hname ", " li [])
              val to' = genhead "To: " to
              val cc' = genhead "Cc: " cc
              val from' = genhead "From: " from
              val subject' = let val s = "Subject: " 
                             in s ^ (#1(Encodings.headerEncode (String.size s) subject)) ^ "\r\n"
                             end
              val eh = foldl (fn (x,y) => y ^ x ^ "\r\n") "" extra_headers
              fun addbcc x = (x,(to' ^ cc' ^ "Bcc: " ^ x ^ "\r\n" ^ from' ^ eh ^ subject'))
              val nobcc = to' ^ cc' ^ from' ^ eh ^ subject'
              fun tosingle [] = []
                | tosingle ((x,y)::xr) = ([x],y)::(tosingle xr)
          in ((to @ cc, nobcc):: (tosingle (map addbcc bcc)))
          end

     fun addrToServer to = case LowMail.getDefaultServer() 
                           of  SOME v  => SOME v
                             | NONE =>( 
                             case String.fields (fn x => x = #"@") to
                             of (name::server::[]) => 
                               (case getserveraddr server
                               of ((x,y)::xr) => SOME(LowMail.FQDN_MX_fromString y)
                                | _ => NONE)
                              | _ => raise Fail("mail address does not parse:" ^ to))

     (* genserverdict generates a map: server option -> (addr list * header) list     *
      * or in other words: server option -> email list                                *)
     fun genserverdict li = 
            let fun order ord (NONE,NONE) = EQUAL
                  | order ord (_,NONE) = GREATER
                  | order ord (NONE,_) = LESS
                  | order ord (SOME(a),SOME(b)) = ord(a,b)
                val myorder = order LowMail.FQDN_MX_compare
                fun grap [] acc = (acc,[])
                  | grap ((x,y)::[]) a =  (x::a,[])
                  | grap ((x1,y1)::(x2,y2)::xr) a = 
                              case myorder (y1,y2) 
                              of EQUAL => grap ((x2,y2)::xr) (x1::a)
                               | _ => (x1::a, (x2,y2)::xr)
                fun bb (([],header),dict) = dict
                  | bb ((addr::addrlist,header),dict) = 
                       let val (_,server) = addr
                           val elist = getOpt(Binarymap.peek(dict, server),[])
                           val (l1,addrrest) = grap (addr::addrlist) []
                       in 
                         bb ((addrrest,header), Binarymap.insert 
                                   (dict, server, ((l1,header)::elist)))
                       end
                fun aa ((addr : string list,header),dict) = 
                   let val addrlist = Listsort.sort 
                                  (fn ((_,a),(_,b)) => myorder (a,b))
                                          (map (fn x => (x, (addrToServer x))) addr)
                   in bb ((addrlist,header), dict)
                   end
            in 
              foldl aa (Binarymap.mkDict myorder) li
            end
    fun pairmsg ([],_) = []
      | pairmsg ((x::xr),msg) = (x,msg)::pairmsg(xr,msg)

     local 
     fun createLookupTabel n = if n = 10 then ("\r\n",2) (* \n *)
                             else if n = 9 then ("\t",1) (* \t *)
                             else if n = 32 then (" ",1) (* space *)
                             else if n >= 33 andalso n <= 60 then (Char.toString (chr n),1)
                             else if n >= 62 andalso n <= 126 then (Char.toString (chr n),1)
                             else if n < 16 then ("=0" ^ Word8.fmt StringCvt.HEX (Word8.fromInt n),3)
                             else ("=" ^ Word8.fmt StringCvt.HEX (Word8.fromInt n),3)
    
       val lookupTableT = ref NONE
     fun lookupTable () = case !lookupTableT 
                          of NONE => (lookupTableT:=SOME (Vector.tabulate (256,createLookupTabel)); 
                                      lookupTable ())
                           | SOME(v) => v
     
     (* quoted printable implementation (RFC 2045)  *)
     fun trawl f b s = Substring.foldl 
                        (fn (c,n) => f(c,n,(Vector.sub(lookupTable (), ord c)))) b (Substring.full s)
     
     fun countbit (li,s ,n,l,ls,offset) = 
                                 if l > 75 
                                 then (li, n+3+ls + offset, ls) (* =\r\n *)
                                 else (li, n+ls + offset, l)

     fun qpencodebit array (li,s,n,l,ls,offset) =
               if l > 75  (*=\r\n*)
               then (Byte.packString (array,n,Substring.full ("=\r\n"^s));(li,n+3+ls+offset,ls))
               else (Byte.packString (array,n,Substring.full s);(li,n+ls+offset,l))


     fun qpencode' f (cc,([],n,l),(s,ls)) = (case cc of  #"\r" => ([(cc,s,ls)],n,l)
                                                    | #"\n" => f ([],s,n,0,ls,0)
                                                    | #" "  => ([(cc,s,ls)],n,l)
                                                    | #"\t" => ([(cc,s,ls)],n,l)
                                                    | _     => f([],s,n,l+ls,ls,0))
       | qpencode' f (cc,([(cp,sp,lsp)],n,l),(s,ls)) = (
                  if cp = #" " orelse cp = #"\t" then
                  case cc of #"\n" => f([], sp^"=\r\n"^s, n, 0, lsp+1,2+ls)  
                           | #"\r" => ([(cp,sp,lsp),(cc,s,ls)],n,l)
                           | #" "  => f([(cc,s,ls)],sp,n,l+lsp,lsp,0)
                           | #"\t" => f([(cc,s,ls)],sp,n,l+lsp,lsp,0)
                           | _     => let val (_,c1,c2) = f([],sp,n,l+lsp,lsp,0) 
                                      in f([],s,c1,c2+ls,ls,0) 
                                      end
                  else
                  case (cp,cc) of (#"\r",#"\n") => f([],"\r\n",n,0,2,0)
                                | (#"\r",#"\r") => f([(cc,s,ls)],sp,n,l+lsp,lsp,0)
                                | (#"\r",#"\t") => f([(cc,s,ls)],sp,n,l+lsp,lsp,0)
                                | (#"\r",#" ")  => f([(cc,s,ls)],sp,n,l+lsp,lsp,0)
                                | (#"\r",_)     => let val (_,c1,c2) = f([],sp,n,l+lsp,lsp,0) 
                                                   in f([],s,c1,c2+ls,ls,0) 
                                                   end
                                | _ => let val (_,c1,c2) = f([],sp,n,l+lsp,lsp,0) 
                                       in f([],s,c1,c2+ls,ls,0) 
                                       end)
       | qpencode' f (cc ,(([(cpp,spp,lspp),(#"\r",sp,lsp)]),n,l),(s, ls)) = 
            if cpp = #"\t" orelse cpp = #" " then 
            let val (_,c1,c2) = f([],spp,n,l+lspp,lspp,0)
            in (
            case cc of #"\n" => f([],"\t=\r\n"^s, n, 0, lspp+1, 2+ls)
                     | #"\r" => f([(cc,s,ls)], sp,c1,c2+lsp,lsp,0)
                     | #"\t" => f([(cc,s,ls)], sp,c1,c2+lsp,lsp,0)
                     | #" "  => f([(cc,s,ls)], sp,c1,c2+lsp,lsp,0)
                     | _ => let val (_,c3,c4) = f([],sp,c1,c2+lsp,lsp,0)
                            in f([],s,c3,c4+ls,ls,0)
                            end )
             end 
             else raise Fail("Encoding algorithm not working (encoding)")

       | qpencode' _ _ = raise Fail("Encoding algorithm not working (encoding)")


     fun qpencodefinal f b a = let val (li,n,ll) = trawl (qpencode' f) b a
                       fun toHex a = Word8.fmt StringCvt.HEX  (Byte.charToByte a)
                       fun final ([],n,l) = ([],n,l)
                         | final  ((#" ",s,ls)::[],n,l) = f([], "=" ^ (toHex #" "), n, l+3,3,0)
                         | final ((#"\t",s,ls)::[],n,l) = f([], "=0" ^ (toHex #"\t"),n, l+3,3,0)
                         | final ((cc,s,ls)::xr,n,l) = let val (_,c1,c2) = f([],s,n,l+ls,ls,0)
                                                       in final(xr,c1,c2) end
                       in final (li,n,ll)
                       end
     in
     fun qpencode s = let val (_,n,_) =  qpencodefinal countbit ([],0,0) s
                        val array = Word8Array.array (n,Word8.fromInt 0)
                    in (qpencodefinal (qpencodebit array) ([],0,0) s;
                        Byte.unpackString (Word8ArraySlice.full array) (*array,0,NONE*))
                    end
     end
    fun smtpencodeCount (#".",[#"\r",#"\n"],n) = ([],n+4)
      | smtpencodeCount (cc,li,n) = ([],List.length li + n + 1)

    fun smtpencodeEncode array (#".",[#"\r",#"\n"],n) = 
                        (Byte.packString (array,n,Substring.full "\r\n.."); ([],n+4))
      | smtpencodeEncode array (cc,li,n) = 
                        (Byte.packString (array,n,Substring.full (implode (li@[cc]))); 
                         ([],List.length li + n + 1))

    fun smtpencode' f (cc,([],n)) = (case cc 
             of #"\r" => ([cc],n)
              | _ => f(cc,[],n))
      | smtpencode' f (cc,([cp],n)) = (case (cp,cc) 
             of (#"\r",#"\n") => ([cp,cc],n)
              | _ => f(cc,[cp],n))
      | smtpencode' f (cc,(li,n)) = f(cc,li,n)

    fun smtpencodefinal f b a = let val (li,n) = Substring.foldl (smtpencode' f) b a
                                in case List.rev li of [] => (li,n)
                                            | (x::xr) => f(x,xr,n)
                                end
    fun smtpencode s = let val (_,n) = smtpencodefinal smtpencodeCount ([],0) (Substring.full s)
                           val tail = if size s > 1 
                                      then Substring.substring(s,size s - 2, 2)
                                      else Substring.full ""
                           val extra = Substring.compare(Substring.full "\r\n", tail) <> EQUAL
                           val array = Word8Array.array (n+(if extra then 5 else 3),Word8.fromInt 0)
                       in (smtpencodefinal (smtpencodeEncode array) ([],0) (Substring.full s);
                           (if extra then smtpencodeEncode array (#"\n",[#"\r",#"\n",#".",#"\r"],n)
                                     else smtpencodeEncode array (#"\n",[#".",#"\r"],n));
                           Byte.unpackString (Word8ArraySlice.full array) (*array,0,NONE*))
                       end

    fun prepareAndSend charset {to, cc, bcc, from, subject, body, extra_headers} = 
         let val (fromok,frombad) = List.partition checkaddr from
             val msgfrom = if List.length from > 1 
                           then "From address' not valid" 
                           else "From address not valid"
             in if fromok <> []
         then 
           let exception e1 of string
               val _ = log(Debug, body)
               val charsetString = case charset of ISO88591 => "ISO-8859-1"
                                                 | UTF8 => "UTF8"
                                                 | USASCII => "US-ASCII"
               val (to_ready, to_failed) = List.partition checkaddr to   
               val (cc_ready, cc_failed) = List.partition checkaddr cc
               val (bcc_ready, bcc_failed) = List.partition checkaddr bcc
               fun offsetstring i = let val i = LargeInt.toInt i
                                        fun f x = 
                                              let val t = x div 3600
                                                  val m = (x mod 3600) div 60
                                                  val tt = if t < 10 then "0" ^ Int.toString t
                                                           else Int.toString x
                                                  val mm = if m < 10 then "0" ^ Int.toString m
                                                           else Int.toString m
                                              in tt ^ mm
                                              end
                                    in  if i = 0 then "+0000" 
                                        else if i < 12 * 60 * 60 then "-" ^ (f i)
                                             else "+" ^ (f (24*60*60-i))
                                    end
               val datastring = let val n = Date.fromTimeLocal (Time.now())
                                    val off = Date.localOffset ()
                                    val t = Time.toSeconds off
                                in (Date.fmt "%a, %d %b %Y %H:%M:%S " n) ^ (offsetstring t)
                                end
               val failed_msg = "address not valid"
               val serverdict = genserverdict (genheads 
                     {to = to_ready, cc = cc_ready, bcc = bcc_ready, from = from, 
                      subject = subject, body = body, 
                      extra_headers =  "MIME-Version: 1.0"::
                                      ("Content-Type: text/plain; charset=" ^ charsetString) ::
                                       "Content-Transfer-Encoding: quoted-printable"::
                                      ("Date: "^ datastring) ::
                                      ("Message-ID: " ^ "test@changeme") ::
                                      extra_headers})
               fun push (server,tolist,header) = 
                   let val _ = (case !current_server of 
                                 SOME(mailer,s) =>
                                 if LowMail.FQDN_MX_compare(s,server) = EQUAL
                                 then ()
                                 else (LowMail.closeConn mailer; 
                                       current_server:= SOME(LowMail.initConn(server),server))
                               | NONE =>
                                       current_server:= SOME(LowMail.initConn(server),server)
                                       ) handle LowMail.ConnectionErr(m,_,_,_) => raise e1(m)
                   in 
                      LowMail.sendmail (tolist, List.nth(fromok,0), header ^ body, 
                                        #1(valOf(!current_server)))
                   end
               fun mysend(NONE, li) = (map (fn (x,_) => pairmsg (x,"address is not valid")) li) 
                 | mysend(SOME(s),[]) = []
                 | mysend(SOME(s),((x,y)::xr)) = ((
                         let val (_,a1,a2) = push(s,x,y) 
                                handle LowMail.ConnectionErr(m,_,l2,l3) => 
                                             (current_server := NONE; ([],l2,l3))
                         in (a1 @ a2)::(mysend(SOME(s),xr))
                         end) handle e1(msg) => map (fn (c,_) => pairmsg (c,msg)) ((x,y)::xr))
           in List.concat (Binarymap.foldl (fn (_,l,r) => l @ r) [] (Binarymap.map mysend serverdict))
           end
         else pairmsg(to,msgfrom) @ pairmsg(cc,msgfrom) @ pairmsg(bcc,msgfrom)
         end

     fun mail unfold fail botu botf = case (unfold botu,NONE) handle ? => (NONE,SOME ?)
             of (NONE,a) => (case !current_server 
                           of NONE => (case a of NONE => (botu,botf) | SOME(?) => raise ?)
                            | SOME(s,_) => (LowMail.closeConn(s);current_server:= NONE;
                                            (case a of NONE => (botu,botf) | SOME ? => raise ?)))
              | (SOME(e : email,a,charset),_) => 
                             let val b = prepareAndSend charset 
                                              {to = #to(e), cc = #cc(e), bcc = #bcc(e), 
                                               from = [#from(e)], subject = #subject(e),
                                               body = ((smtpencode o qpencode) (#body(e))),
                                               extra_headers = #extra_headers(e)}
                             in if b = [] 
                                then mail unfold fail a botf 
                                else mail unfold fail a (fail (e,b,botf))
                             end
              

(*    fun sendmail {to: string list, cc: string list, bcc: string list,
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
           smtpencode body]
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
           raise Fail ("Failed to send email from " ^ from ^ " using Web.sendmail."))
      end*)

      fun stdEnc () = Option.getOpt(Option.join
                         (Option.map (fn "iso-8859-1" => SOME ISO88591
                                       | "utf-8" => SOME UTF8
                                       | "ascii" => SOME USASCII
                                       | _ => NONE) (Info.configGetValue(Info.Type.String, "standardEmailEncoding"))),ISO88591)
      fun sendmail (e : email) : unit =
               ignore(
                 mail
                   (fn false => NONE
                     | true  => SOME(e, false, stdEnc()))
                   (fn (em,err,()) => WebBasics.log(WebBasics.Warning, String.concat
                                                 ("tried to mail: " :: (List.foldl (fn (x,l) => x::", "::l) [] (#to em)) @
                                                                  [" but I got the following errors: "] @
                                                 (List.foldl (fn ((x,y),z) => "; " :: x :: " " :: y :: z) [] err))))
                   true ())

    fun send {to: string, from: string, subject: string, body: string} : unit =
      sendmail {to=[to],from=from,cc=nil,bcc=nil,subject=subject,
          extra_headers=nil,body=body}
  end

  
    type status = Conn.status

  val log = WebBasics.log

    type quot = Quot.quot
    fun return (q : quot) : status =
        Conn.return(Quot.toString q)

  fun fetchUrlTime (timeout : int) (url : string) = (
           let 
               val (sscheme,r1) = Substring.splitAt (Substring.full url, 7)
               val (shp,r2) = Substring.splitl (fn x => x <> #"/") r1
               val (sserver,sport1) = Substring.splitl (fn x => x <> #":") shp 
               val (_,sport) = Substring.splitAt (sport1, 1) 
                            handle Subscript => (Substring.full "", Substring.full "80")
               val scheme = Substring.string sscheme
               val page = let val a = Substring.string r2 in if a = "" then "/" else a end
               val server = Substring.string sserver
               val port = getOpt(Int.fromString (Substring.string sport),80)
               val _ = if port < 1 orelse port > 65535 then raise Subscript else 0
               val _ = if scheme <> "http://" then raise Subscript else 0
               val _ = if server = "" then raise Subscript else 0
               val r : string = prim("apsml_getpage",(server,page,(port,timeout,getReqRec())))
           in if isNull r then NONE else SOME r
           end ) handle Subscript => (log(Debug, "fetchUrl: subscript raised");NONE)

    fun fetchUrl url = 
           let
               val timeout = getOpt(Info.configGetValue (Info.Type.Int, "FetchUrlTimeOut"),60)
           in
               fetchUrlTime timeout url
           end

    val returnRedirect =
      Conn.returnRedirect 

    fun write (q : quot) : status = 
      Conn.write (Quot.toString q)

    (*fun getMimeType(s: string) : string = prim("apsml_GetMimeType", (s,getReqRec()))*)

    fun returnFileMime mimetype file = Conn.returnFile(~1, mimetype, file )
    fun returnFile file = Conn.returnFile(~1, Mime.addEncoding (Mime.getMime file), file)

    fun exit() = raise Interrupt
    (* By raising Interrupt, the web-server is not killed as it
     * would be if we call OS.Process.exit. Also, handlers can
     * protect the freeing of resources such as file descriptors
     * and database handles. Moreover, region pages are freed as 
     * they should be. *)

    fun schedule' (f : string) (s : string option) (first : int) (interval : int) : unit = 
            let
              val port : int = getOpt(Info.configGetValue (Info.Type.Int, "SchedulePort"),
                                      if Conn.hasConnection () then Conn.port() else 0)
              val _ = if port = 0 then raise Fail "tried to schedule with an invalid port number" 
                      else ()
            in
            case s of NONE =>
            prim("apsml_reg_schedule", (first : int, interval : int, 0,
                                        (f : string ,"localhost",port : int), getReqRec()))
                   | SOME(server) =>
            prim("apsml_reg_schedule", (first : int, interval : int, 0, 
                                        (f : string, server : string, port : int), getReqRec()))
            end

    fun schedule f server first interval = 
                  let fun next t1 t2 = if Time.>=(t1,t2) 
                                       then Time.-(t1, t2) 
                                       else next (Time.+(t1, interval)) t2
                      val fir = LargeInt.toInt (Time.toSeconds(next (Date.toTime first) (Time.now())))
                  in
                    schedule' f server fir (LargeInt.toInt(Time.toSeconds interval))
                  end

    fun deSchedule (f:string) : unit = 
            prim("apsml_reg_schedule", (0,0,1,(f,"",0),getReqRec()))
            
    fun scheduleScript f s i = schedule' f s i i
    fun scheduleDaily f s {hour, minute} = 
       let val now = Date.fromTimeUniv (Time.now())
           val first = ((hour - Date.hour now) mod 24) * 60*60 
                      + ((minute - Date.minute now) mod 60) * 60
       in schedule' f s first (24*60*60)
       end
    fun scheduleWeekly f s {day, hour, minute} = 
       let fun weekday Date.Mon = 0
             | weekday Date.Tue = 1
             | weekday Date.Wed = 2
             | weekday Date.Thu = 3
             | weekday Date.Fri = 4
             | weekday Date.Sat = 5
             | weekday Date.Sun = 6
           val now = Date.fromTimeUniv (Time.now())
           val first = ((weekday day - weekday (Date.weekDay now)) mod 7) * 24 * 60 * 60
                      + ((hour - Date.hour now) mod 24) * 60*60 
                      + ((minute - Date.minute now) mod 60) * 60 
       in schedule' f s first (24*60*60*7)
       end

     (* Creating the supported database interfaces *)
        
     structure DbOraBackend = DbOracleBackend(struct 
                                                 type conn = int
                                                 val getReqRec = getReqRec
                                                 val log = (fn x => (log(Debug, x); x))
                                                 val isNull = isNull
                                                 structure Info = Info
                                                 structure Dynlib = WebDynlib
                                                 end) :> WEB_DB_BACKEND 
                                                 where type 'a Type = 'a Info.Type.Type

     structure DbPostgreSQLBackend = DbODBCBackend(struct 
                                                 type conn = int
                                                 val getReqRec = getReqRec
                                                 val log = (fn x => (log(Debug, x); x))
                                                 val isNull = isNull
                                                 structure Info = Info
                                                 structure Dynlib = WebDynlib
                                                 end) :> WEB_DB_BACKEND 
                                                 where type 'a Type = 'a Info.Type.Type

     structure DbMySqlBackend = DbODBCBackend(struct 
                                                 type conn = int
                                                 val getReqRec = getReqRec
                                                 val log = (fn x => (log(Debug, x); x))
                                                 val isNull = isNull
                                                 structure Info = Info
                                                 structure Dynlib = WebDynlib
                                                 end) :> WEB_DB_BACKEND 
                                                 where type 'a Type = 'a Info.Type.Type

     (*structure DbOra = DbFunctor(structure DbBackend = DbOraBackend)*)

  end
