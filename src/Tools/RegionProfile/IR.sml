(* Resolve IR companions once while generating a report. No browser filesystem
 * access is needed. All offsets and columns in the embedded data count bytes. *)
structure ProfileIR =
struct
  open ProfileJson
  fun check b message = if b then () else raise Fail message
  fun natural s =
    if size s > 0 andalso List.all Char.isDigit (explode s) then
      (case Int.fromString s of SOME n => n | NONE => raise Fail "IR integer overflow")
    else raise Fail "invalid IR integer"
  fun number n = Num (Int.toString n)
  fun stringField r k = case find r k of SOME (Str s) => s | _ => ""
  fun rows r k = case find r k of SOME (Arr xs) => xs | _ => []
  fun read path =
    let
      val stream = TextIO.openIn path
      val text = (TextIO.inputAll stream before TextIO.closeIn stream)
                 handle e => (TextIO.closeIn stream; raise e)
      val pos = ref 0
      fun line () =
        let val start = !pos
            fun scan i = if i >= size text then raise Fail "truncated IR file"
                         else if String.sub(text,i) = #"\n" then i else scan (i+1)
            val stop = scan start
        in pos := stop+1; String.substring(text,start,stop-start)
        end
      fun field key =
        let val s = line ()
            val prefix = key ^ "\t"
        in check (String.isPrefix prefix s) "invalid IR header";
           String.extract(s,size prefix,NONE)
        end
      fun decoded s = case String.fromString s of SOME v => v | NONE => raise Fail "invalid IR string"
      val version = line ()
      val () = check (version = "MLKIT-IR 2" orelse version = "MLKIT-IR 3" orelse version = "MLKIT-IR 4" orelse version = "MLKIT-IR 5" orelse version = "MLKIT-IR 6" orelse version = "MLKIT-IR 7") "unsupported IR version"
      val identity = field "identity"
      val unit = decoded (field "unit")
      val source = decoded (field "source")
      val objectDigest = field "object-md5"
      val digestStart = !pos + size "content-md5\t"
      val digest = field "content-md5"
      val () = check (size digest = 32 andalso size objectDigest = 32) "invalid IR digest"
      val normalized = String.substring(text,0,digestStart) ^
                       String.implode(List.tabulate(32,fn _ => #"0")) ^
                       String.extract(text,digestStart+32,NONE)
      val () = check (MD5.fromString normalized = digest) "IR content mismatch"
      val codeBytes = natural (field "code-bytes")
      val codeStart = !pos
      val () = check (codeBytes <= size text-codeStart) "truncated IR code"
      val codeEnd = codeStart+codeBytes
      val () = pos := codeEnd
      val () = check (line () = "" andalso line () = "MLKIT-IR-LOCATIONS 1" andalso
                      line () = "mark\tstart\tlength\tline\tcolumn") "invalid IR table"
      fun advance (p,row,col) target =
        if p = target then (p,row,col)
        else if String.sub(text,p) = #"\n" then advance (p+1,row+1,1) target
        else advance (p+1,row,col+1) target
      fun table position acc =
        case line () of
          "MLKIT-IR-LOCATIONS-END" => rev acc
        | s =>
          (case map natural (String.fields (fn c => c = #"\t") s) of
            [mark,start,len,row,col] =>
              let val (previous,_,_) = position
                  val () = check (start >= previous andalso start >= codeStart andalso
                            start <= codeEnd andalso len <= codeEnd-start) "IR span out of bounds"
                  val next as (_,r,c) = advance position start
                  val () = check (row = r andalso col = c) "IR line/column mismatch"
                  val token = String.substring(text,start,len)
                  val isAllocation = List.exists (fn prefix => String.isPrefix prefix token)
                                       ["attop ","atbot ","sat "]
                  (* Foreign calls can use a friendly primitive name or infix
                   * operator. Their marks still identify the call token. *)
                  val kind = if isAllocation then 0 else 1
                  val () = check (isAllocation orelse
                    (size token > 0 andalso not (List.exists Char.isSpace (String.explode token))))
                    "invalid allocation span"
              in table next (Obj [("mark",number mark),("start",number start),
                    ("length",number len),("line",number row),("column",number col),
                    ("location_kind",number kind)] :: acc)
              end
          | _ => raise Fail "invalid IR table row")
      val spans = table (codeStart,8,1) []
      fun calls acc =
        case line () of
            "MLKIT-IR-CALLS-END" => rev acc
          | s => (case String.fields (fn c => c = #"\t") s of
              [kind,caller,callee] =>
                (check (List.exists (fn k => k = kind) ["function","direct","indirect","closure"])
                   "invalid call kind";
                 calls (Obj [("kind",Str kind),("caller",Str (decoded caller)),
                             ("callee",Str (decoded callee))] :: acc))
            | _ => raise Fail "invalid IR call row")
      val edges = if version = "MLKIT-IR 2" then []
                  else (check (line () = "MLKIT-IR-CALLS 1") "invalid IR calls"; calls [])
      fun regionRows acc =
        case line () of
            "MLKIT-IR-REGIONS-END" => rev acc
          | s =>
            let val fields = map decoded (String.fields (fn c => c = #"\t") s)
                fun num n = (ignore (natural n); Str n)
                val row = case fields of
                    ["region",id,role,owner,position] =>
                      (check (role = "formal" orelse role = "local") "invalid region role";
                       if role = "formal" then ignore(natural position) else check (position = "") "invalid local position";
                       Obj [("kind",Str "region"),("region",num id),("role",Str role),
                            ("owner",Str owner),("position",Str position)])
                  | ["flow",caller,callee,position,actual,mode,point] =>
                      (check (List.exists (fn m => m = mode) ["attop","atbot","sat"]) "invalid flow mode";
                       Obj [("kind",Str "flow"),("caller",Str caller),("callee",Str callee),
                            ("position",num position),("actual",num actual),("mode",Str mode),("point",num point)])
                  | ["flow",caller,callee,position,actual,mode,point,occurrence] =>
                      (check (List.exists (fn m => m = mode) ["attop","atbot","sat"]) "invalid flow mode";
                       Obj [("kind",Str "flow"),("caller",Str caller),("callee",Str callee),
                            ("position",num position),("actual",num actual),("mode",Str mode),
                            ("point",num point),("occurrence",num occurrence)])
                  | ["function",label,parent,flavor] =>
                      (check (flavor = "named" orelse flavor = "anonymous") "invalid function flavor";
                       Obj [("kind",Str "function"),("label",Str label),("parent",Str parent),("flavor",Str flavor)])
                  | ["point",point,region] =>
                      Obj [("kind",Str "point"),("point",num point),("region",num region)]
                  | _ => raise Fail "invalid region-flow row"
            in regionRows (row::acc)
            end
      val regionData = if version = "MLKIT-IR 5" orelse version = "MLKIT-IR 6" orelse version = "MLKIT-IR 7" then
            (check (line () = "MLKIT-IR-REGIONS 1") "invalid region-flow table"; regionRows [])
          else []
      val () = check (!pos = size text) "trailing IR bytes"
    in Obj [("identity",Str identity),("unit",Str unit),("source",Str source),
            ("path",Str path),("text",Str text),("code_start",number codeStart),
            ("code_bytes",number codeBytes),("spans",Arr spans),("calls",Arr edges),("closure_edges",Bool (version = "MLKIT-IR 4" orelse version = "MLKIT-IR 5" orelse version = "MLKIT-IR 6" orelse version = "MLKIT-IR 7")),
            ("region_data",Arr regionData),("region_flow",Bool (version = "MLKIT-IR 5" orelse version = "MLKIT-IR 6" orelse version = "MLKIT-IR 7"))]
    end

  (* Resolve formal parameters by native label and ordinal, never by the numeric
   * region identity observed in the importing compilation unit. *)
  fun assemble documents manifest needed =
    let
      val nodes = ref (Binarymap.mkDict String.compare)
      val formals = ref (Binarymap.mkDict String.compare)
      val issues = ref ([] : string list)
      fun warn s = issues := s :: !issues
      fun field r k = stringField r k
      fun global r = case Int.fromString r of SOME n => n >= 1 andalso n <= 7 | NONE => false
      fun key unit r = encodeJson (Arr [Str (if global r then "<global>" else unit),Str r])
      fun ensure doc r =
        let val unit = field doc "unit"
            val id = key unit r
        in if Option.isSome(Binarymap.peek(!nodes,id)) then ()
           else nodes := Binarymap.insert(!nodes,id,Obj [("id",Str id),("unit",Str (if global r then "<global>" else unit)),
                  ("region",Str r),("owner",Str ""),("role",Str (if global r then "global" else "unknown")),
                  ("source",Str (field doc "source"))]);
           id
        end
      fun formalKey owner position = encodeJson (Arr [Str owner,Str position])
      fun declare doc row =
        if field row "kind" <> "region" then ()
        else
          let val id = ensure doc (field row "region")
              val node = Obj [("id",Str id),("unit",Str (field doc "unit")),
                ("region",Str (field row "region")),("owner",Str (field row "owner")),
                ("position",Str (field row "position")),("role",Str (field row "role")),("source",Str (field doc "source"))]
              val () = case Binarymap.peek(!nodes,id) of
                  SOME old => if field old "role" = "unknown" orelse old = node then ()
                              else warn ("Conflicting region definition: " ^ id)
                | NONE => ()
              val () = nodes := Binarymap.insert(!nodes,id,node)
          in if field row "role" <> "formal" then ()
             else let val k = formalKey (field row "owner") (field row "position")
                      val prior = case Binarymap.peek(!formals,k) of SOME xs => xs | NONE => []
                  in formals := Binarymap.insert(!formals,k,if List.exists (fn n => n = id) prior then prior else id::prior)
                  end
          end
      val () = app (fn doc => app (declare doc) (rows doc "region_data")) documents
      val edges = ref []
      val points = ref []
      fun connect doc row =
        case field row "kind" of
            "point" => points := Obj [("identity",Str (field doc "identity")),("point",Str (field row "point")),
                          ("node",Str (ensure doc (field row "region")))] :: !points
          | "flow" =>
              let val actual = ensure doc (field row "actual")
                  val k = formalKey (field row "callee") (field row "position")
                  val choices = case Binarymap.peek(!formals,k) of SOME xs => xs | NONE => []
                  val localChoices = List.filter (fn id => case Binarymap.peek(!nodes,id) of
                      SOME n => field n "unit" = field doc "unit" | NONE => false) choices
                  val choices = if null localChoices then choices else localChoices
              in case choices of
                  [formal] => edges := Obj [("formal",Str formal),("actual",Str actual),
                    ("caller",Str (field row "caller")),("callee",Str (field row "callee")),
                    ("identity",Str (field doc "identity")),("point",Str (field row "point")),
                    ("position",Str (field row "position")),("occurrence",Str (field row "occurrence")),
                    ("mode",Str (field row "mode"))] :: !edges
                | _ => warn ("Unresolved formal region: " ^ field row "callee" ^ " parameter " ^ field row "position")
              end
          | _ => ()
      val () = app (fn doc => app (connect doc) (rows doc "region_data")) documents
      val () = if null manifest then warn "No linked-object manifest; connecting compilation units may be missing." else ()
      val () = app (fn entry => if List.exists (fn d => field entry "ir_identity" = field d "identity") documents then ()
                 else warn ("Missing or mismatched IR: " ^ field entry "ir_object")) needed
      val () = app (fn d => if find d "region_flow" = SOME (Bool true) then ()
                 else warn ("No region-flow metadata: " ^ field d "unit")) documents
    in Obj [("available",Bool (List.exists (fn d => find d "region_flow" = SOME (Bool true)) documents)),
            ("nodes",Arr (map #2 (Binarymap.listItems (!nodes)))),("edges",Arr (rev (!edges))),
            ("points",Arr (rev (!points))),("issues",Arr (map Str (rev (!issues))))]
    end

  fun enrich roots {samples : t list,metadata} =
    let
      val allocations = rows metadata "allocations"
      fun siteMark site =
        if find site "location_kind" = SOME (Num "2") then SOME (Num "0") else find site "site"
      fun unique key xs =
        let fun add (x,dict) = Binarymap.insert(dict,key x,x)
        in map #2 (Binarymap.listItems (foldl add (Binarymap.mkDict String.compare) xs))
        end
      val sites = unique (fn r => encodeJson (get r "definition")) allocations
      val manifest = rows metadata "ir_objects"
      val neededSites = List.filter (fn r => Option.isSome(find r "ir_identity") andalso
                                 siteMark r <> SOME (Num "0")) sites
      val needed = neededSites @ manifest
      val paths = unique (fn s => s)
        (List.mapPartial (fn r => case stringField r "ir_object" of
            "" => NONE | path => SOME (path ^ ".ir")) needed)
      val cache = ref (Binarymap.mkDict String.compare)
      fun candidate path =
        let val key = OS.Path.mkCanonical(OS.Path.mkAbsolute {path = path,relativeTo = OS.FileSys.getDir()})
        in case Binarymap.peek(!cache,key) of
            SOME result => result
          | NONE => let val result = (SOME (read key) handle _ => NONE)
                    in cache := Binarymap.insert(!cache,key,result); result
                    end
        end
      val candidates = List.mapPartial candidate paths
      fun matches site doc = stringField site "ir_identity" = stringField doc "identity" andalso
                             (stringField site "unit" = "" orelse stringField site "unit" = stringField doc "unit")
      val missing = List.filter (fn site => not(List.exists (matches site) candidates)) needed
      val found = ref candidates
      val visited = ref (Binarymap.mkDict String.compare)
      fun visit path =
        ((let val path = OS.FileSys.fullPath path
          in if Option.isSome(Binarymap.peek(!visited,path)) then ()
             else (visited := Binarymap.insert(!visited,path,());
               if OS.FileSys.isDir path then
                 let val dir = OS.FileSys.openDir path
                     fun loop () = case OS.FileSys.readDir dir of NONE => ()
                                   | SOME name => (visit(OS.Path.concat(path,name)); loop ())
                 in (loop () before OS.FileSys.closeDir dir)
                    handle e => (OS.FileSys.closeDir dir; raise e)
                 end
               else if String.isSuffix ".o.ir" path then
                 (case candidate path of
                    SOME doc => if List.exists (fn site => matches site doc) missing then
                                  found := doc :: !found else ()
                  | NONE => ())
               else ())
          end) handle OS.SysErr _ => ())
      val () = if null missing then () else app visit roots
      val documents = unique (fn r => stringField r "identity")
        (List.filter (fn doc => List.exists (fn entry => matches entry doc) needed) (!found))
      fun spanKey mark kind = encodeJson mark ^ ":" ^ encodeJson kind
      fun index doc =
        let fun add (span,dict) =
              let val key = spanKey (get span "mark") (get span "location_kind")
                  val prior = case Binarymap.peek(dict,key) of SOME xs => xs | NONE => []
              in Binarymap.insert(dict,key,span::prior)
              end
        in (doc,foldr add (Binarymap.mkDict String.compare) (rows doc "spans"))
        end
      val indexed = foldl (fn (doc,dict) => Binarymap.insert(dict,stringField doc "identity",index doc))
                          (Binarymap.mkDict String.compare) documents
      fun locate site =
        let val identity = stringField site "ir_identity"
            val point = siteMark site
            val base = [("definition",get site "definition"),("site",get site "site"),
                        ("unit",get site "unit")]
            fun unavailable reason = Obj(base @ [("status",Str reason),("spans",Arr [])])
        in case point of
            NONE => unavailable "missing-location"
          | SOME (Num "0") => unavailable "generated"
          | SOME mark =>
              (case Binarymap.peek(indexed,identity) of
                NONE => unavailable "missing-or-mismatched-ir"
              | SOME (doc,index) =>
                let val spans = if stringField doc "unit" <> stringField site "unit" then []
                      else case Binarymap.peek(index,spanKey mark (get site "location_kind")) of
                          SOME xs => xs | NONE => []
                in if stringField doc "unit" <> stringField site "unit" then unavailable "missing-or-mismatched-ir"
                   else if null spans then unavailable "missing-mark"
                   else (Obj(base @ [("status",Str "available"),("identity",Str identity),
                                     ("spans",Arr spans)]))
                end)
        end
      val locations = map locate sites
      val fields = case metadata of Obj fs => fs | _ => raise Fail "invalid profile metadata"
    in {samples = samples, metadata = Obj(fields @
         [("ir_sites",Arr locations),("ir_documents",Arr documents),("region_flow",assemble documents manifest needed)])}
    end
end
