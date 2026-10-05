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
      val () = check (version = "MLKIT-IR 2" orelse version = "MLKIT-IR 3") "unsupported IR version"
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
                  val kind = if String.isPrefix "$" token then 1 else 0
                  val () = check (kind = 1 orelse List.exists (fn prefix => String.isPrefix prefix token)
                                      ["attop ","atbot ","sat "]) "invalid allocation span"
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
                (check (List.exists (fn k => k = kind) ["function","direct","indirect"])
                   "invalid call kind";
                 calls (Obj [("kind",Str kind),("caller",Str (decoded caller)),
                             ("callee",Str (decoded callee))] :: acc))
            | _ => raise Fail "invalid IR call row")
      val edges = if version = "MLKIT-IR 2" then []
                  else (check (line () = "MLKIT-IR-CALLS 1") "invalid IR calls"; calls [])
      val () = check (!pos = size text) "trailing IR bytes"
    in Obj [("identity",Str identity),("unit",Str unit),("source",Str source),
            ("path",Str path),("text",Str text),("code_start",number codeStart),
            ("code_bytes",number codeBytes),("spans",Arr spans),("calls",Arr edges)]
    end

  fun enrich roots {samples : t list,metadata} =
    let
      val allocations = rows metadata "allocations"
      fun unique key xs =
        let fun add (x,dict) = Binarymap.insert(dict,key x,x)
        in map #2 (Binarymap.listItems (foldl add (Binarymap.mkDict String.compare) xs))
        end
      val sites = unique (fn r => encodeJson (get r "definition")) allocations
      val needed = List.filter (fn r => Option.isSome(find r "ir_identity") andalso
                                 find r "point" <> SOME (Num "0")) sites
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
                             stringField site "unit" = stringField doc "unit"
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
      val documents = unique (fn r => stringField r "identity") (!found)
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
      val used = ref (Binarymap.mkDict String.compare)
      fun locate site =
        let val identity = stringField site "ir_identity"
            val point = find site "point"
            val base = [("definition",get site "definition"),("site",get site "site"),
                        ("unit",get site "unit")]
            fun unavailable reason = Obj(base @ [("status",Str reason),("spans",Arr [])])
        in case point of
            NONE => unavailable "legacy-profile"
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
                   else (used := Binarymap.insert(!used,identity,doc);
                         Obj(base @ [("status",Str "available"),("identity",Str identity),
                                     ("spans",Arr spans)]))
                end)
        end
      val locations = map locate sites
      val fields = case metadata of Obj fs => fs | _ => raise Fail "invalid profile metadata"
    in {samples = samples, metadata = Obj(fields @
         [("ir_sites",Arr locations),("ir_documents",Arr(map #2 (Binarymap.listItems (!used))))])}
    end
end
