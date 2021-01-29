structure Json :> JSON = struct
  structure M = StringMap
  fun die s = raise Fail ("Json: " ^ s)

  datatype t = RAW of string
             | OBJECT of obj
             | STRING of string | ARRAY of t list | NULL
             | BOOL of bool | NUMBER of string
  withtype obj = t M.map

  (* Operations on object maps *)
  fun objFromList l = List.foldl (fn ((k,v),obj) => M.add(k,v,obj)) M.empty l
  fun objFromKeyValues l = List.foldl (fn ((k,v),obj) => M.add(k,STRING v,obj)) M.empty l
  fun objLook obj k = M.lookup obj k
  fun objFold (f : (string * t) * 'a -> 'a) (acc : 'a) (obj:obj) : 'a =
      M.Fold (fn ((k,v),a) => f((k,v),a)) acc obj
  fun objList obj = M.list obj
  fun objAdd obj k v = M.add(k,v,obj)
  val objEmp : obj = M.empty

  local
    fun to_str (RAW s) acc = s :: acc
      | to_str (STRING s) acc = "\"" :: s ::  "\"" :: acc
      | to_str (OBJECT objs) acc =
        let fun loop nil acc = acc
              | loop [(l,js)] acc = to_str js ("\":" :: l :: "\"" :: acc)
              | loop ((l,js)::objs) acc = loop objs (", " :: to_str js ("\":" :: l :: "\"" :: acc))
        in "}" :: loop (objList objs) ("{"::acc)
        end
      | to_str (ARRAY jss) acc =
        let fun loop nil acc = acc
              | loop [js] acc = to_str js acc
              | loop (js::jss) acc = loop jss (", " :: to_str js acc)
        in "]" :: loop jss ("["::acc)
        end
      | to_str NULL acc = "null" :: acc
      | to_str (BOOL true) acc = "true" :: acc
      | to_str (BOOL false) acc = "false" :: acc
      | to_str (NUMBER s) acc = s :: acc
  in
    fun toString js =
        String.concat(rev(to_str js nil))
  end

  fun isSym #"[" = true
    | isSym #"]" = true
    | isSym #"," = true
    | isSym #":" = true
    | isSym #"{" = true
    | isSym #"}" = true
    | isSym _ = false

  datatype token = Id of string | Str of string | Sym of char | Num of string
  fun pp_token t =
      case t of
          Id s => "Id(" ^ s ^")"
        | Str s => "Str(" ^ s ^ ")"
        | Sym c => "Sym(" ^ String.str c ^ ")"
        | Num s => "Num(" ^ s ^ ")"
  fun pp_tokens ts = String.concatWith "," (List.map pp_token ts)

  type stream = string * int
  fun streamFromString (s:string) : stream = (s,0)
  fun getc ((s,n):stream):(char*stream)option = if n >= size s then NONE
                                                else SOME(String.sub(s,n),(s,n+1))
  fun pos ((s,n):stream):int = n
  fun extract ((s,n0),(_,n1)) = String.substring(s,n0,n1-n0)

  fun lex_id (cs0,cs,ts) =
      case getc cs of
          NONE => (cs, Id (extract(cs0,cs)) :: ts)
        | SOME (c,cs1) => if Char.isAlphaNum c orelse c = #"_" then lex_id(cs0,cs1,ts)
                          else (cs,Id (extract(cs0,cs)) :: ts)

  fun lex_str (cs0,cs,ts) =
      case getc cs of
          NONE => die "lexer found unclosed string"
        | SOME (#"\"",cs1) => (cs1,Str(extract(cs0,cs))::ts)
        | SOME (_,cs) => lex_str(cs0,cs,ts)

  fun lex_num0 (xs,cs0,ts) =
      case getc cs0 of
          NONE => die "lex_num0 error 2"
        | SOME (#"0",cs) => lex_num_dot(#"0"::xs,cs,ts)
        | SOME (c,cs) => if Char.isDigit c then lex_num(c::xs,cs,ts)
                         else die "lex_num0 error"

  and lex_num (xs,cs0,ts) =
      case getc cs0 of
          NONE => (cs0,rev(Num(implode(rev xs))::ts))
        | SOME (c,cs) => if Char.isDigit c then lex_num(c::xs,cs,ts)
                         else lex_num_dot(xs,cs0,ts)

  and lex_num_dot (xs,cs0,ts) =
      case getc cs0 of
          NONE => (cs0,rev(Num(implode(rev xs))::ts))
        | SOME (c,cs) => if c = #"." then lex_num_frac(c::xs,cs,ts)
                         else lex_e(xs,cs0,ts)

  and lex_num_frac (xs,cs0,ts) =
      case getc cs0 of
          NONE => die "lex_num_frac error 2"
        | SOME (c,cs) => if Char.isDigit c then lex_num_frac1(c::xs,cs,ts)
                         else die "lex_num_frac error"

  and lex_num_frac1 (xs,cs0,ts) =
      case getc cs0 of
          NONE => (cs0,rev(Num(implode(rev xs))::ts))
        | SOME (c,cs) => if Char.isDigit c then lex_num_frac1(c::xs,cs,ts)
                         else lex_e(xs,cs0,ts)

  and lex_e (xs,cs0,ts) =
      case getc cs0 of
          NONE => (cs0,rev(Num(implode(rev xs))::ts))
        | SOME (c,cs) => if c = #"e" orelse c= #"E" then lex_pm(c::xs,cs,ts)
                         else (cs0,Num(implode(rev xs)) :: ts)

  and lex_pm (xs,cs0,ts) =
      case getc cs0 of
          NONE => die "lex_pm error"
        | SOME (c,cs) => if c = #"+" orelse c= #"-" then lex_pmd(c::xs,cs,ts)
                         else lex_pmd(xs,cs0,ts)

  and lex_pmd (xs,cs0,ts) =
      case getc cs0 of
          NONE => die "lex_pmd error 2"
        | SOME (c,cs) => if Char.isDigit c then lex_pmd2(c::xs,cs,ts)
                         else die "lex_pmd error"

  and lex_pmd2 (xs,cs0,ts) =
      case getc cs0 of
          NONE => (cs0,rev(Num(implode(rev xs))::ts))
        | SOME (c,cs) => if Char.isDigit c then lex_pmd2(c::xs,cs,ts)
                         else (cs0,Num(implode(rev xs))::ts)

  fun lex (cs0,ts) =
      case getc cs0 of
          NONE => rev ts
        | SOME (c,cs) =>
          lex(if Char.isSpace c then (cs,ts)
              else if isSym c then (cs,Sym c::ts)
              else if Char.isAlpha c orelse c = #"_" then lex_id(cs0,cs,ts)
              else if c = #"\"" then lex_str(cs,cs,ts)
              else if c = #"-" then lex_num0([c],cs,ts)
              else if c = #"0" then lex_num_dot([c],cs,ts)
              else if Char.isDigit c then lex_num([c],cs,ts)
              else die "lexing error")

  fun parse_jsons pj jsons ts =
      case pj ts of
          (json, Sym #"," :: ts) => parse_jsons pj (json::jsons) ts
        | (json, Sym #"]" :: ts) => (rev(json::jsons),ts)
        | _ => die "parser expecting ',' or ']'"

  fun parse_json ts =
      case ts of
          Str v :: ts => (STRING v, ts)
        | Id "null" :: ts => (NULL, ts)
        | Id "true" :: ts => (BOOL true, ts)
        | Id "false" :: ts => (BOOL false, ts)
        | Num n :: ts => (NUMBER n, ts)
        | Sym #"[" :: Sym #"]" :: ts => (ARRAY nil, ts)
        | Sym #"[" :: ts =>
          (case parse_jsons parse_json nil ts of
               (jsons, ts) => (ARRAY jsons, ts))
        | Sym #"{" :: ts =>
          (case parse_kvs objEmp ts of
               (kvs, Sym #"}" :: ts) => (OBJECT kvs, ts)
             | _ => die "parser expecting '}'")
        | _ => die "parsing expecting json"

  and parse_kvs acc (Id k :: Sym #":" :: ts) =
      (case parse_json ts of
           (json, ts) => parse_kvs' (objAdd acc k json) ts)
    | parse_kvs acc (Str k :: Sym #":" :: ts) =
      (case parse_json ts of
           (json, ts) => parse_kvs' (objAdd acc k json) ts)
    | parse_kvs acc ts = (acc, ts)

  and parse_kvs' acc ts =
    case ts of
       Sym #","::ts => parse_kvs acc ts
     | _ => (acc,ts)

  fun fromString s =
      let val ts = lex(streamFromString s,nil)
      in case parse_json ts of
             (json,nil) => json
           | (_,t::ts) => die "fromString.garbage after json"
      end

  fun foldl_jsons pj f acc ts =
      case pj ts of
          (json, Sym #"," :: ts) => foldl_jsons pj f (f(json,acc)) ts
        | (json, Sym #"]" :: nil) => f(json,acc)
        | (json, Sym #"]" :: _) => die "parsefolder found garbage after array"
        | _ => die "parsefolder expecting ',' or ']'"

  fun foldlArrayJson f acc s =
      let val ts = lex(streamFromString s,nil)
      in case ts of
             Sym #"[" :: Sym #"]" :: nil => acc
           | Sym #"[" :: ts => foldl_jsons parse_json f acc ts
           | _ => die "parsefolder expecting '['"
      end

  fun fromKeyValues kvs =
      OBJECT (objFromKeyValues kvs)

  fun getFromJsonObj json k =
      case json of
          OBJECT kvs => M.lookup kvs k
        | _ => die "getFomJsonObj.expects object"

  fun foldlArray f a json =
      case json of
          ARRAY js => List.foldl f a js
        | _ => die "foldlArray.expects array"

  fun foldrArray f a json =
      case json of
          ARRAY js => List.foldr f a js
        | _ => die "foldrArray.expects array"

  fun getBool json k =
      case getFromJsonObj json k of
          SOME (BOOL b) => b
        | SOME _ => die "getBool.wrong type"
        | NONE => die "getBool.missing key"

  fun getString json k =
      case getFromJsonObj json k of
          SOME (STRING s) => s
        | SOME _ => die "getString.wrong type"
        | NONE => die ("getString.missing key " ^ k ^ " in " ^ toString json)

  fun getStringOpt json k v =
      case getFromJsonObj json k of
          SOME (STRING s) => s
        | SOME _ => die "getStringOpt.wrong type"
        | NONE => v
end
