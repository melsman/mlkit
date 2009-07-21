
signature MAP = sig
  type 'a t
  val emp    : unit -> 'a t
  val insert : 'a t * string * 'a -> 'a t
  val lookup : 'a t * string -> 'a option
  val list   : 'a t -> (string * 'a) list
  val dom    : 'a t -> string list
  val plus   : 'a t * 'a t -> 'a t
  val argsForWhich : 'a t -> ('a -> bool) -> string list
  val single : string * 'a -> 'a t
end

structure Map :> MAP = struct
  type 'a t = (string * 'a) list
  fun emp() = []
  fun insert (m,a,b) = (a,b)::m
  fun lookup (m,a) = 
      case List.find (fn (b,_) => a = b) m of
        SOME (_,x) => SOME x
      | NONE => NONE
  fun list a = a
  fun dom a = List.map (fn (x,y) => x) a
  val plus = op @
  fun single (a,b) = [(a,b)]
  fun argsForWhich (m:'a t) (p : 'a -> bool) : string list =
      map #1 (List.filter (fn (a,b) => p b) m)
end

(* Concatenable strings *)
datatype cstring = $ of string | & of cstring * cstring | CSeq of cstring list
infix 6 &  (* as ^ *)
fun stringOfCString ($ s) = s
  | stringOfCString cs =
    let fun loop ($ s,a) = s::a
          | loop (cs1 & cs2,a) = loop(cs1,loop(cs2,a))
          | loop (CSeq(cs::css),a) = loop(cs,loop(CSeq css,a))
          | loop (CSeq nil,a) = a
    in concat(loop(cs,nil))
    end
fun cconcat l = CSeq l
fun cconcatWith s [cs] = cs
  | cconcatWith s nil = $""
  | cconcatWith s (cs::css) =
    cs & ($s) & cconcatWith s css

(* Lexing of sml tokens *)
signature LEX = sig
  type id = string
  type excon = id
  type tycon = id
  type strid = id
  type sigid = id
  datatype t = ID of id
             | SEP of string
             | SPACE of string
             | COMMENT of string
             | KW of string
             | LINK of {href:string,elem:string}
  val lex : string -> t list
  val defs : t list -> id list * excon list * tycon list * (strid*sigid) list
  val conv : (strid -> sigid) -> t list -> t list
  val isKw : string -> bool
  val pp  : t list -> cstring
end

fun htmlencode s : string =
    let fun enc #"<" = "&lt;"
	  | enc #">" = "&gt;"
	  | enc #"&" = "&amp;"
	  | enc #"\"" = "&quot;"
	  | enc c = String.str c
    in String.translate enc s 
    end

fun taga t a e = $("<" ^ t ^ a ^ ">") & e & $("</" ^ t ^ ">") 
fun tag t e = taga t "" e
fun tag0 t = $("<" ^ t ^ " />") 

fun encode id =
    let val chars = "#%&/<>-_+'~^:!@$=?|"
      fun i_to_s i = if i < 10 then "0" ^ Int.toString i else Int.toString i
      fun f c = case CharVector.findi (fn (_,e) => c = e) chars of
                  SOME (i,_) => "$" ^ i_to_s i
                | NONE => String.str c
    in String.translate f id
    end

fun init_space s = 
    (Char.isSpace(String.sub(s,0)))
    handle _ => false

fun remove_init_ws s =
    let fun loop n = 
            if Char.isSpace(String.sub(s,n)) then loop (n+1)
            else n
    in String.extract(s,loop 0,NONE)
    end handle _ => ""

structure Lex : LEX =
struct
type id = string
type excon = id
type tycon = id
type strid = id
type sigid = id
datatype t = ID of id
           | SEP of string
           | SPACE of string
           | COMMENT of string
           | KW of string
           | LINK of {href:string,elem:string}

fun sOf (ID s) = $ s
  | sOf (SEP s) = $ s
  | sOf (SPACE s) = $ s
  | sOf (COMMENT s) = $ s
  | sOf (KW s) = tag "b" ($s)
  | sOf (LINK {href,elem}) = taga "a" (" href='" ^ href ^ "'") ($elem)

(* signature level key words *)
fun isKw s =
    case s of
      "and" => true
    | "eqtype" => true
    | "end" => true
    | "exception" => true
    | "sig" => true
    | "signature" => true
    | "structure" => true
    | "type" => true
    | "datatype" => true
    | "val" => true
    | "include" => true
    | _ => false

fun spaces t =
    case t of
      SPACE s :: rest => 
      (case spaces rest of
         NONE => SOME(s,rest)
       | SOME (sp,r) => SOME(s^sp,r))
    | _ => NONE

fun valId t =
    case t of
      ID "val" :: t =>
      (case spaces t of
         SOME(space, ID id :: rest) => SOME (SPACE space,id,rest)
       | _ => NONE)
    | _ => NONE

fun longid id =
    case String.fields (fn c => c = #".") id of
      [id1,id2] => SOME (id1,id2)
    | _ => NONE

fun lookupStructure m id =
    m id

fun conv m t =
    let fun conv0 t =
            case valId t of
              SOME(sp,id,rest) =>
              KW "val" :: sp :: LINK{href="#" ^ encode id,
                                     elem=id} :: conv0 rest
            | NONE =>
              case t of
                nil => nil
              | ID id :: rest =>
                if isKw id then KW id :: conv0 rest
                else 
                  (case longid id of
                     SOME(id1,id2) =>
                     let val signat = lookupStructure m id1
                       val file = signat ^ ".sml.html"
                       val href1 = file ^ "#" ^ "$S" ^ id1 
                       val href2 = file ^ "#" ^ "$T" ^ id2 
                     in
                       LINK{href=href1,elem=id1} :: ID "." ::
                       LINK{href=href2,elem=id2} :: conv0 rest
                     end
                   | NONE => ID id :: conv0 rest)
              | e :: rest => e :: conv0 rest
    in conv0 t
    end

fun pp ts = cconcat(map sOf ts)

type pos = int
type level = int

type stream = string * pos

fun getc (s,p) =
    if String.size s > p then
      SOME(String.sub(s,p),(s,p+1))
    else NONE

fun read_chars (f:char->bool) (is:stream) (C:string -> t) =
    let fun read is a =
            case getc is of
              SOME(c,is') => 
              if f c then
                read is' (c::a)
              else if a <> [] then 
                SOME(C(String.implode(rev a)),is)
              else NONE
            | NONE => 
              if a <> [] then 
                SOME(C(String.implode(rev a)),is)
              else NONE
    in read is []
    end

fun read_id is = 
    let fun isIdChar c = 
            Char.isAlpha c orelse Char.isDigit c orelse c = #"'" orelse c = #"_" orelse c = #"."
    in read_chars isIdChar is ID
    end

fun read_symb is =
    let val symbolChars = "@$/*-+<>!#%?^~:|="
        fun isSymbolChar c = CharVector.exists (fn c' => c=c') symbolChars
    in read_chars isSymbolChar is ID
    end

fun read_sep is =
    let val sepChars = ",;{}[]()"
        fun isSepChar c = CharVector.exists (fn c' => c=c') sepChars
    in case getc is of
         SOME(c,is') =>
         if isSepChar c then SOME(SEP(String.str c),is')
         else NONE
       | NONE => NONE
    end

fun read_space is =
    read_chars Char.isSpace is SPACE

fun read_comment is =
    let fun read lev is a =
            case getc is of
              SOME(#"(",is1) =>
              (case getc is1 of
                 SOME(#"*",is2) =>
                 read (lev+1) is2 (#"*":: #"("::a)
               | _ => read lev is1 (#"("::a))
            | SOME(#"*",is1) =>
              (case getc is1 of
                 SOME(#")",is2) =>
                 let val a = #")":: #"*"::a
                 in if lev=1 then
                      SOME(COMMENT(implode(rev a)),is2)
                    else read (lev-1) is2 a
                 end
               | _ => read lev is1 (#"*"::a))
            | SOME(c,is1) => read lev is1 (c::a)
            | NONE => raise Fail "immature end of comment"
    in case getc is of
         SOME(#"(",is1) =>
         (case getc is1 of
            SOME(#"*",is2) =>
            read 1 is2 [#"*", #"("]
          | _ => NONE)
       | _ => NONE
    end

fun eos (s,i) = i >= String.size s

fun lex s =
    let fun read (is:stream) (a:t list) : t list =           
            case read_space is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_comment is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_sep is of
              SOME(t,is) => read is (t::a)
            | NONE =>
            case read_symb is of
              SOME(t,is) => read is (t::a)
            | NONE => 
            case read_id is of
              SOME(t,is) => read is (t::a)
            | NONE => 
              if eos is then rev a
              else raise Fail "Error reading string"
    in read (s,0) nil
    end

fun defs ts = 
    let fun tyvar id = 
            case explode id of
              #"'" :: _ => true
            | _ => false
        fun eat_tyvars ts =
            case ts of
              ID id :: SPACE _ :: ts' => 
              if tyvar id then ts' else ts
            | _ => ts
        fun typespec "type" = true
          | typespec "eqtype" = true
          | typespec "datatype" = true
          | typespec _ = false
            
        fun loop (acc as (ids,tycons,excons,strs)) ts =
            case ts of
              ID "val" :: SPACE _ :: ID id :: ts =>
              loop (id::ids,tycons,excons,strs) ts
            | ID "exception" :: SPACE _ :: ID excon :: ts =>
              loop (ids,tycons,excon::excons,strs) ts
            | ID "structure" :: SPACE _ :: ID strid :: 
              SPACE _ :: ID ":" :: SPACE _ :: ID sigid :: ts =>
              loop (ids,tycons,excons,(strid,sigid)::strs) ts
            | ID kw :: SPACE _ :: ts =>
              if typespec kw then
                (case eat_tyvars ts of
                   ID tycon :: ts =>
                   loop (ids,tycon::tycons,excons,strs) ts
                 | _ => loop acc ts)
              else loop acc ts
            | _ :: ts => loop acc ts
            | nil => acc
    in
      loop ([],[],[],[]) ts
    end
end

structure R = RegExp

type sigid = string
type strid = string
type id = string

type sigmap = {comment:string, src:string, comments:string} Map.t   (* dom=sigid *)

fun readFile f =
    let val () = print ("Reading file: " ^ f ^ "\n")
        val is = TextIO.openIn f
    in let val s = TextIO.inputAll is
       in TextIO.closeIn is; s
       end handle ? => (TextIO.closeIn is; raise ?)
    end

fun read_sig (f:string) : sigmap =
    let val s = readFile f
    in case R.extract (R.fromString ".*\\(\\*\\*(.*)\\*\\).*(signature ([0-9a-zA-Z_]+).*end).*\\(\\*\\*(.*)\\*\\).*") s of
         SOME [c,sigid,src,cs] => 
         Map.single(sigid, {comment=c,src=src,comments=cs})
       | SOME ss => (List.app (fn s => print(s ^ "\n")) ss;
                     raise Fail "read_sig wrong format 0")
       | NONE => raise Fail "read_sig wrong format"
    end

type strmap = sigid Map.t   (* StrId -> SigId *)

fun read_impl (f:string) : strmap =
    let fun loop acc s =
            case R.extract (R.fromString "(.*)structure ([0-9a-zA-Z_]+) :>? ([0-9a-zA-Z_]+).*") s of
              SOME [rest,strid,sigid] => 
              loop (Map.insert(acc,strid,sigid)) rest
            | SOME [strid,sigid] => 
              Map.insert(acc,strid,sigid)
            | SOME ss => (List.app (fn s => print(s ^ "\n")) ss;
                          raise Fail "read_impl wrong format 1")
            | NONE => acc
    in loop (Map.emp()) (readFile f)
    end

fun match_id nil h = NONE
  | match_id (id::ids) h =
    if h = id orelse String.isPrefix (id ^ " ") h orelse
       String.isSubstring (" " ^ id ^ " ") h then
      SOME id
    else match_id ids h

fun pp_comments (ids,tycons,excons,strs) s =
    let (*val () = print ("ids = " ^ Int.toString (length ids) ^"\n")*)
(*(*(*
        fun loop cs s =
            case R.extract (R.fromString "(.*)\\n\\[([\\-0-9a-zA-Z_:<>=&%#@|'!*/$(), ]+)\\](.*)") s of
              SOME [rest,h,b] => 
              loop ((h,b)::cs) rest
            | SOME [h,b] => ((h,b)::cs)
            | SOME ss => (List.app (fn s => print(s ^ "\n")) ss;
                          raise Fail "pp_comments wrong format 1")
            | NONE => cs
*)
        val lines = String.fields (fn #"\n" => true | _ => false) s
        val lines = map (fn s => s ^ "\n") lines
        fun read_head s =
            (case CharVector.findi (fn (_,#"]") => true | _ => false) s of
               SOME (i,_) =>
               (String.extract(s,1,SOME (i-1)),
                String.extract(s,i+1,NONE) handle _ => "")
             | NONE => raise Fail "read_head")
            handle _ => raise Fail ("read_head2:" ^ s) 
        fun only_ws s = CharVector.all Char.isSpace s
        fun eat_wss (s::ss) = if only_ws s then eat_wss ss
                              else s::ss
          | eat_wss nil = nil
        fun finalize a cs =
            case eat_wss(rev a) of
              l0::ls =>
              let val (h,r) = read_head l0
              in (h, concat (r::ls))::cs
              end
            | nil => cs
        fun loop cs a (l::ls) =
            if String.isPrefix "[" l then
              loop (finalize a cs) [l] ls
            else loop cs (l::a) ls
          | loop cs a nil = rev(finalize a cs)
      val cs = loop [] [] lines
      fun layout_head h = if stringOfCString h = "Discussion" then tag "i" h else ($"[") & tag "tt" h & ($"]")
      fun layout_body b =
          let
            val b = remove_init_ws b
            val lines = String.fields (fn #"\n" => true | _ => false)  b
            fun next_init_space ls =
                (init_space(hd ls))
                handle _ => false
            fun loop nil true acc = "</pre>\n"::acc
              | loop nil false acc = acc
              | loop (l0::ls) ispre acc =
                let val l = l0 ^ "\n"
                in
                  if ispre then
                    if init_space l0 then
                      if next_init_space ls then
                        loop ls true (l::acc)
                      else loop ls true acc
                    else loop ls false (l::"</pre>\n"::acc)
                  else if only_ws l then
                    loop ls false acc
                  else if init_space l then
                    loop ls true (l::"<pre>\n"::acc)
                  else loop ls false (l::acc)
                end
          in $(concat(rev(loop lines false nil)))
          end
    in tag "dl" 
           (cconcat 
                (map (fn (h,b) => 
                         let
                           val h2 = htmlencode h
                           val h3 =
                               case match_id ids h of
                                 SOME id => 
                                 let val name = encode id
                                 in taga "a" (" name='" ^ name ^ "'") ($h2)
                                 end
                               | NONE => $ h2
                           val b = htmlencode b
                         in if only_ws b then tag "dt" (tag "b" (layout_head h3))
                            else tag "dt" (tag "b" (layout_head h3)) & ($" ") & tag "dd" (layout_body b) & tag0 "br"
                         end) cs))
    end

fun page h idx b =
    let val str_idx_link = taga "a" " href='str_idx.html'" ($"Structure Idx")
        val sig_idx_link = taga "a" " href='sig_idx.html'" ($"Signature Idx")
        val id_idx_link = taga "a" " href='id_idx.html'" ($"Id Idx")
    in
      tag "html" 
          (tag "body" 
               (taga "table" " width=100%" (tag "tr" (tag "td" (tag "b" h) & (tag "td" idx) & 
                                                        taga "td" " align='right'" 
                                                          (str_idx_link & ($" | ") & 
                                                           sig_idx_link & ($" | ") & 
                                                           id_idx_link))) &
                     (tag0 "hr" &
                           b & 
                           tag0 "hr" &
                           tag "i" ($"Generated by SigDoc"))))
    end

fun strs_for_sigid sigid strmap =
    Map.argsForWhich strmap (fn x => sigid=x)

fun pp strmap (sigid, {comment,src,comments}) =
    let val ts = Lex.lex src 
      val ids = Lex.defs ts 
      val idmap = Map.single(sigid,ids)
      val ts = Lex.conv (fn x => x) ts
      val src2 = Lex.pp ts
      open Lex
      fun layout_struct x = tag "b" ($("structure" ^ " " ^ x ^ " : " ^ sigid ^ "\n"))                            
      val output =
          page 
              ($"Signature " & tag "tt" ($sigid))
              ($"")
              ($(htmlencode comment) &
                tag0 "hr" &
                tag "pre" (cconcat (map layout_struct (strs_for_sigid sigid strmap))) &
                tag0 "hr" &
                tag "pre" src2 & 
                tag0 "hr" &
                pp_comments ids comments)
    in (output, idmap)
    end

fun writeFile f a =
    let val () = print ("Writing file: " ^ f ^ "\n")
        val os = TextIO.openOut f
    in (TextIO.output(os,a);
        TextIO.closeOut os)
       handle ? => (TextIO.closeOut os; raise ?)
    end

fun gen_idx {head: string,
             lines: 'a list,
             line_id: 'a -> string,
             line_entry: 'a -> cstring,
             line_body: 'a -> cstring,
             sep: string} =
    let
      fun section ch part =
          if part = $"" then $""
          else 
          let val h =
                  if Char.isAlpha ch then String.str (Char.toUpper ch)
                  else "Symbols" 
              val h = taga "a" (" name='"^h^"'") ($h)
          in tag "h3" h & tag "table" part
          end
      val (ch,ch_acc,acc,chs) = 
          foldl (fn (line,(ch,ch_acc,acc,chs)) =>
                    let
                      val body = line_body line
                      val id = line_id line
                      val ch2 = String.sub(id,0)
                      val e = line_entry line
                      val entry = tag "tr" (tag "td" (tag "tt" e) & tag "td" ($sep) & tag "td" body)
                    in if not(Char.isAlpha ch2) orelse ch=ch2 then
                         (ch,ch_acc & entry,acc,chs)
                       else (ch2,entry,acc & section ch ch_acc,ch2::chs)
                    end)
                (#"*",$"",$"",nil) lines
      val cs = acc & section ch ch_acc
      val idx = 
          cconcatWith " | "
          (map (fn c => 
                   let val h = String.str (Char.toUpper c)
                   in taga "a" (" href='#"^h^"'") ($h)
                   end) (rev chs))
    in page ($head) idx cs
    end

fun gen_sig_idx (sigmap, strmap) = 
    let val sigs = Map.dom sigmap
        val sigs = Listsort.sort String.compare sigs
        val im = List.map (fn s => 
                              let val t = s ^ ".sml.html" 
                                  val strs = strs_for_sigid s strmap
                                  val strs = List.foldl (fn (s,a) => tag "tt" ($s) & ($" ") & a) ($"") strs
                              in (s, taga "a" (" href='" ^ t ^ "'") (tag "tt" ($s)), strs)
                              end) sigs
        val cs = gen_idx {head="Signature Index",
                          lines=im,
                          line_id= #1,
                          line_entry= #2,
                          line_body= #3,
                          sep="&nbsp;"}
    in writeFile "sig_idx.html" (stringOfCString cs)
    end

fun gen_str_idx (sigmap, strmap) =
    let val strs = Map.list strmap
        val strs = Listsort.sort (fn ((x,y),(x1,y1)) => String.compare(x,x1)) strs
        val im = List.map (fn (strid,sigid) => 
                              let val t = sigid ^ ".sml.html" 
                              in (strid, taga "a" (" href='" ^ t ^ "'") (tag "tt" ($sigid)))
                              end) strs
        val cs = gen_idx {head="Structure Index",
                          lines=im,
                          line_id= #1,
                          line_entry = $ o #1,
                          line_body= #2,
                          sep=":"}
    in writeFile "str_idx.html" (stringOfCString cs)
    end

fun gen_id_idx (idmap, sigmap, strmap) =
    let val im = foldl (fn ((sigid, (ids,_,_,_)),a) =>
                           let val strs = strs_for_sigid sigid strmap
                           in (map (fn id => (id,sigid,strs)) ids) @ a
                           end) nil (Map.list idmap)
      val im = Listsort.sort (fn((id,_,_),(id2,_,_)) => String.compare (id,id2)) im               
      fun compact nil a = rev a
        | compact ((id,sigid,strs)::rest) nil =
             compact rest [(id,[(sigid,strs)])]
        | compact ((id,sigid,strs)::rest) (acc as ((id2,args)::acc2)) =
          if id = id2 then
            compact rest ((id,(sigid,strs)::args)::acc2)
          else 
            compact rest ((id,[(sigid,strs)])::acc)
      val im = compact im nil
      fun layout_impls (id, nil) = $""
        | layout_impls (id, (sigid,strs)::rest) = 
          List.foldl (fn (s,a) => a & taga "a" (" href='" ^ sigid ^ ".sml.html'") (tag "tt" ($s)) & (tag "tt" ($("." ^ id))) & ($" "))
                     ($"") strs & layout_impls (id, rest)
      val cs =
          gen_idx {head="Id Index",
                   lines=im,
                   line_id= #1,
                   line_entry = $ o #1,
                   line_body=layout_impls,
                   sep="&nbsp;"}
    in writeFile "id_idx.html" (stringOfCString cs)
    end

fun gen (sigfiles:string list, implfiles) =
    (* Assumption: One signature for each sigfile (named according to the file).
     * Look in all implfiles for strings of the form
          structure A : B
          structure A :> B
     *)
    let 
      val sigmap : sigmap = 
          foldl (fn (x,a) => Map.plus(a,read_sig x)) (Map.emp()) sigfiles
      val strmap : strmap =
          foldl (fn (x,a) => Map.plus(a,read_impl x)) (Map.emp()) implfiles
      fun out (arg as (s,a), idmap) =
          let val (cstr, idmap2) = pp strmap arg 
              val str = stringOfCString cstr
              val file = s ^ ".sml.html"
          in writeFile file str;
             Map.plus(idmap,idmap2)
          end
      val idmap = List.foldl out (Map.emp()) (Map.list sigmap);
    in
       gen_sig_idx (sigmap, strmap);
       gen_str_idx (sigmap, strmap);
       gen_id_idx (idmap, sigmap, strmap)
    end

signature PARSE_ARG = sig
  datatype t = Nullary of string * (unit -> unit)
             | Unary of string * (string -> unit)
             | Multi of string * (string list -> unit)
  val run : t list -> string list
end
structure ParseArg : PARSE_ARG =
struct
  exception Exit of string list
  datatype t = Nullary of string * (unit -> unit)
             | Unary of string * (string -> unit)
             | Multi of string * (string list -> unit)
  fun isFlag x = (String.sub(x,0) = #"-") handle _ => false
  fun read_args xs =
      let fun loop nil a = (rev a, nil) 
            | loop (x::xs) a =
              if isFlag x then (rev a, x::xs)
              else loop xs (x::a)
      in loop xs nil
      end
  fun getNullary x (Nullary(y,f)::ts) = 
      if x = y then SOME f
      else getNullary x ts
    | getNullary x (t::ts) = getNullary x ts
    | getNullary x nil = NONE
  fun getUnary x (Unary(y,f)::ts) = 
      if x = y then SOME f
      else getUnary x ts
    | getUnary x (t::ts) = getUnary x ts
    | getUnary x nil = NONE
  fun getMulti x (Multi(y,f)::ts) = 
      if x = y then SOME f
      else getMulti x ts
    | getMulti x (t::ts) = getMulti x ts
    | getMulti x nil = NONE
                      
  fun err s = (print (s ^ "\n"); raise Fail "ParseArg error")
  fun unknown x = err ("Unknown argument '" ^ x ^ "'")
  fun run ts =
      let 
        fun loop nil = []
          | loop (x::xs) =
            if isFlag x then
              let val (args, xs) = read_args xs
                  val () = 
                      case args of
                        nil => 
                        (case getNullary x ts of
                           SOME f => f()
                         | NONE => unknown x)
                      | [a] => 
                        (case getUnary x ts of
                           SOME f => f a
                         | NONE => 
                           (case getMulti x ts of
                              SOME f => f [a]
                            | NONE => 
                              (case getNullary x ts of
                                 SOME f => (f(); raise Exit(a::xs))
                               | NONE => unknown x)))
                      | a::aa =>
                        (case getMulti x ts of
                           SOME f => f args
                         | NONE => 
                           (case getUnary x ts of
                              SOME f => (f a; raise Exit(aa@xs))
                            | NONE =>
                              (case getNullary x ts of
                                 SOME f => (f(); raise Exit(args@xs))
                               | NONE => unknown x)))
              in loop xs
              end
            else raise Exit(x::xs)
      in loop (CommandLine.arguments()) handle Exit ys => ys
      end
end

fun help() = print "usage: sigdoc [-sigs FILES] [-impl FILES]\n"
val sigs : string list ref = ref nil
val impl : string list ref = ref nil
fun reg r xs = r := xs
val () = case ParseArg.run [ParseArg.Multi("-sigs",reg sigs),
                            ParseArg.Multi("-impl",reg impl)] of
           [] => if !sigs = nil then help() 
                 else gen(!sigs,!impl)
         | _ => help()
