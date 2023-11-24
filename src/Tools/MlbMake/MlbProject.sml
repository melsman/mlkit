functor MlbProject (Env : ENVIRONMENT) :> MLB_PROJECT =
struct

  fun die s = (print ("Die: MlbProject: " ^ s ^ "\n"); raise Fail s)

  fun unique_true f = MlbFileSys.unique true f

  structure MS =
  struct

    structure Bid :>
      sig
        eqtype bid and longbid
        val bid : string -> bid
        val bidCompare : bid * bid -> order
        val longbid : bid list -> longbid
        val longopen : longbid -> bid * longbid option
        val pp_bid : bid -> string
        val pp_longbid : longbid -> string
        val explode : longbid -> bid list
      end =
    struct
      type bid = string
      type longbid = bid list
      fun bid s = s
      val bidCompare = String.compare
      fun pp_bid s = s
      fun longbid (bids:bid list) : longbid = bids
      fun longopen nil = die "empty longbid"
        | longopen [bid] = (bid,NONE)
        | longopen (bid::longbid) = (bid, SOME longbid)
      fun pp_longbid nil = die "empty longbid"
        | pp_longbid [b] = pp_bid b
        | pp_longbid (b::bs) = pp_bid b ^ "." ^ pp_longbid bs
      fun explode ss = ss
    end

    type atbdec = string (* path.{sml,sig} *)
    datatype bexp = BASbexp of bdec
                  | LETbexp of bdec * bexp
                  | LONGBIDbexp of Bid.longbid

    and bdec = SEQbdec of bdec * bdec
             | EMPTYbdec
             | LOCALbdec of bdec * bdec
             | BASISbdec of Bid.bid * bexp
             | OPENbdec of Bid.longbid list
             | ATBDECbdec of atbdec
             | MLBFILEbdec of string  (* path.mlb *)
             | ANNbdec of string * bdec

    fun supported_annotation s =
        case s of
            "safeLinkTimeElimination" => true
          | "inline_functors" => true
          | _ => false

  end

  val depDir : string ref = ref "PM"

  val error = MlbUtil.error
  val warn = MlbUtil.warn
  val quot = MlbUtil.quot

  fun has_ext (s: string,ext) =
      case OS.Path.ext s of
        SOME ext' => ext = ext'
      | NONE => false

  fun member s l =
      let fun m [] = false
            | m (x::xs) = x = s orelse m xs
      in m l
      end

  fun drop_comments (file:string) (l: char list) : char list =
      let fun loop(n, #"(" :: #"*" :: rest ) = loop(n+1, rest)
            | loop(n, #"*" :: #")" :: rest ) = loop(n-1, if n=1 then #" "::rest else rest)
            | loop(0, ch ::rest) = ch :: loop (0,rest)
            | loop(0, []) = []
            | loop(n, ch ::rest) = loop(n,rest)
            | loop(n, []) = error ("Unclosed comment in project " ^ quot(file))
      in loop(0,l)
      end

  fun is_symbol #"=" = true
    | is_symbol _ = false

  local
    fun lex_symbol (c::chs) =
        if is_symbol c then SOME (c,chs) else NONE
      | lex_symbol nil = NONE

    fun lex_whitesp (all as c::rest) =
        if Char.isSpace c then lex_whitesp rest else all
      | lex_whitesp [] = []

    fun lex_string (c::rest, acc) =
        if is_symbol c then (implode(rev acc), c::rest)
        else if Char.isSpace c then (implode(rev acc), rest)
       else lex_string (rest, c::acc)
      | lex_string ([], acc) = (implode(rev acc), [])

    val lex_string_lit =
     fn file => fn #"\"" :: rest =>
       let
         fun lp acc =
          fn #"\"" :: rest => SOME (implode (rev acc), rest)
           | c :: rest => lp (c::acc) rest
           | [] => error ("Unclosed string literal in project " ^
                          quot(file))
       in lp [] rest
       end
      | _ => NONE

    fun lex0 file (chs : char list, acc) : string list =
        case lex_whitesp chs
         of [] => rev acc
          | chs => lex0 file (case lex_symbol chs
                               of  SOME (c,chs) => (chs, Char.toString c :: acc)
                                 | NONE =>
                                   case lex_string_lit file chs of
                                     SOME (s, chs) => (chs, s :: acc)
                                   | NONE =>
                                     let val (s, chs) = lex_string(chs,[])
                                     in (chs, s::acc)
                                     end)
  in
    fun lex file chs : string list = lex0 file (chs,[])
  end

  fun is_symb s =
      case explode s of
        [c] => is_symbol c
      | _ => false

  fun is_mlbfile s =
      has_ext (s,"mlb")

  fun is_smlfile s =
      has_ext(s,"sml") orelse has_ext(s,"sig") orelse has_ext(s,"fun")

  fun is_id s =
      case explode s of
        c::cs => Char.isAlpha c
                 andalso List.all (fn c => Char.isAlphaNum c
                                           orelse c = #"_"
                                           orelse c = #"'") cs
      | _ => false

  fun is_valid_path p =
      case String.fields (fn x => x = #"/") p of
        f :: rest => (List.all (fn x => is_id x andalso not (is_mlbfile x orelse is_smlfile x)) rest
                      andalso (is_id f andalso not (is_mlbfile f orelse is_smlfile f)
                               orelse f = ""))
      | _ => false

  fun is_keyword s =
      case s of
        "open" => true
      | "let" => true
      | "local" => true
      | "in" => true
      | "end" => true
      | "bas" => true
      | "basis" => true
      | "scriptpath" => true
      | "ann" => true
      | _ => false

  local
    fun is_fileext s =
        case s of
          "mlb" => true
        | "sml" => true
        | "sig" => true
        | _ => false

  in
    fun is_bid s =
        is_id s andalso not (is_keyword s) andalso not (is_fileext s)

    fun is_longbid s =
        let val fs = String.fields (fn c => c = #".") s
        in if List.all is_bid fs then SOME (MS.Bid.longbid (map MS.Bid.bid fs))
           else NONE
        end
  end

  fun print_ss ss = (print "Tokens: "; app (fn s => print (s ^ " ")) ss; print "\n")

  fun parse_error mlbfile msg = error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg)
  fun parse_error1 mlbfile (msg, rest: string list) =
      case rest of
        [] => error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg ^ "(reached end of file)")
      | s::_ => error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg ^ "(reached " ^ quot s ^ ")")

  fun parse_warn1 mlbfile (msg, rest) =
      warn (concat ["while parsing basis file ", quot mlbfile, " : ", msg,
                    case rest of [] => "(reached end of file)"
                               | s::_ => "(reached " ^ quot s ^ ")"])

  fun expand mlbfile s =
      let
        val implodeRev = implode o rev
        fun resolveVar pathVar =
            case Env.getEnvVal pathVar
             of SOME path => path
              | NONE => parse_error mlbfile ("path variable $(" ^ pathVar ^") not in env")

        fun inVar strs cs =
         fn [] => parse_error mlbfile "malformed path-var"
          | #")" :: cc =>
            inLit (resolveVar (implodeRev cs)::strs) [] cc
          | cc as (#"/" :: _) =>
            inLit (resolveVar (implodeRev cs)::strs) [] cc
          | c::cc => inVar strs (c::cs) cc

        and inLit strs cs =
         fn [] => concat (rev (implodeRev cs::strs))
          | #"$" :: #"(" :: cc => inVar (implodeRev cs::strs) [] cc
          | #"$" :: cc => inVar (implodeRev cs::strs) [] cc
          | c :: cc => inLit strs (c::cs) cc
      in
        inLit [] [] (explode s)
      end

  fun parse_bdec_more mlbfile (bdec,ss) =
      case parse_bdec_opt mlbfile ss of
        SOME(bdec',ss) => SOME(MS.SEQbdec(bdec,bdec'),ss)
      | NONE => SOME(bdec,ss)

  and parse_bexp mlbfile (ss:string list) : MS.bexp * string list =
      case ss of
        nil => parse_error1 mlbfile ("missing basis expression", ss)
      | "let" :: ss =>
        let
          fun parse_rest(bdec,ss) =
              case ss of
                "in" :: ss =>
                let val (bexp,ss) = parse_bexp mlbfile ss
                in
                  case ss of
                    "end" :: ss => (MS.LETbexp(bdec,bexp),ss)
                  | _ => parse_error1 mlbfile ("missing 'end'", ss)
                end
              | _ => parse_error1 mlbfile ("missing 'in'", ss)
        in case parse_bdec_opt mlbfile ss of
             NONE => parse_rest(MS.EMPTYbdec,ss)
           | SOME(bdec,ss) => parse_rest(bdec,ss)
        end
      | "bas" :: ss =>
        let
          fun parse_rest(bdec,ss) =
              case ss of
                "end" :: ss => (MS.BASbexp bdec, ss)
              | _ => parse_error1 mlbfile ("missing 'end'", ss)
        in
          case parse_bdec_opt mlbfile ss of
            NONE => parse_rest(MS.EMPTYbdec,ss)
          | SOME(bdec,ss) => parse_rest(bdec,ss)
        end
      | s :: ss' =>
        (case is_longbid s of
           SOME longbid => (MS.LONGBIDbexp longbid,ss')
         | NONE => parse_error1 mlbfile ("invalid basis expression", ss))

  and parse_ann mlbfile ss =
      let
        fun lp (anns, s::ss) =
            if is_keyword s then (anns, s::ss)
            else if MS.supported_annotation s then lp (s::anns,ss)
            else (parse_warn1 mlbfile ("non-supported annotation '"^s^
                                       "' after", ss)
                ; lp (anns,ss))
          | lp (_, _) =
            parse_error1 mlbfile ("missing annotation after 'ann'", ss)
      in
        lp ([], ss)
      end

  and parse_bdec_opt mlbfile ss =
      case ss of
        "local" :: ss =>
        let
          fun parse_rest'(bdec,bdec',ss) =
              case ss of
                "end" :: ss => parse_bdec_more mlbfile (MS.LOCALbdec(bdec,bdec'),ss)
              | _ => parse_error1 mlbfile ("I expect an 'end'", ss)
          fun parse_rest(bdec,ss) =
              case ss of
                "in" :: ss =>
                (case parse_bdec_opt mlbfile ss of
                   NONE => parse_rest'(bdec,MS.EMPTYbdec,ss)
                 | SOME(bdec',ss) => parse_rest'(bdec,bdec',ss))
              | _ => parse_error1 mlbfile ("I expect an 'in'", ss)
        in case parse_bdec_opt mlbfile ss of
             NONE => parse_rest(MS.EMPTYbdec,ss)
           | SOME(bdec,ss) => parse_rest(bdec,ss)
        end
      | "basis" :: ss =>
        (case ss of
           nil => parse_error1 mlbfile ("I expect a basis identifier", ss)
         | bid :: ss =>
           if not (is_bid bid) then
             parse_error1 mlbfile ("I expect a basis identifier", ss)
           else
             (case ss of
                "=" :: ss =>
                let val (bexp,ss) = parse_bexp mlbfile ss
                in parse_bdec_more mlbfile (MS.BASISbdec(MS.Bid.bid bid,bexp),ss)
                end
              | _ => parse_error1 mlbfile ("missing '='", ss)))
      | "open" :: ss =>
        let
          fun readBids (nil,acc) = (rev acc, nil)
            | readBids (ss_all as (s::ss),acc) =
              (case is_longbid s of
                 SOME longbid => readBids(ss,longbid::acc)
               | NONE => (rev acc, ss_all))
          val (longbids,ss) = readBids (ss,nil)
        in parse_bdec_more mlbfile (MS.OPENbdec longbids,ss)
        end
      | "ann" :: ss =>
        let
          fun parse_rest' (anns,bdec,ss) =
              case ss of
                "end" :: ss =>
                parse_bdec_more
                    mlbfile
                    (foldl (fn (ann, bdec) =>
                               MS.ANNbdec(ann,bdec))
                           bdec
                           anns,
                     ss)
              | _ => parse_error1 mlbfile ("I expect an 'end'", ss)
          fun parse_rest (anns,ss) =
              case ss of
                "in" :: ss =>
                (case parse_bdec_opt mlbfile ss of
                   NONE => parse_rest'(anns,MS.EMPTYbdec,ss)
                 | SOME(bdec,ss) => parse_rest'(anns,bdec,ss))
              | _ => parse_error1 mlbfile ("I expect an 'in'", ss)
        in case parse_ann mlbfile ss of
             (anns,ss) => parse_rest(anns,ss)
        end
      | s :: ss =>
        if is_smlfile s then parse_bdec_more mlbfile (MS.ATBDECbdec (expand mlbfile s),ss)
        else
          if is_mlbfile s then
            let val mlbfile2 = expand mlbfile s
              (*val () = print ("File " ^ s ^ " expands to " ^ mlbfile2 ^ " in " ^ mlbfile ^"\n") *)
            in parse_bdec_more mlbfile (MS.MLBFILEbdec mlbfile2,ss)
            end
          else NONE
      | nil => NONE

  fun fromFile' (filename:string) : string option =
      SOME(MlbFileSys.fromFile filename) handle _ => NONE

  fun parse (mlbfile: string) : MS.bdec * string =
      if not (has_ext(mlbfile, "mlb")) then
        error ("The basis file " ^ quot mlbfile ^ " does not have extension 'mlb'")
      else
        let (* val _ = print ("currently at " ^ OS.FileSys.getDir() ^ "\n") *)
          val mlbcontent = MlbFileSys.fromFile mlbfile
          val mlbhash = MD5.fromString mlbcontent
          val ss = (lex mlbfile o drop_comments mlbfile o
                    explode) mlbcontent
        (* val _ = print_ss ss *)
          val bdec =
              case parse_bdec_opt mlbfile ss of
                SOME (bdec,nil) => bdec
              | SOME (bdec,ss) => parse_error1 mlbfile ("misformed basis declaration", ss)
              | NONE => MS.EMPTYbdec
        in (* print ("Successfully parsed " ^ mlbfile ^ "\n"); *)
           (bdec, mlbhash)
        end
        handle IO.Io {name=io_s,cause,...} => error ("The basis file " ^ quot mlbfile ^ " cannot be opened")


  (* Support for writing dependency information to disk in .d-files. *)

  fun path_concat dir file =
      (if OS.Path.isAbsolute file then file
       else OS.Path.concat(dir,file))
      handle OS.Path.Path => die "Path: path_concat"

  structure DepEnv =
  struct
    type L = string list
    datatype D = D of L * (MS.Bid.bid * D) list
    fun lookup d longbid : D option =
        let
          fun look nil _ = NONE
            | look ((x,d)::xs) y = if x = y then SOME d else look xs y
          fun lookD _ nil = die "empty longbid"
            | lookD (D(_,xs)) [y] = look xs y
            | lookD (D(_,xs)) (y::ys) =
              case look xs y of
                SOME d => lookD d ys
              | NONE => NONE
        in lookD d (MS.Bid.explode longbid)
        end
    fun plus (D(L1,M1),D(L2,M2)) = D(L1 @ L2, M1 @ M2)

    fun getL (D(L,_)) : L = L
    fun singleBidEntry e = D(nil,[e])
    fun singleFile f = D([f],nil)
    val empty = D(nil,nil)
    fun dirModify ("", dep) = dep
      | dirModify (dir, dep : D) : D =
        let fun depMap f (D(L,bds)) = D(map f L,map (fn (b,d) => (b,depMap f d)) bds)
        in depMap (path_concat dir) dep
        end
  end

  fun timeMax(t1,t2) = if Time.<(t1,t2) then t2 else t1

  fun maybe_create_dir d : unit =
      if OS.FileSys.access (d, []) handle _ => error ("I cannot access directory " ^ quot d) then
        if OS.FileSys.isDir d then ()
        else error ("The file " ^ quot d ^ " is not a directory")
      else ((OS.FileSys.mkDir d;()) handle _ =>
         error ("I cannot create directory " ^ quot d ^ " --- the current directory is " ^
                OS.FileSys.getDir()))

  fun maybe_create_dirs path dirs =
      let val dirs = String.tokens (fn c => c = #"/") dirs
        fun append_path "" d = d
          | append_path p d = p ^ "/" ^ d
        fun loop (p, nil) = ()
          | loop (p, d::ds) = let val p = append_path p d
                              in maybe_create_dir p; loop(p, ds)
                              end
      in loop(path, dirs)
      end

  fun maybe_create_depdir path =
      maybe_create_dirs path (!depDir)

  fun writeDep pathfile s =
      (let val os = TextIO.openOut pathfile
       in (  TextIO.output(os,s)
           ; TextIO.closeOut os
          (* ; print ("File " ^ pathfile ^ " updated\n") *)
          ) handle ? => (TextIO.closeOut os; raise ?)
       end handle _ => error("Failed to write dependencies to file " ^ quot pathfile))

  fun fileNewer t file : bool =
      Time.>(OS.FileSys.modTime file,t) handle _ => false

  local
    fun mkAbs file = OS.Path.mkAbsolute{path=file,relativeTo=OS.FileSys.getDir()}
                     handle OS.Path.Path => die "mkAbs"

  in
    fun subtractDir _ "" = (fn p => p)
      | subtractDir hasLink dir =
        if hasLink
        then fn p => path_concat (MlbFileSys.getCurrentDir()) p
        else
          let val dir_abs = mkAbs dir
          in fn p =>
                let val p_abs = mkAbs p
                    (* val _ = if OS.Path.isAbsolute dir then print ("mkAbs: " ^ p ^ " " ^ p_abs ^ " " ^ dir ^ "\n") else () *)
                in OS.Path.mkRelative{path=p_abs,relativeTo=dir_abs} handle OS.Path.Path => die "subtractDir"
                end
          end
  end

  local
    fun hasLinks d =
        let
          val islink = fn x => (OS.FileSys.isLink x handle OS.SysErr (s,e) =>
                                                           error ("Couldn't get status on " ^ x ^" : " ^ (Option.getOpt (Option.map OS.errorMsg e, ""))))
          val {arcs,vol,isAbs} = OS.Path.fromString d
        in
          case arcs
           of [] => false
            | (x::xr) => #2(List.foldl (fn (x,(y,b)) =>
                                           let val y' = OS.Path.concat (y,x) handle OS.Path.Path => die "Path: hasLinks"
                                           in (y',b orelse islink y')
                                           end)
                                       (let val a = OS.Path.toString {vol = vol, isAbs = isAbs, arcs = [x]} handle OS.Path.Path => die "Path: hasLinks2"
                                        in (a, islink a)
                                        end)
                                       xr)
        end
  in
    fun maybeWriteDep smlfile (modTimeMlbFileMax:Time.time) D : unit =
        let
          val {dir,file} = OS.Path.splitDirFile smlfile
          infix ##
          val op ## = OS.Path.concat
          val file = (dir ## (!depDir) ## (file ^ ".d")) handle OS.Path.Path => die "Path: maybeWriteDep"
        in if fileNewer modTimeMlbFileMax file then ()         (* Don't write .d-file if the current .d-file *)
           else                                                (*  is newer than the mlb-file or any imported mlb-file. *)
             let
               val L = DepEnv.getL D
               val L = map (subtractDir (hasLinks dir) dir) L
               val s = concat (map (fn s => s ^ " ") L)
               fun write () =
                   ((if dir = "" then ()
                     else maybe_create_depdir dir);
                    writeDep file s)
             in case fromFile' file of
                  SOME s' => if s = s' then ()                  (* Don't write .d-file if its content already *)
                             else write()                       (*  specifies the correct dependencies. *)
                | NONE => write()
             end
        end

    fun maybeWriteDef mlbfile (modTimeMlbFileMax:Time.time) D : unit =  (* much similar to maybeWriteDep! *)
        let
          val {dir,file} = OS.Path.splitDirFile mlbfile
          infix ##
          val op ## = OS.Path.concat
          val file = (dir ## (!depDir) ## (file ^ ".df")) handle OS.Path.Path => die "Path: maybeWriteDef"
        in if fileNewer modTimeMlbFileMax file then ()         (* Don't write .df-file if the current .df-file *)
           else                                                (*  is newer than the mlb-file or any imported mlb-file. *)
             let
               val L = DepEnv.getL D
               val L = map (subtractDir (hasLinks dir) dir) L
               val s = concat (map (fn s => s ^ " ") L)
               fun write () =
                   ((if dir = "" then ()
                     else maybe_create_depdir dir);
                    writeDep file s)
             in case fromFile' file of
                  SOME s' => if s = s' then ()                  (* Don't write .df-file if its content already *)
                             else write()                       (*  specifies the correct dependencies. *)
                | NONE => write()
             end
        end

  end

  local
        val op + = DepEnv.plus
        type D = DepEnv.D
        type A = {modTimeMlbFileMax: Time.time, mlbfiles: (MlbFileSys.unique * D) list}   (* max-modtime of mlb-files met so far *)
        val emptyA = {modTimeMlbFileMax=Time.zeroTime,mlbfiles=nil}
        fun lookupA {modTimeMlbFileMax, mlbfiles} mlbfile : D option =
            let fun look nil = NONE
                  | look ((x,D)::xs) = if mlbfile = x then SOME D
                                       else look xs
            in look mlbfiles
            end

        fun dep_bexp (D:D) (A:A) bexp : D * A =
            case bexp of
                MS.BASbexp bdec => dep_bdec D A bdec
              | MS.LETbexp (bdec,bexp) =>
                    let val (D1,A) = dep_bdec D A bdec
                        val (D2,A) = dep_bexp (D+D1) A bexp
                    in (D2,A)
                    end
              | MS.LONGBIDbexp longbid =>
                    (case DepEnv.lookup D longbid of
                         SOME D => (D,A)
                       | NONE => error ("The long basis identifier "
                                        ^ MS.Bid.pp_longbid longbid
                                        ^ " is undefined"))

        and dep_bdec (D:D) (A:A) bdec : D * A =
            case bdec of
                MS.SEQbdec (bdec1,bdec2) =>
                    let val (D1,A) = dep_bdec D A bdec1
                        val (D2,A) = dep_bdec (D + D1) A bdec2
                    in (D1 + D2, A)
                    end
              | MS.EMPTYbdec => (DepEnv.empty,A)
              | MS.LOCALbdec (bdec1,bdec2) =>
                    let val (D1,A) = dep_bdec D A bdec1
                        val (D2,A) = dep_bdec (D + D1) A bdec2
                    in (D2,A)
                    end
              | MS.BASISbdec (bid, bexp) =>
                    let val (D',A) = dep_bexp D A bexp
                    in (DepEnv.singleBidEntry (bid,D'), A)
                    end
              | MS.OPENbdec longbids =>
                    let val D' =
                        foldl (fn (longbid,Dacc) =>
                               case DepEnv.lookup D longbid of
                                   SOME D => Dacc + D
                                 | NONE => error ("The long basis identifier "
                                                  ^ MS.Bid.pp_longbid longbid
                                                  ^ " is undefined")) DepEnv.empty longbids
                    in (D',A)
                    end
              | MS.ATBDECbdec smlfile =>
                    let val _ = maybeWriteDep smlfile (#modTimeMlbFileMax A) D
                    in (DepEnv.singleFile smlfile, A)
                    end
              | MS.MLBFILEbdec mlbfile =>
                    let val (D,A) = dep_bdec_file A mlbfile
                    in (DepEnv.dirModify (OS.Path.dir mlbfile, D), A)
                    end
              | MS.ANNbdec (s,bdec) => dep_bdec D A bdec

        and dep_bdec_file (A:A) mlbfile_rel : D * A =
            let
                val unique = unique_true mlbfile_rel
(*          let val mlbfile_abs = mkAbs mlbfile_rel *)
            in
                case lookupA A unique of
                    SOME D => (D,A)
                  | NONE =>
                        let val {cd_old,file=mlbfile} = MlbFileSys.change_dir mlbfile_rel
                        in let val _ = maybe_create_depdir ""
                               val (bdec,_) = parse mlbfile
                               val modTimeMlbFileMaxOld = #modTimeMlbFileMax A
                               val modTimeMlbFileMaxNew = OS.FileSys.modTime mlbfile
                                   handle _ => error ("Failed to read the modification time of the basis file " ^
                                                      mlbfile)
                               val A = {modTimeMlbFileMax=modTimeMlbFileMaxNew,
                                        mlbfiles= #mlbfiles A}
                               val (D,A) = dep_bdec DepEnv.empty A bdec

                           in  maybeWriteDef mlbfile (#modTimeMlbFileMax A) D ;
                               cd_old() ;
                               (D, {modTimeMlbFileMax=timeMax(modTimeMlbFileMaxOld,modTimeMlbFileMaxNew),
                                    mlbfiles=(unique,D) :: #mlbfiles A})
                           end handle X => (cd_old(); raise X)
                        end
            end

  in
        fun dep (mlbfile : string) : unit =
            ((* print "[Computing dependencies ..."; *)
             dep_bdec_file emptyA mlbfile; ()
             (* ; print "]\n" *))
  end

  (* Support for finding the source files of a basis file *)

  structure Atom :>
      sig
        type atom
        val toString : atom -> string
        val fromString : string -> atom
        val compare : (atom * atom) -> order
      end =
      struct
        type atom = string
        val toString = fn x => x
        val fromString = fn x => x
        val compare = String.compare
      end

  type SmlFile = Atom.atom

  structure BuildGraph :
    sig
      type 'a A
      type 'a B
      type 'a Map
      type DepToken
      type I
      val firstB : I
      val emptyA : 'a A
      val projectB : 'a B -> ('a * I) option
      val foldB : (('a * 'b) -> 'b) -> 'b -> 'a B -> 'b
      val add_first : 'a A -> DepToken list -> 'a -> ('a A * DepToken)
      val turn : 'a A -> ('a B * 'a Map)
      val fromMap : ('a Map * I) -> 'a B list
      val done : I -> unit
      val next : 'a B -> 'a B option
    end =
  struct
      type I = bool ref * int
      type 'a A = {id:int, elem:'a, status:bool ref, depend_status: (bool ref * int) list} list
      type 'a elem = {id:int, elem:'a, status:bool ref, queued : bool ref, depend_status: bool ref list}
      type 'a B = 'a elem list
      type 'a Map =  (int,'a B list) Binarymap.dict
      type DepToken = bool ref * int
      fun done (r,_) = r:= true
      fun next x = ((Option.map #2) o List.getItem) x
      val firstB = (ref true,0)
      fun projectB [] = NONE
        | projectB (({elem,id,status,queued,depend_status}:'a elem) ::r) =
            if not (!queued) andalso List.all ! depend_status
            then (queued := true ; SOME (elem, (status,id)))
            else NONE
      fun fromMap (m,(_,i)) = case Binarymap.peek (m,i)
                          of NONE => []
                           | SOME l => List.filter (fn [] => false
                                                     | (elem::_) => not (!(#status (elem : 'a elem)))) l
      val emptyA = []
      val foldB = fn f => List.foldl (fn ({elem,id,status,queued,depend_status},acc) => f (elem,acc))
      fun add_first bg deps elem =
        let
          val r = ref false
          val i = case List.getItem bg
                  of NONE => 1
                   | SOME ({id,...},_) => Int.+(id,1)
        in
          ({id = i, elem = elem, status = r, depend_status = deps}::bg,(r,i))
        end
      local
        fun updatesMap (m,i,l) =
              case Binarymap.peek(m,i)
              of NONE => Binarymap.insert(m,i,[l])
               | SOME bl => Binarymap.insert(m,i,l::bl)
        fun turn' ({id,elem,status,depend_status},(acc,m)) =
         let
           val acc = {id=id,elem=elem,status=status,queued = ref false,depend_status = List.map (#1) depend_status}::acc
         in
           (acc,
            case List.partition (fn (br,i) => i <> id-1) depend_status
            of ([],[]) => updatesMap (m,0,acc)
             | ([],_) => m
             | (l,_) => List.foldl (fn ((_,i),m) => updatesMap(m,i,acc)) m l
            )
          end
      in
        fun turn l = List.foldl turn' ([],Binarymap.mkDict Int.compare) l
      end
    end

  structure BG = BuildGraph

  datatype Deps = Dep of MS.Bid.bid * BG.DepToken list * Deps list
  fun add_level_map [] = [[],[]]
    | add_level_map m = []::m
  fun wrap_level ([], bid,deps) = [[Dep(bid,deps,[])]]
    | wrap_level (m::[], bid,deps) = [[Dep(bid,deps,m)]]
    | wrap_level (m1::m2::r,bid,deps) = (Dep(bid,deps,m1)::m2)::r

  local
    fun wrap_local [] = []
      | wrap_local (m::[]) = (m::[])
      | wrap_local (m1::m2::r) = (m1 @ m2) :: r
    fun find_in_map _ [] = NONE : BG.DepToken list option
      | find_in_map lbid (Dep(b,deps,m)::r) =
          if #1(MS.Bid.longopen lbid) = b
          then
            case #2(MS.Bid.longopen lbid)
            of NONE => SOME deps
             | SOME lbid => find_in_map lbid m
          else find_in_map lbid r
  in
    fun kill_local (m1::m2::r) = wrap_local(m1::r)
      | kill_local r = r
    and find_in_maps _ [] = NONE : BG.DepToken list option
      | find_in_maps lbid (m::r) = case find_in_map lbid m
                                   of NONE => find_in_maps lbid r
                                    | SOME deps => SOME deps
  end

  local
(*    val unique = MlbFileSys.unique *)
    exception Cycle of (string * MlbFileSys.unique) list * (string * MlbFileSys.unique) option
    fun pathUpArrow(p,p_pre) = path_concat p_pre p
  in
    fun srcs_bdec_file {dir,current_mlb} state mlbs mlbfile_rel =
      let
        val mlbfile_abs = unique_true mlbfile_rel
        val _ = if List.exists (fn m => m = mlbfile_abs) current_mlb
                then raise Cycle ([(mlbfile_rel,mlbfile_abs)], NONE)
                else ()
      in
        case List.find (fn (a,_) => mlbfile_abs = a) mlbs
        of SOME (_,deps) => (state,deps, mlbs)
         | NONE =>
           let
             val {cd_old,file=mlbfile} = MlbFileSys.change_dir mlbfile_rel
           in
             let
               val (bdec,mlbhash) = parse mlbfile
               val dir = path_concat dir (OS.Path.dir mlbfile_rel)
                                       (* srcs_bdec state mlbfile mlbs dir [] [] bdec *)
(*
               val () = print ("CurrentDir: " ^ FileSys.getDir() ^ "\n")
               val () = print ("Dir before processing " ^ mlbfile ^ ": " ^ dir ^ "\n")
*)
               val (state,deps,mlbs) = (srcs_bdec state (mlbfile,mlbhash) mlbs {dir=dir,current_mlb = mlbfile_abs::current_mlb} [] [] bdec)
                 handle Cycle (l,cycend) =>
                   let
                     val dir = OS.Path.dir mlbfile_rel
                     val l = List.map (fn (s,u) => (pathUpArrow (s,dir),u)) l
                   in
                     raise Cycle((mlbfile_rel,mlbfile_abs)::l,
                                  case cycend
                                  of NONE => if #2(List.last l) = mlbfile_abs then SOME (mlbfile_rel,mlbfile_abs) else NONE
                                   | SOME (s,u) => SOME (pathUpArrow (s,dir),u))
                   end
             in
               (cd_old ()
              ; (state, deps, (mlbfile_abs,deps)::mlbs))
             end handle X => (cd_old (); raise X)
           end
      end

  and srcs_bdec state mlbfilehash mlbs dir anns deps bdec =
    case bdec
    of MS.SEQbdec (bdec1, bdec2) =>
      let
        val (s1,deps,mlbs) = srcs_bdec state mlbfilehash mlbs dir anns deps bdec1
      in
        srcs_bdec s1 mlbfilehash mlbs dir anns deps bdec2
      end
     | MS.EMPTYbdec => (state,deps,mlbs)
     | MS.LOCALbdec (bdec1,bdec2) =>
         let
           val (s1,deps,mlbs) = srcs_bdec {bg = #bg state, bid_map = add_level_map (#bid_map state)} mlbfilehash mlbs dir anns deps bdec1
           val (s2,deps,mlbs) = srcs_bdec {bg = #bg s1, bid_map = add_level_map (#bid_map s1)} mlbfilehash mlbs dir anns deps bdec2
         in
           ({bg = #bg s2, bid_map = kill_local (#bid_map s2)},deps,mlbs)
         end
     | MS.BASISbdec (bid,bexp) =>
       let
         val (s1,deps1,mlbs) = srcs_bexp {bg = #bg state, bid_map = add_level_map (#bid_map state)} mlbfilehash dir mlbs anns deps bexp
       in
         ({bg = #bg s1, bid_map = wrap_level (#bid_map s1, bid, deps1)},deps,mlbs)
       end
     | MS.OPENbdec l => (state,
                        List.foldl (fn (lbid,acc) => (case find_in_maps lbid (#bid_map state)
                                                      of NONE => []
                                                       | SOME d => d) @ acc) deps l
                         , mlbs)
     | MS.ATBDECbdec smlfile =>
       let
         val smlfile = path_concat (#dir dir) smlfile
         val (bg,deps) = BG.add_first (#bg state : (SmlFile * (string*string) * string list) BG.A) deps (Atom.fromString smlfile, mlbfilehash, anns)
       in
         ({bg = bg, bid_map = #bid_map state}, [deps], mlbs)
       end
     | MS.MLBFILEbdec mlbfile =>
       let
         val (ss,deps2,mlbs) = srcs_bdec_file dir state mlbs mlbfile
       in
         (ss,deps @ deps2, mlbs)
       end
     | MS.ANNbdec (ann,bdec) => srcs_bdec state mlbfilehash mlbs dir (ann::anns) deps bdec

  and srcs_bexp state mlbfilehash dir mlbs anns deps bexp =
    case bexp
    of MS.BASbexp bdec => srcs_bdec state mlbfilehash mlbs dir anns deps bdec
     | MS.LETbexp (bdec,bexp) =>
       let
         val (s1,deps,mlbs) = srcs_bdec state mlbfilehash mlbs dir anns deps bdec
       in
         srcs_bexp s1 mlbfilehash dir mlbs anns deps bexp
       end
     | MS.LONGBIDbexp bid => (state, case find_in_maps bid (#bid_map state) of NONE => [] | SOME d => d, mlbs)

(*
  fun pp_prior k [] = ""
    | pp_prior k ((x,y,z)::r) = "priority: " ^ (Int.toString k) ^ " " ^ x ^ " " ^ y ^ "\n" ^ (pp_prior k r)
  *)

  type Elem = SmlFile * (string*string) * string list
  type BG = Elem BG.B * Elem BG.Map
  type File = SmlFile * Elem BG.B * BG.I * (Elem BG.B) list

  local
      fun fromElem (e : Elem, acc) =
          let val a = #1 e
              val smlfile = Atom.toString a
              val (mlbfile,mlbhash) = #2 e
          in
            ((unique_true smlfile, (a,mlbfile)) :: acc)
            handle _ =>
                   MlbUtil.error
                   ("The file " ^ MlbUtil.quot smlfile ^ " (referenced in " ^ MlbUtil.quot mlbfile ^ ") does not exist or cannot be read")
          end

      fun list (es:Elem BG.B) : (MlbFileSys.unique*(SmlFile*string))list = BG.foldB fromElem [] es

      fun sort x = Listsort.sort (fn ((a,_),(b,_)) => MlbFileSys.cmp (a,b)) x
      fun filesEqual (f,g) =
          (case MlbFileSys.cmp (unique_true f, unique_true g) of
             EQUAL => true
           | _ => false)
          handle _ => false
      fun report (s,m1,m2) =
        let
          val s = Atom.toString s
          val first = "The file " ^ MlbUtil.quot s ^ " is referenced "
        in
          if filesEqual(m1,m2) then
            first ^ "more than once in " ^ MlbUtil.quot m1
          else
            first ^ "in both " ^ MlbUtil.quot m1 ^ " and " ^ MlbUtil.quot m2
        end
      fun check' [] = ()
        | check' [a] = ()
        | check' ((f1,(sml1 : Atom.atom,mlb1:string))::(a2 as (f2,(sml2,mlb2))):: r) =
            if f1 = f2
            then MlbUtil.error (report (sml1,mlb1,mlb2))
            else check' (a2::r)
      fun check (a:Elem BG.B) : unit = check' (sort (list a))
  in
    fun sources (mlbfile:string) =
        let
          (*val () = print ("Finding sources in " ^ mlbfile ^ "\n")*)
          val (a,b) = BG.turn (#bg(#1 (srcs_bdec_file {dir="",current_mlb = []}
                                                      {bg = BG.emptyA, bid_map = []}
                                                      [] mlbfile)))
        in (check a; (a,b))
        end
        handle Cycle (l,u) =>
               case u
                of NONE => error ("Bad cycle detected, this is a compiler bug")
                 | SOME (e,u) =>
                   let
                     fun cycstart [] = ["hmmm - empty cycle!"]
                       | cycstart [(s,us)] = [MlbUtil.quot s]
                       | cycstart ((s,us)::l) =
                         MlbUtil.quot s :: " -> " :: cycstart l
                   in
                     error (String.concat (["Cycle detected in MLB files:\n  "] @ cycstart l))
                   end
  end
  end

  fun project (a,_,_,_) = a
  val fold = fn f => fn b => fn (a,_) => BG.foldB (fn ((file,s,sl),b) => f(file,s,sl,b)) b a

  local
    fun file m (x:Elem BG.B) : (File * (string*string) * string list) option =
        Option.map (fn ((f,s,sl),i) => ((f,x,i,BG.fromMap (m,i)),s,sl)) (BG.projectB x)
  in
      fun initial (_,m) =
         let
           val l = BG.fromMap (m,BG.firstB)
         in
           List.mapPartial (file m) l
         end
      fun done ((_,x,i,l),(ll,m)) =
          let
            val _ = BG.done i
            val n = BG.next x
          in
            (List.mapPartial (file m) (case n of NONE => l | SOME n => n::l),(ll,m))
          end
  end

end
