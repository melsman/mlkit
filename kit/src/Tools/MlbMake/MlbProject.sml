functor MlbProject (Env : ENVIRONMENT) : MLB_PROJECT = 
    struct
(*	structure Bid :>
	    sig eqtype bid and longbid
		val bid : string -> bid
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
	    fun pp_bid s = s
	    fun longbid (bids:bid list) : longbid = bids
	    fun longopen nil = raise Fail "empty longbid"
	      | longopen [bid] = (bid,NONE)
	      | longopen (bid::longbid) = (bid, SOME longbid)
	    fun pp_longbid nil = raise Fail "empty longbid"
	      | pp_longbid [b] = pp_bid b
	      | pp_longbid (b::bs) = pp_bid b ^ "." ^ pp_longbid bs
	    fun explode ss = ss
	end

	fun supported_annotation s =
	    case s of
		"safeLinkTimeElimination" => true
	      | _ => false

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
		      | MLBFILEbdec of string * string option  (* path.mlb <scriptpath p> *)
	              | SCRIPTSbdec of atbdec list
	              | ANNbdec of string * bdec *)
  
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
        fun longopen nil = raise Fail "empty longbid"
          | longopen [bid] = (bid,NONE)
          | longopen (bid::longbid) = (bid, SOME longbid)
        fun pp_longbid nil = raise Fail "empty longbid"
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
             | MLBFILEbdec of string * string option  (* path.mlb <scriptpath p> *)
             | SCRIPTSbdec of atbdec list
             | ANNbdec of string * bdec

    fun supported_annotation s =
        case s of
      "safeLinkTimeElimination" => true
          | _ => false

    fun fold (SEQbdec(a,b))         e bas loc fopen fatbdec fmlb script ann = 
                  fold b (fold a e bas loc fopen fatbdec fmlb script ann)
                       bas loc fopen fatbdec fmlb script ann
      | fold EMPTYbdec              e bas loc fopen fatbdec fmlb script ann = e
      | fold (LOCALbdec(a,b))       e bas loc fopen fatbdec fmlb script ann = loc(b,loc(a,e))
      | fold (BASISbdec (bid,bexp)) e bas loc fopen fatbdec fmlb script ann = bas(bid,bexp,e)
      | fold (OPENbdec l)           e bas loc fopen fatbdec fmlb script ann = List.foldl fopen e l
      | fold (ATBDECbdec a)         e bas loc fopen fatbdec fmlb script ann = fatbdec(a,e)
      | fold (MLBFILEbdec (s,so))   e bas loc fopen fatbdec fmlb script ann = fmlb(s,so,e)
      | fold (SCRIPTSbdec l)        e bas loc fopen fatbdec fmlb script ann = List.foldl script e l 
      | fold (ANNbdec (ann',bdec))  e bas loc fopen fatbdec fmlb script ann = ann(ann',bdec,e)

  end

	val depDir : string ref = ref "PM"

	fun error (s : string) = 
	    (print ("\nError: " ^ s ^ ".\n\n"); raise Fail "MlbProject.error")

	fun warn (s : string) = print ("\nWarning: " ^ s ^ ".\n\n")

	fun quot s = "'" ^ s ^ "'"

	fun has_ext(s: string,ext) = 
	    case OS.Path.ext s of 
		SOME ext' => ext = ext'
	      | NONE => false
		    
	fun member s l = let fun m [] = false
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

	fun lex file chs : string list =
	    let 
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
                    fn #"\"" :: rest =>
                       let fun lp acc =
                               fn #"\"" :: rest => SOME (implode (rev acc), rest)
                                | c :: rest => lp (c::acc) rest
                                | [] =>
                                  error ("Unclosed string literal in project " ^
                                         quot(file))
                       in lp [] rest
                       end
                     | _ => NONE

		fun lex0 (chs : char list, acc) : string list =
		    case lex_whitesp chs of 
			[] => rev acc
		      | chs =>
                        lex0 (case lex_symbol chs of 
				 SOME (c,chs) => (chs, Char.toString c :: acc)
			       | NONE =>
                                 case lex_string_lit chs of
                                    SOME (s, chs) => (chs, s :: acc)
                                  | NONE =>
                                    let val (s, chs) = lex_string(chs,[])
				    in (chs, s::acc) 
				    end)
	    in lex0(chs,[])
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
                    case Env.getEnvVal pathVar of
		       SOME path => path
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
	      | "scripts" :: ss => 
			 let 
			     fun readSrcs (nil,_) = 
				 parse_error1 mlbfile ("Missing 'end' in 'scripts ... end' at end of file",nil)
			       | readSrcs ("end"::ss,acc) = (rev acc,ss)
			       | readSrcs (ss_all as (s::ss),acc) =				 
				 if is_smlfile s then readSrcs(ss,s::acc)
				 else parse_error1 mlbfile ("Expecting src-file or 'end' in 'scripts ... end' construct",ss)
			     val (srcs,ss) = readSrcs (ss,nil)
			 in parse_bdec_more mlbfile (MS.SCRIPTSbdec srcs,ss)
			 end
	      | "ann" :: ss =>
		    let 
			fun parse_rest'(anns,bdec,ss) =
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
			fun parse_rest(anns,ss) =
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
				 let val (opt,ss) =
				       (case ss of
					    "scriptpath" :: ss =>
						(case ss of
						     p :: ss' => 
							 if is_valid_path p then (SOME p,ss')
							 else parse_error1 mlbfile ("invalid path following SCRIPTPATH keyword",ss)
						   | _ => parse_error1 mlbfile ("missing path to follow SCRIPTPATH keyword",ss))
					  | _ => (NONE,ss))
				 in parse_bdec_more mlbfile (MS.MLBFILEbdec (expand mlbfile s,opt),ss)
				 end
			     else NONE
	      | nil => NONE

	fun fromFile' (filename:string) : string option =
	    SOME(MlbFileSys.fromFile filename) handle _ => NONE

	fun parse (mlbfile: string) : MS.bdec =
	    if not (has_ext(mlbfile, "mlb")) then 
		error ("The basis file " ^ quot mlbfile ^ " does not have extension 'mlb'")	    
	    else
		let (* val _ = print ("currently at " ^ OS.FileSys.getDir() ^ "\n") *)
		    val ss = (lex mlbfile o drop_comments mlbfile o
                              explode o MlbFileSys.fromFile) mlbfile
		    (* val _ = print_ss ss *)
		in  case parse_bdec_opt mlbfile ss of
		    SOME (bdec,nil) => bdec
		  | SOME (bdec,ss) => parse_error1 mlbfile ("misformed basis declaration", ss)
		  | NONE => MS.EMPTYbdec
		end
	    handle IO.Io {name=io_s,cause,...} => error ("The basis file " ^ quot mlbfile ^ " cannot be opened")


	(* Support for writing dependency information to disk in .d-files. *)

	fun dirMod dir file = if OS.Path.isAbsolute file then file
			      else OS.Path.concat(dir,file)

	structure DepEnv = 
	    struct
		type L = string list	    
		datatype D = D of L * (MS.Bid.bid * D) list
		fun lookup d longbid : D option =
		    let fun look nil _ = NONE
			  | look ((x,d)::xs) y = if x = y then SOME d else look xs y
			fun lookD _ nil = raise Fail "empty longbid"
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
		    in depMap (dirMod dir) dep
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

	fun mkAbs file = OS.Path.mkAbsolute{path=file,relativeTo=OS.FileSys.getDir()}

	fun subtractDir _ "" = (fn p => p)
	  | subtractDir hasLink dir =
      if hasLink
      then fn p => if OS.Path.isAbsolute p
                   then p
                   else OS.Path.concat (MlbFileSys.getCurrentDir (),p)
      else
	    let val dir_abs = mkAbs dir
	    in fn p =>
		let val p_abs = mkAbs p
        val _ = if OS.Path.isAbsolute dir then print ("mkAbs: " ^ p ^ " " ^ p_abs ^ " " ^ dir ^ "\n") else ()
		in OS.Path.mkRelative{path=p_abs,relativeTo=dir_abs}
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
                                             let val y' = OS.Path.concat (y,x)
                                             in (y',b orelse islink y')
                                             end)
                            (let val a = OS.Path.toString {vol = vol, isAbs = isAbs, arcs = [x]}
                                 in (a, islink a)
                                 end)
                            xr)
          end
  in
	fun maybeWriteDep smlfile (modTimeMlbFileMax:Time.time) D : unit =
	    let val {dir,file} = OS.Path.splitDirFile smlfile
		infix ## 
		val op ## = OS.Path.concat
		val file = dir ## (!depDir) ## (file ^ ".d")
	    in if fileNewer modTimeMlbFileMax file then ()         (* Don't write .d-file if the current .d-file *)
	       else                                                  (*  is newer than the mlb-file or any imported mlb-file. *)
		   let val L = DepEnv.getL D
		       val L = map (subtractDir (hasLinks dir) dir) L
		       val s = concat (map (fn s => s ^ " ") L)
		       fun write() =
			   ((if dir = "" then ()
			     else maybe_create_depdir dir);
			    writeDep file s)
		   in case fromFile' file of
		       SOME s' => if s = s' then ()                  (* Don't write .d-file if its content already *)
				  else write()                       (*  specifies the correct dependencies. *)
		     | NONE => write()
		   end
	    end
  end

	val op + = DepEnv.plus
	type D = DepEnv.D
	type A = {modTimeMlbFileMax: Time.time, mlbfiles: (string * D) list}   (* max-modtime of mlb-files met so far *)
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
	      | MS.MLBFILEbdec (mlbfile,_) => 
	            let val (D,A) = dep_bdec_file A mlbfile
		    in (DepEnv.dirModify (OS.Path.dir mlbfile, D), A)
		    end
	      | MS.SCRIPTSbdec smlfiles => 
		    let val t = #modTimeMlbFileMax A
			val _ = app (fn f => maybeWriteDep f t D) smlfiles
		    in (DepEnv.empty, A)
		    end
	      | MS.ANNbdec (s,bdec) => dep_bdec D A bdec

	and dep_bdec_file (A:A) mlbfile_rel : D * A =
	    let val mlbfile_abs = mkAbs mlbfile_rel
	    in
		case lookupA A mlbfile_abs of
		    SOME D => (D,A)
		  | NONE =>
			let val {cd_old,file=mlbfile} = MlbFileSys.change_dir mlbfile_rel
			in let val _ = maybe_create_depdir ""
			       val bdec = parse mlbfile
			       val modTimeMlbFileMaxOld = #modTimeMlbFileMax A
			       val modTimeMlbFileMaxNew = OS.FileSys.modTime mlbfile
				   handle _ => error ("Failed to read the modification time of the basis file " ^
						      mlbfile)
			       val A = {modTimeMlbFileMax=modTimeMlbFileMaxNew,
					mlbfiles= #mlbfiles A}
			       val (D,A) = dep_bdec DepEnv.empty A bdec
			   in  cd_old() ; 
			       (D, {modTimeMlbFileMax=timeMax(modTimeMlbFileMaxOld,modTimeMlbFileMaxNew),
				    mlbfiles=(mlbfile_abs,D) :: #mlbfiles A})
			   end handle X => (cd_old(); raise X)
			end
	    end

	fun dep (mlbfile : string) : unit =
	    (dep_bdec_file emptyA mlbfile; ())
(*
	fun map2 f ss = map (fn (x,y,a) => (f x, f y,a)) ss *)

	(* Support for finding the source files of a basis file *)

	datatype srctype = SRCTYPE_ALL | SRCTYPE_SCRIPTSONLY | SRCTYPE_ALLBUTSCRIPTS

	val emptyA = nil

  structure Priority :>
    sig
      type 'a P
      val empty : (('a * 'a) -> 'a) -> 'a P
      val single : (int * 'a * (('a * 'a) -> 'a)) -> 'a P
      val union : ('a P * 'a P) -> 'a P
      val map : ('b * 'b -> 'b) -> (int * 'a -> 'b) -> 'a P -> 'b P
      val fold : (int * 'a * 'b -> 'b) -> 'b -> 'a P -> 'b
      type Q
      val fresh : Q
      val new : Q -> Q
      val lookup : Q -> MS.Bid.longbid -> int option
      val extend : Q -> Q -> MS.Bid.bid -> int -> Q
      val up : (MS.Bid.longbid * Q) -> Q
      val loc : Q -> Q -> Q

    end = 
    struct
      datatype 'a P = Pair of ((int,'a) Binarymap.dict * (('a * 'a) -> 'a))
      val empty = fn comp => Pair (Binarymap.mkDict Int.compare, comp)
      val single = fn (k,a,comp) => Pair (Binarymap.insert (case (empty comp) of Pair (b,_) => b,k,a),comp)
      val union = fn (Pair(a,_),Pair(b,comp)) => Pair(Binarymap.foldl
                     (fn (k,f,acc) =>
                        case Binarymap.peek (acc,k)
                        of NONE => (Binarymap.insert(acc,k,f))
                         | SOME l => (Binarymap.insert(acc,k,comp(f,l)))) b a,
                      comp)
      val toList = fn Pair(a,_) => Binarymap.foldr (fn (k,b,acc) => b :: acc) [] a
      val fold = fn f => fn e => fn Pair(a,_) => Binarymap.foldl f e a
      val map = fn c => fn f => fn Pair(a,_) => Pair(Binarymap.map f a,c)

      datatype E = Empty 
                 | Map of (MS.Bid.bid,(E * int)) Binarymap.dict 
      type Q = (E * E)
      val fresh = Binarymap.mkDict MS.Bid.bidCompare : (MS.Bid.bid,(E * int)) Binarymap.dict
      fun new (A,_) = (A,Empty)
      fun lookup (Empty,_) _  = NONE
        | lookup _ [] = NONE
        | lookup (Map m,_) [b] = Option.map (fn (_,i) => i) (Binarymap.peek (m,b))
        | lookup (Map m,A) (b::lb) = case Binarymap.peek (m,b)
                                     of NONE => NONE
                                      | SOME (m',_) => lookup (m',A) lb
      val lookup = fn q => fn lb => lookup q (MS.Bid.explode lb)
      fun update bid n A Empty = Map (Binarymap.insert (fresh,bid,(A,n)))
        | update bid n A (Map m) = Map (Binarymap.insert (m,bid,(A,n)))
      fun extend (M,A) (_,B) bid p = (update bid p B M, update bid p B A)
      fun loc (M,A) (N,Empty) = (M,A)
        | loc (Empty,Empty) (_,Map m) = (Map m, Map m)
        | loc (m,Empty) (_,Map m') = (Binarymap.foldl (fn (bid,(e,i),acc) => update bid i e acc) m m', Map m')
        | loc (Empty,m) (_,Map m') = (Map m', Binarymap.foldl (fn (bid,(e,i),acc) => update bid i e acc) m m')
        | loc (M,A) (B,Map m) = let
                                  val (a,_) = loc (M,Empty) (B,Map m)
                                  val (_,b) = loc (Empty,A) (B,Map m)
                                in (a,b) end
      fun up Empty _ _ = Empty
        | up (Map m) _ [] = Map m
        | up (Map m) Empty _ = Map m
        | up (Map m) (Map m') [b] = (case Binarymap.peek (m',b) 
                                    of NONE => (Map m)
                                     | SOME (e,n) => Map (Binarymap.insert (m,b,(e,n))))
        | up (Map m) (Map m') (b::bl) = up (Map m) (case Binarymap.peek (m',b)
                                                    of NONE => Empty
                                                     | SOME (e,_) => e) 
                                                   bl
      val up = fn (lb,(q,e)) => (up q q (MS.Bid.explode lb), up e e (MS.Bid.explode lb))
      val fresh = (Empty,Empty)
    end

	fun map2 f f' ss = Priority.map op@ (fn (k,b) => List.map (fn (x,y,a) => (f x, f' y,a)) b) ss

  fun max (a,b) = if a < b then b else a

    structure Atom = 
      struct
        type atom = string
        val toString = fn x => x
        val fromString = fn x => x
        val compare = String.compare
      end
    datatype SmlFile = Script of Atom.atom
                     | NonScript of Atom.atom
    fun smlfilemap f (Script a) = Script (f a)
      | smlfilemap f (NonScript a) = NonScript (f a)

  fun stripFile (Script a) = a
    | stripFile (NonScript a) = a

	fun dirMod2 dir file = if OS.Path.isAbsolute (Atom.toString (stripFile file)) then file
			      else smlfilemap (fn a => Atom.fromString (OS.Path.concat(dir,Atom.toString a))) file


	fun srcs_bdec_file T mlbs mlbfile_rel =
	    let val mlbfile_abs = mkAbs mlbfile_rel
	    in case List.find (fn (a,_) => mlbfile_abs = a) mlbs 
         of SOME(_,p) => (Priority.empty op@ ,mlbs,p,Priority.fresh)
	        | NONE => let val {cd_old,file=mlbfile} = MlbFileSys.change_dir mlbfile_rel
		    in 
			let val bdec = parse mlbfile
			    val (ss : (SmlFile * string * string list) list Priority.P , mlbs,p,e) = srcs_bdec 0 Priority.fresh T emptyA mlbfile mlbs bdec
			    val ss = map2 (dirMod2 (OS.Path.dir mlbfile_rel)) (dirMod (OS.Path.dir mlbfile_rel)) ss
			in cd_old()
			    ; (ss,(mlbfile_abs,p)::mlbs,p,e)
			end handle X => (cd_old(); raise X)
		    end 
	    end

	and srcs_bdec p e T A mlbfile mlbs bdec =
	    case bdec of 
		MS.SEQbdec (bdec1,bdec2) =>
		    let val (ss1, mlbs, p, e) = srcs_bdec p e T A mlbfile mlbs bdec1
			val (ss2,mlbs,p,e) = srcs_bdec p e T A mlbfile mlbs bdec2
		    in (Priority.union (ss1,ss2),mlbs,p,e)
		    end
	      | MS.EMPTYbdec => (Priority.empty op@,mlbs,p,e)
	      | MS.LOCALbdec (bdec1,bdec2) =>
		    let val (ss1,mlbs,p',e') = srcs_bdec p e T A mlbfile mlbs bdec1
			val (ss2,mlbs,p',e') = srcs_bdec p' (Priority.new e') T A mlbfile mlbs bdec2
		    in (Priority.union(ss1,ss2),mlbs, p', Priority.loc e e')
		    end
	      | MS.BASISbdec (bid,bexp) => let
                                       val (ss,mlbs,p',e') = (srcs_bexp p (Priority.new e) T A mlbfile mlbs bexp)
                                     in
                                       (ss,mlbs,p, Priority.extend e e' bid p')
                                     end
	      | MS.OPENbdec l => (Priority.empty op@,mlbs, List.foldl (fn (x,i) => case Priority.lookup e x
                                                                             of NONE => i
                                                                              | SOME i' => max(i,i')) p l,
                                                     List.foldl (fn (a,e') => Priority.up (a,e')) e l)
	      | MS.ATBDECbdec smlfile => 
		    let val L = (* case T of SRCTYPE_SCRIPTSONLY => Priority.empty op@ 
		                        | _ => *) Priority.single (p,[(NonScript (Atom.fromString smlfile),mlbfile,A)],op@)
		    in (L,mlbs,Int.+(p,1),e)
		    end
	      | MS.MLBFILEbdec (mlbfile,_) => let val (ss,mlbs,p',e) = srcs_bdec_file T mlbs mlbfile
                                        in (ss,mlbs, max(p,p'), e)
                                        end
	      | MS.SCRIPTSbdec smlfiles => 
		    let val L = (* case T of SRCTYPE_ALLBUTSCRIPTS => Priority.empty op@
		                        | _  => *) Priority.single (p,map (fn f => (Script (Atom.fromString f),mlbfile,A)) smlfiles,op@)
		    in (L,mlbs,p,e)
		    end
	      | MS.ANNbdec (ann,bdec) => srcs_bdec p e T (ann::A) mlbfile mlbs bdec

	and srcs_bexp p e T A mlbfile mlbs bexp =
	    case bexp of
		MS.BASbexp bdec => srcs_bdec p e T A mlbfile mlbs bdec
	      | MS.LETbexp (bdec,bexp) =>
		    let val (ss1,mlbs,p,e) = srcs_bdec p e T A mlbfile mlbs bdec
			val (ss2,mlbs,p,e) = srcs_bexp p e T A mlbfile mlbs bexp
		    in (Priority.union(ss1,ss2),mlbs,p,e)
		    end
	      | MS.LONGBIDbexp bid => (Priority.empty op@,mlbs, case Priority.lookup e bid
                                                          of NONE => p
                                                           | SOME p' => max (p,p'),
                                                          e)
  fun pp_prior k [] = ""
    | pp_prior k ((x,y,z)::r) = "priority: " ^ (Int.toString k) ^ " " ^ x ^ " " ^ y ^ "\n" ^ (pp_prior k r)
	
	fun sources (* (T:srctype) *) (mlbfile : string) =
	    List.rev (Priority.fold (fn (k,a,acc) => ((List.map (fn (a,b,c) => (k,a,b,c)) a) @ acc)) [] (#1 (srcs_bdec_file () nil mlbfile)))
    
    type BG = (int * SmlFile * string * string list) list
    type File = SmlFile * int
    fun project (a,b) = a
    val fold = fn f => List.foldl (fn ((_,file,s1,s2),acc) => f(file,s1,s2,acc))
    fun take i = 
      let 
        fun t acc [] = acc
          | t acc ((j,a,b,c)::r) = if j <= i then t (((a,j),b,c)::acc) r else acc
      in
       fn l => t [] l
      end
    fun initial [] = []
      | initial g = take 0 g 
    fun remove d [] = []
      | remove d ((i,a,b,c)::r) = case Atom.compare (stripFile a,d)
                                  of EQUAL => r
                                   | _ => (i,a,b,c) :: (remove d r)
    fun next i [] = ([],[])
      | next i (l as ((j,_,_,_)::r)) = if i < j then (take j l,l) 
                                       else ([], l)
    fun done ((d,i),[]) = ([],[])
      | done ((d,i),g) = next i (remove (stripFile d) g)

    end
	
