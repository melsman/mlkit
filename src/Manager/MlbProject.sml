functor MlbProject () : MLB_PROJECT = 
    struct
	structure Bid :>
	    sig eqtype bid and longbid
		val bid : string -> bid
		val longbid : bid list -> longbid
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
	    fun pp_longbid nil = raise Fail "empty longbid"
	      | pp_longbid [b] = pp_bid b
	      | pp_longbid (b::bs) = pp_bid b ^ "." ^ pp_longbid bs
	    fun explode ss = ss
	end

	datatype bexp = BASbexp of bdec
                      | LETbexp of bdec * bexp
                      | LONGBIDbexp of Bid.longbid

             and bdec = SEQbdec of bdec * bdec
	              | EMPTYbdec 
                      | LOCALbdec of bdec * bdec
                      | BASISbdec of Bid.bid * bexp
                      | OPENbdec of Bid.longbid list
	              | SMLFILEbdec of string  (* path.{sml,sig} *)
		      | MLBFILEbdec of string  (* path.mlb *)

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

	fun lex chs : string list =
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

		fun lex0 (chs : char list, acc) : string list =
		    case lex_whitesp chs of 
			[] => rev acc
		      | chs => lex0 (case lex_symbol chs of 
					 SOME (c,chs) => (chs, Char.toString c :: acc)
				       | NONE => let val (s, chs) = lex_string(chs,[])
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
	    has_ext(s,"sml") orelse has_ext(s,"sig")

	fun is_id s = 
	    case explode s of
		c::cs => Char.isAlpha c 
		    andalso List.all (fn c => Char.isAlphaNum c 
				      orelse c = #"_"
				      orelse c = #"'") cs
	      | _ => false

	local
	    fun is_keyword s = 
		case s of
		    "open" => true
		  | "let" => true
		  | "local" => true
		  | "in" => true
		  | "end" => true
		  | "bas" => true
		  | "basis" => true
		  | _ => false
			
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
		in if List.all is_bid fs then SOME (Bid.longbid (map Bid.bid fs))
		   else NONE
		end
	end

	fun print_ss ss = (print "Tokens: "; app (fn s => print (s ^ " ")) ss; print "\n")
			
        fun parse_error mlbfile msg = error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg)
        fun parse_error1 mlbfile (msg, rest: string list) = 
          case rest of 
            [] => error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg ^ "(reached end of file)")
          | s::_ => error ("while parsing basis file " ^ quot mlbfile ^ " : " ^ msg ^ "(reached " ^ quot s ^ ")")


	fun parse_bdec_more mlbfile (bdec,ss) =
	    case parse_bdec_opt mlbfile ss of
		SOME(bdec',ss) => SOME(SEQbdec(bdec,bdec'),ss)
	      | NONE => SOME(bdec,ss)

	and parse_bexp mlbfile (ss:string list) : bexp * string list =	    
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
					    "end" :: ss => (LETbexp(bdec,bexp),ss)
					  | _ => parse_error1 mlbfile ("missing 'end'", ss)
				    end
			      | _ => parse_error1 mlbfile ("missing 'in'", ss)
		    in case parse_bdec_opt mlbfile ss of 
			NONE => parse_rest(EMPTYbdec,ss)
		      | SOME(bdec,ss) => parse_rest(bdec,ss)
		    end
	      | "bas" :: ss => 
		    let 
			fun parse_rest(bdec,ss) =
			    case ss of
				"end" :: ss => (BASbexp bdec, ss)
			      | _ => parse_error1 mlbfile ("missing 'end'", ss)
		    in
			case parse_bdec_opt mlbfile ss of 
			    NONE => parse_rest(EMPTYbdec,ss)
			  | SOME(bdec,ss) => parse_rest(bdec,ss)
		    end
	      | s :: ss' => 
		    (case is_longbid s of
			 SOME longbid => (LONGBIDbexp longbid,ss')
		       | NONE => parse_error1 mlbfile ("invalid basis expression", ss))

	and parse_bdec_opt mlbfile ss =
	    case ss of
		"local" :: ss => 
		    let 
			fun parse_rest'(bdec,bdec',ss) =
			    case ss of 
				"end" :: ss => parse_bdec_more mlbfile (LOCALbdec(bdec,bdec'),ss)
			      | _ => parse_error1 mlbfile ("I expect an 'end'", ss)
			fun parse_rest(bdec,ss) =
			    case ss of 
				"in" :: ss => 
				    (case parse_bdec_opt mlbfile ss of 
					 NONE => parse_rest'(bdec,EMPTYbdec,ss)
				       | SOME(bdec',ss) => parse_rest'(bdec,bdec',ss))
			      | _ => parse_error1 mlbfile ("I expect an 'in'", ss)
		    in case parse_bdec_opt mlbfile ss of 
			NONE => parse_rest(EMPTYbdec,ss)
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
				      in parse_bdec_more mlbfile (BASISbdec(Bid.bid bid,bexp),ss)
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
			 in parse_bdec_more mlbfile (OPENbdec longbids,ss)
			 end
	      | s :: ss =>
			 if is_smlfile s then parse_bdec_more mlbfile (SMLFILEbdec s,ss)
			 else if is_mlbfile s then parse_bdec_more mlbfile (MLBFILEbdec s,ss)
			      else NONE
	      | nil => NONE

        fun fromFile (filename:string) =
	    let val is = TextIO.openIn filename 
		(*val _ = TextIO.output(TextIO.stdOut, "opened " ^filename^"\n")*)
		val s = TextIO.inputAll is handle E => (TextIO.closeIn is; raise E)
	    in TextIO.closeIn is; s
	    end

	fun fromFile' (filename:string) : string option =
	    SOME(fromFile filename) handle _ => NONE

	fun parse (mlbfile: string) : bdec =
	    if not (has_ext(mlbfile, "mlb")) then 
		error ("The basis file " ^ quot mlbfile ^ " does not have extension 'mlb'")	    
	    else
		let val ss = (lex o (drop_comments mlbfile) o explode o fromFile) mlbfile
		    (* val _ = print_ss ss *)
		in  case parse_bdec_opt mlbfile ss of
		    SOME (bdec,nil) => bdec
		  | SOME (bdec,ss) => parse_error1 mlbfile ("misformed basis declaration", ss)
		  | NONE => parse_error1 mlbfile ("missing basis declaration", ss)
		end
	    handle IO.Io {name=io_s,...} => error ("The basis file " ^ quot mlbfile ^ " cannot be opened")


	(* Support for writing dependency information to disk in .d-files. *)

	fun dirMod dir file = if OS.Path.isAbsolute file then file
			      else OS.Path.concat(dir,file)
			
	structure DepEnv = 
	    struct
		type L = string list	    
		datatype D = D of L * (Bid.bid * D) list
		fun lookup d longbid : D option =
		    let fun look nil _ = NONE
			  | look ((x,d)::xs) y = if x = y then SOME d else look xs y
			fun lookD _ nil = raise Fail "empty longbid"
			  | lookD (D(_,xs)) [y] = look xs y
			  | lookD (D(_,xs)) (y::ys) =
			    case look xs y of
				SOME d => lookD d ys
			      | NONE => NONE
		    in lookD d (Bid.explode longbid)
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

	val older = Time.<

	fun maybe_create_dir d : unit = 
	    if OS.FileSys.access (d, []) handle _ => error ("I cannot access directory " ^ quot d) then
		if OS.FileSys.isDir d then ()
		else error ("The file " ^ quot d ^ " is not a directory")
	    else ((OS.FileSys.mkDir d;()) handle _ => 
		  error ("I cannot create directory " ^ quot d ^ " --- the current directory is " ^ 
			 OS.FileSys.getDir()))
		
	fun maybe_create_depdir() =
	    let val dirs = String.tokens (fn c => c = #"/") (!depDir)
		fun append_path "" d = d
		  | append_path p d = p ^ "/" ^ d
		fun loop (p, nil) = ()
		  | loop (p, d::ds) = let val p = append_path p d
				      in maybe_create_dir p; loop(p, ds)
				      end
	    in loop("", dirs)
	    end

	fun writeDep file s =
	    (let val os = TextIO.openOut file       (* what about general paths *)
	     in (  TextIO.output(os,s)
		 ; TextIO.closeOut os
		 (* ; print ("File " ^ file ^ " updated\n") *)
                ) handle ? => (TextIO.closeOut os; raise ?)
	     end handle _ => error("Failed to write dependencies to file " ^ quot file))

	fun mlbFileIsOlder modTimeMlbFile file : bool =
	    older (modTimeMlbFile, OS.FileSys.modTime file) handle _ => false

	fun mkAbs file = OS.Path.mkAbsolute(file,OS.FileSys.getDir())

	fun subDir "" = (fn p => p)
	  | subDir dir =
	    let val dir_abs = mkAbs dir
	    in fn p =>
		let val p_abs = mkAbs p
		in OS.Path.mkRelative(p_abs,dir_abs)
		end
	    end

	fun maybeWriteDep smlfile modTimeMlbFile D : unit =
	    let val {dir,file} = OS.Path.splitDirFile smlfile
		infix ## 
		val op ## = OS.Path.concat
		val file = dir ## (!depDir) ## (file ^ ".d")
	    in if mlbFileIsOlder modTimeMlbFile file then ()         (* Don't write .d-file if the current .d-file *)
	       else                                                  (*  is newer than the mlb-file! *) 
		   let val L = DepEnv.getL D
		       val L = map (subDir dir) L
		       val s = concat (map (fn s => s ^ " ") L)
		   in case fromFile' file of
		       SOME s' => if s = s' then ()                  (* Don't write .d-file if its content already *)
				  else writeDep file s               (*  specifies the correct dependencies. *)
		     | NONE => writeDep file s
		   end
	    end
	    
	val op + = DepEnv.plus
	type D = DepEnv.D
	type A = {modTimeMlbFile: Time.time option, mlbfiles: (string * D) list}
	val emptyA = {modTimeMlbFile=NONE,mlbfiles=nil}
	fun lookupA {modTimeMlbFile, mlbfiles} mlbfile : D option =
	    let fun look nil = NONE
		  | look ((x,D)::xs) = if mlbfile = x then SOME D
				       else look xs
	    in look mlbfiles
	    end

	fun change_dir path : {cd_old : unit -> unit, file : string} =
	    let val {dir,file} = OS.Path.splitDirFile path
	    in if dir = "" then {cd_old = fn()=>(),file=file}
	       else let val old_dir = OS.FileSys.getDir()
			val _ = OS.FileSys.chDir dir
		    in {cd_old=fn()=>OS.FileSys.chDir old_dir, file=file}
		    end handle OS.SysErr _ => error ("Failed to access directory " ^ quot dir)
	    end

	fun dep_bexp (D:D) (A:A) bexp : D * A =
	    case bexp of
	        BASbexp bdec => dep_bdec D A bdec
	      | LETbexp (bdec,bexp) =>
		    let val (D1,A) = dep_bdec D A bdec
			val (D2,A) = dep_bexp (D+D1) A bexp
		    in (D2,A)
		    end
	      | LONGBIDbexp longbid =>
		    (case DepEnv.lookup D longbid of
			 SOME D => (D,A)
		       | NONE => error ("The long basis identifier " 
					^ Bid.pp_longbid longbid 
					^ " is undefined"))

	and dep_bdec (D:D) (A:A) bdec : D * A =
	    case bdec of
		SEQbdec (bdec1,bdec2) =>
		    let val (D1,A) = dep_bdec D A bdec1
			val (D2,A) = dep_bdec (D + D1) A bdec2
		    in (D1 + D2, A)
		    end
	      | EMPTYbdec => (DepEnv.empty,A)
	      | LOCALbdec (bdec1,bdec2) =>
		    let val (D1,A) = dep_bdec D A bdec1
			val (D2,A) = dep_bdec (D + D1) A bdec2
		    in (D2,A)
		    end
	      | BASISbdec (bid, bexp) =>
		    let val (D',A) = dep_bexp D A bexp
		    in (DepEnv.singleBidEntry (bid,D'), A)
		    end
	      | OPENbdec longbids =>
		    let val D' = 
			foldl (fn (longbid,Dacc) => 
			       case DepEnv.lookup D longbid of
				   SOME D => Dacc + D
				 | NONE => error ("The long basis identifier " 
						  ^ Bid.pp_longbid longbid 
						  ^ " is undefined")) DepEnv.empty longbids
		    in (D',A)
		    end
	      | SMLFILEbdec smlfile =>
		    (case #modTimeMlbFile A of
			 SOME t => (  maybeWriteDep smlfile t D
				    ; (DepEnv.singleFile smlfile, A))
		       | NONE => error "No modification time available for the basis file")
	      | MLBFILEbdec mlbfile => 
			 let val (D,A) = dep_bdec_file A mlbfile
			 in (DepEnv.dirModify (OS.Path.dir mlbfile, D), A)
			 end

	and dep_bdec_file (A:A) mlbfile_rel : D * A =
	    let val mlbfile_abs = mkAbs mlbfile_rel
	    in
		case lookupA A mlbfile_abs of
		    SOME D => (D,A)
		  | NONE =>
			let val {cd_old,file=mlbfile} = change_dir mlbfile_rel
			in let val _ = maybe_create_depdir()
			       val bdec = parse mlbfile
			       val modTimeMlbFileTmp = #modTimeMlbFile A
			       val modTimeMlbFile = OS.FileSys.modTime mlbfile
				   handle _ => error ("Failed to read the modification time of the basis file " ^
						      mlbfile)
			       val A = {modTimeMlbFile=SOME(modTimeMlbFile),
					mlbfiles= #mlbfiles A}
			       val (D,A) = dep_bdec DepEnv.empty A bdec
			   in  cd_old() ; 
			       (D, {modTimeMlbFile = modTimeMlbFileTmp,
				    mlbfiles=(mlbfile_abs,D) :: #mlbfiles A})
			   end handle X => (cd_old(); raise X)
			end
	    end

	fun dep (mlbfile : string) : unit =
	    (dep_bdec_file emptyA mlbfile; ())


	(* Support for finding the source files of a basis file *)
	fun srcs_bdec_file mlbs mlbfile_rel =
	    let val mlbfile_abs = mkAbs mlbfile_rel
	    in if member mlbfile_abs mlbs then (nil,mlbs)
	       else let val {cd_old,file=mlbfile} = change_dir mlbfile_rel
		    in 
			let val bdec = parse mlbfile
			    val (ss, mlbs) = srcs_bdec mlbs bdec
			    val ss = map (dirMod (OS.Path.dir mlbfile_rel)) ss
			in cd_old()
			    ; (ss,mlbfile_abs::mlbs)
			end handle X => (cd_old(); raise X)
		    end 
	    end

	and srcs_bdec mlbs bdec =
	    case bdec of 
		SEQbdec (bdec1,bdec2) =>
		    let val (ss1,mlbs) = srcs_bdec mlbs bdec1
			val (ss2,mlbs) = srcs_bdec mlbs bdec2
		    in (ss1@ss2,mlbs)
		    end
	      | EMPTYbdec => (nil,mlbs)
	      | LOCALbdec (bdec1,bdec2) =>
		    let val (ss1,mlbs) = srcs_bdec mlbs bdec1
			val (ss2,mlbs) = srcs_bdec mlbs bdec2
		    in (ss1@ss2,mlbs)
		    end
	      | BASISbdec (bid,bexp) => srcs_bexp mlbs bexp
	      | OPENbdec _ => (nil,mlbs)
	      | SMLFILEbdec smlfile => ([smlfile],mlbs)
	      | MLBFILEbdec mlbfile => srcs_bdec_file mlbs mlbfile
		    
	and srcs_bexp mlbs bexp =
	    case bexp of
		BASbexp bdec => srcs_bdec mlbs bdec
	      | LETbexp (bdec,bexp) =>
		    let val (ss1,mlbs) = srcs_bdec mlbs bdec
			val (ss2,mlbs) = srcs_bexp mlbs bexp
		    in (ss1@ss2,mlbs)
		    end
	      | LONGBIDbexp _ => (nil,mlbs)
	
	fun sources (mlbfile : string) : string list =
	    #1 (srcs_bdec_file nil mlbfile)
    
    end
	