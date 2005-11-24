functor UlFile (MlbProject : MLB_PROJECT)
    : ULFILE where type bdec = MlbProject.bdec =
    struct
	structure Mlb = MlbProject
	structure Bid = Mlb.Bid

	fun die s = 
	    let val s = "UlFile.Error: " ^ s
	    in print(s ^ "\n"); raise Fail s
	    end

	local
	    fun change_dir dir : {cd_old : unit -> unit} =
	        if dir = "" then {cd_old = fn()=>()}
		else let val old_dir = OS.FileSys.getDir()
			 val _ = OS.FileSys.chDir dir
		     in {cd_old=fn()=>OS.FileSys.chDir old_dir}
		     end handle OS.SysErr _ => die ("I cannot access directory " ^ dir)
	in
	    fun doCD dir (f : unit -> 'a) : 'a =
		let val {cd_old} = change_dir dir
		in (f() before cd_old())
		    handle X => (cd_old(); raise X)
		end
	end
	    

	type bid = Bid.bid
	type longbid = Bid.longbid
	type bdec = Mlb.bdec
	type location = string   (* request location (as seen in browser) *)
	type uofile = string     (* path to uofile on file system *)
	type smlfile = string
	type phi = smlfile -> uofile list	   

	datatype ul = SEQul of ul * ul
	  | SCRIPTSul of (uofile * location) list
	  | CODEFILESul of uofile list
	  | EMPTYul

	fun pp_uo_loc (uo,loc) : string =
	    concat[" ", OS.Path.mkCanonical uo, " As ", loc, "\n"]
	    
	fun pp_uo uo : string =
	    concat[" ", OS.Path.mkCanonical uo, "\n"]

	fun pp_ul0 ul : string list=
	    case ul of
		SEQul (ul1,ul2) => pp_ul0 ul1 @ pp_ul0 ul2
	      | SCRIPTSul uo_loc_s => 
		    ("Scripts\n" :: map pp_uo_loc uo_loc_s @ ["End\n"])
	      | CODEFILESul uo_s =>
		    ("Codefiles\n" :: map pp_uo uo_s @ ["End\n"])
	      | EMPTYul => [""]

	fun pp_ul (ul:ul) : string = 
	    concat(pp_ul0 ul)
		
	type S = (uofile*location)list
	type C = uofile list
	datatype B = B0 of (Bid.bid * (B*S)) list
	type M = (string * (B*S)option) list
	 
	fun norm(p,r) =
	    if p = r orelse p="" then ""
	    else let val {dir,file=arc} = OS.Path.splitDirFile p
		     val tmp = norm(dir,r)
		 in OS.Path.concat(tmp,arc)
		 end

	fun pathDownArrow(p,r) = 
	    if OS.Path.isAbsolute p then p
	    else OS.Path.mkCanonical (norm(p,r))
	    handle _ => die ("pathDownArrow: p=" ^ p^ "; r=" ^ r)

	fun pathUpArrow(p,p_pre) = 
	    if OS.Path.isAbsolute p then p
	    else OS.Path.mkCanonical(OS.Path.concat(p_pre,p))
	    handle _ => die "pathUpArrow"

        structure S =
	    struct
		val empty : S = nil
		fun insert (p,S) = p::S
		fun plus (S1,S2) = foldr insert S1 S2
		fun list (a:S):(uofile*location)list = rev a
		fun extendLoc scriptpath S =
		    map (fn (uo,loc) => (uo,OS.Path.concat(scriptpath,loc))) S
		fun on f S =
		    map (fn (uo,loc) => (f uo,loc)) S
		fun upArrow(S,p) = 
		    on (fn x => pathUpArrow(x,p)) S
	    end
	structure B =
	    struct	       
		val empty : B = B0 nil
		fun insert ((bid,(Bsub,S)),B0 B) = B0((bid,(Bsub,S))::B)
		fun lookup bid (B0 nil) = NONE
		  | lookup bid (B0((x,(Bsub,S))::xs)) = if bid=x then SOME (Bsub,S)
							else lookup bid (B0 xs)
		fun lookupl lbid B =
		    let val (bid,opt) = Bid.longopen lbid
		    in case lookup bid B of
			NONE => NONE
		      | SOME(Bs,S) => 
			    (case opt of
				 SOME lbid => lookupl lbid Bs
			       | NONE => SOME(Bs,S))
		    end
		fun plus (B1,B0 B2) = foldr insert B1 B2
		fun extendLoc scriptpath (B0 l) =
		    B0(map (fn (bid,(Bsub,S)) => 
			    (bid,(extendLoc scriptpath Bsub,S.extendLoc scriptpath S)))
		       l)
		fun on f (B0 l) =
		    B0 (map (fn (bid,(Bsub,S)) => (bid,(on f Bsub,S.on f S))) l)
		fun upArrow(B,p) = 
		  on (fn x => pathUpArrow(x,p)) B
	    end
	structure M =
	    struct	       
		val empty : M = nil
		fun insert (s,l,M:M):M = (s,l)::M
		fun lookup s nil = NONE
		  | lookup s ((x,l:(B*S)option)::xs) = if s=x then SOME l
					   else lookup s xs
		fun on f M =
		    map (fn (s,l) =>
			 (f s, Option.map (fn (B,S) => (B.on f B,S.on f S)) l)) 
		    M
					   
		fun downArrow(M,p) = 
		  on (fn x => pathDownArrow(x,p)) M
		fun upArrow(M,p) = 
		  on (fn x => pathUpArrow(x,p)) M
	    end
        structure C =
	    struct
		val empty : C = nil
		fun insert (uo,C) = uo::C
		fun plus (C1,C2) = foldr insert C1 C2
		fun fromList l = rev l
		fun list a = rev a
		fun on f C = map f C
		fun upArrow(C,p) = 
		    on (fn x => pathUpArrow(x,p)) C
	    end

	(* ulb:   M,B |- bdec => S,C,M',B' *)
	fun ulb (phi:phi) (M:M) (B:B) (bdec:bdec) 
	    : S * C * M * B =
	    case bdec of
		Mlb.SEQbdec (bdec1, bdec2) =>
		    let val (S1,C1,M,B1) = ulb phi M B bdec1
			val (S2,C2,M,B2) = ulb phi M (B.plus(B,B1)) bdec2
		    in (S.plus(S1,S2),C.plus(C1,C2),M,B.plus(B1,B2))
		    end
	      | Mlb.EMPTYbdec => (S.empty,C.empty,M,B.empty)
	      | Mlb.LOCALbdec (bdec1, bdec2) =>
		    let val (S1,C1,M,B1) = ulb phi M B bdec1
			val (S2,C2,M,B2) = ulb phi M (B.plus(B,B1)) bdec2
		    in (S2,C.plus(C1,C2),M,B2)
		    end
	      | Mlb.BASISbdec (bid, bexp) =>
		    let val (S,C,M,B1) = ule phi M B bexp
		    in (S.empty,C,M.empty,B.insert((bid,(B1,S)),B.empty))
		    end
	      | Mlb.OPENbdec lbids =>
		    let val (Ba,Sa) = foldl (fn (lbid,(Ba,Sa)) => 
					     case B.lookupl lbid B of
						 SOME (B',S') => (B.plus(Ba,B'),S.plus(Sa,S'))
					       | NONE => die ("Failed to lookup " ^ Bid.pp_longbid lbid ^ " in B")) 
			(B.empty,S.empty) lbids
		    in (Sa,C.empty,M,Ba)
		    end
	      | Mlb.ATBDECbdec smlfile =>
		    let val uofiles = phi(smlfile)
		    in (S.empty,C.fromList uofiles,M,B.empty)
		    end
	      | Mlb.MLBFILEbdec (mlbfile, NONE) =>  (* derived form *)
		    let val scriptpath = OS.Path.dir mlbfile
		    in ulb phi M B (Mlb.MLBFILEbdec (mlbfile, SOME scriptpath))
		    end
	      | Mlb.MLBFILEbdec (mlbfile, SOME scriptpath) => 
		    (case OS.Path.dir mlbfile of
			 "" => (case M.lookup mlbfile M of
				    SOME (SOME (B,S)) => 
					let val S' = S.extendLoc scriptpath S
					    val B' = B.extendLoc scriptpath B
					in (S',C.empty,M,B')
					end
				  | SOME NONE =>
					die ("cycle in mlb-file: problem with " ^ mlbfile)
				  | NONE => 
					let val bdec = Mlb.parse mlbfile
					    val (S,C,M,B) = ulb phi (M.insert(mlbfile,NONE,M)) B.empty bdec
					    val M = M.insert(mlbfile,SOME(B,S),M)
					    val S' = S.extendLoc scriptpath S
					    val B' = B.extendLoc scriptpath B
					in (S',C,M,B')
					end)
	               | dir => 
			     let val phi = fn sml => 
				 (map (fn uo => pathDownArrow(uo,dir))
				  (phi (pathUpArrow(sml,dir))))
(*
				 val _ = print ("currently at " ^ OS.FileSys.getDir() ^ "\n")
				 val _ = print ("going into " ^ dir ^ "\n")
*)
				 val (S,C,M,B) = doCD dir (fn() => 
				       ulb phi (M.downArrow (M,dir)) B 
				       (Mlb.MLBFILEbdec (OS.Path.file mlbfile, SOME scriptpath)))
(*				 val _ = print ("up again from " ^ dir ^ "\n") *)
			     in (S.upArrow(S,dir),C.upArrow(C,dir),
				 M.upArrow(M,dir),B.upArrow(B,dir))
			     end)
	      | Mlb.SCRIPTSbdec smlfiles =>
		    let val S =
			foldl (fn (smlfile,S) =>
			       let val uofile = 
				   case phi(smlfile) of
				       [uofile] => uofile
				     | _ => die ("not exactly one uofile associated with script " ^ smlfile)
			       in S.insert((uofile,smlfile),S)
			       end) S.empty smlfiles
		    in (S,C.empty,M,B.empty)
		    end
        (* ule:   M,B |- bexp => S,C,M',B' *)
        and ule (phi:phi) (M:M) (B:B) bexp
          : S * C * M * B =
          case bexp of
	     Mlb.BASbexp bdec => ulb phi M B bdec
	   | Mlb.LETbexp (bdec, bexp) =>
 	      let val (S1,C1,M,B1) = ulb phi M B bdec
		  val (S2,C2,M,B2) = ule phi M (B.plus(B,B1)) bexp
	      in (S2,C.plus(C1,C2),M,B.plus(B1,B2))
	      end
	   | Mlb.LONGBIDbexp lbid =>
	      (case B.lookupl lbid B of
		   SOME(B',S') => (S',C.empty,M,B')
		 | NONE => die ("lookup of longbid " ^ Bid.pp_longbid lbid ^ " in B failed"))

	fun from_bdec (phi:phi) bdec =
	    let val (S,C,M,B) = ulb phi (M.empty) (B.empty) bdec
		val ul = SEQul(CODEFILESul (C.list C),
			       SCRIPTSul (S.list S))
	    in ul
	    end

    end