functor UlFile (MlbProject : MLB_PROJECT)
    : ULFILE where bdec = MlbProject.bdec =
    struct
	structure Mlb = MlbProject
	structure Bid = Mlb.Bid

	fun die s = 
	    let val s = "UlFile.Error: " ^ s
	    in print(s ^ "\n"); raise Fail s
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

	fun pp_uo_loc (uo,loc) : string =
	    concat[" ", uo, " As ", loc, "\n"]
	    
	fun pp_uo uo : string =
	    concat[" ", uo, "\n"]

	fun pp_ul0 ul : string list=
	    case ul of
		SEQul (ul1,ul2) => pp_ul0 ul1 @ ("\n" :: pp_ul0 ul2)
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
	type M = string * (B*S)option
	structure M =
	    struct	       
		val empty : M = nil
		fun insert (s,l,M:M):M = (s,l)::M
		fun lookup s nil = NONE
		  | lookup s ((x,l)::xs) = if s=x then SOME l
					   else lookup s xs
	    end
        structure S =
	    struct
		val empty : S = nil
		fun insert (p,S) = p::S
		fun plus (S1,S2) = foldr insert S1 S2
		fun list (a:S):(uofile*location)list = rev a
		fun extendLoc scriptpath S =
		    map (fn (uo,loc) => (uo,OS.Path.concat(scriptpath,loc))) S
	    end
	structure B =
	    struct	       
		val empty : B = B0 nil
		fun insert (bid,(Bsub,S),B0 B) = B0((bid,(Bsub,S))::B)
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
		fun plus (B1,B2) = foldr insert B1 B2
		fun extendLoc scriptpath (B0 l) =
		    B0(map (fn (bid,(Bsub,S)) => 
			    (bid,(extendLoc scriptpath Bsub,S.extendLoc scriptpath S)))
		       l)
	    end
        structure C =
	    struct
		val empty : C = nil
		fun insert (uo,C) = uo::C
		fun plus (C1,C2) = foldr insert C1 C2
		fun fromList l = rev l
		fun list a = rev a
	    end

	(* ulb:   M,B |- bdec => S,C,M',B' *)
	fun ulb (phi:phi) (M:M) (B:B) (bdec:bdec) 
	    : S * C * M * B =
	    case bdec of
		Mlb.SEQbdec (bdec1, bdec2) =>
		    let val (S1,C1,M,B1) = ulb phi M B bdec1
			val (S1,C1,M,B2) = ulb phi M (B.plus(B,B1)) bdec2
		    in (S.plus(S1,S2),C.plus(C1,C2),M,B.plus(B1,B2))
		    end
	      | Mlb.EMPTYbdec => (S.empty,C.empty,M,B.empty)
	      | Mlb.LOCALbdec (bdec1, bdec2) =>
		    let val (S1,C1,M,B1) = ulb phi M B bdec1
			val (S1,C1,M,B2) = ulb phi M (B.plus(B,B1)) bdec2
		    in (S2,C.plus(C1,C2),M,B2)
		    end
	      | Mlb.BASISbdec (bid, bexp) =>
		    let val (S,C,M,B1) = ule phi M B bexp
		    in (S.empty,C,M.empty,B.insert(bid,SOME (B1,S),B.empty))
		    end
	      | Mlb.OPENbdec lbids =>
		    let val (S,B) = foldl (fn (lbid,(S,B)) => 
					   case B.lookupl lbid B of
					       SOME (B',S') => (B.plus(B,B'),S.plus(S,S'))
					     | NONE => die ("Failed to lookup " ^ Bid.pp_longbid lbid ^ " in B")) 
			(B.empty,S.empty) lbids
		    in (S,C.empty,M,B)
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
					    val (S,C,M,B) = ulb phi (M.insert(mlbfile,SOME NONE,M)) B.empty bdec
					    val M = M.insert(mlbfile,SOME(SOME(B,S)),M)
					    val S' = S.extendLoc scriptpath S
					    val B' = B.extendLoc scriptpath B
					in (S',C,M,B')
					end)
	               | path => die "path in mlb-file not implemented")
	      | Mlb.SCRIPTSbdec smlfiles =>
		    let val S =
			foldl (fn (smlfile,S) =>
			       let val uofile = 
				   case phi(smlfile) of
				       [uofile] => uofile
				     | _ => die ("not exactly one uofile associated with script " ^ smlfile)
			       in S.insert((uofile,smlfile),S)
			       end S.empty smlfiles
		    in (S,C.empty,M,B.empty)
		    end
        (* ule:   M,B |- bexp => S,C,M',B' *)
        and ule (phi:phi) (M:M) (B:B) bexp
          : S * C * M * B =
          case bexp of
	     Mlb.BASbexp bdec => ulb phi M B bdec
	   | Mlb.LETbexp (bdec, bexp) =>
 	      let val (S1,C1,M,B1) = ulb phi M B bdec
		  val (S1,C1,M,B2) = ule phi M (B.plus(B,B1)) bexp
	      in (S2,C.plus(C1,C2),M,B.plus(B1,B2))
	      end
	   | Mlb.LONGBIDbexp lbid =>
	      (case B.lookupl lbid B of
		   SOME(B',S') => (S',C.empty,M,B')
		 | NONE => die ("lookup of longbid " ^ Bid.pp_longbid lbid ^ " in B failed"))

	fun from_bdec (phi:phi) bdec =
	    let val (S,C,M) = ulb phi (M.empty) bdec
		val ul = SEQul(CODEFILESul (C.list C),
			       SCRIPTSul (S.list S))
	    in ul
	    end

    end