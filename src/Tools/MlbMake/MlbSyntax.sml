structure MlbSyntax =
struct
	structure Bid :>
	  sig eqtype bid and longbid
	  end =
    struct
      type bid = string
      type longbid = bid list
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
           | ANNbdec of string * bdec

	fun supported_annotation s =
	    case s of
		"safeLinkTimeElimination" => true
	      | _ => false

  fun fold (SEQbdec(a,b))         e seq bas loc fopen fatbdec fmlb script ann = seq(b,seq(a,e))
    | fold EMPTYbdec              e seq bas loc fopen fatbdec fmlb script ann = e
    | fold (LOCALbdec(a,b))       e seq bas loc fopen fatbdec fmlb script ann = loc(b,loc(a,e))
    | fold (BASISbdec (bid,bexp)) e seq bas loc fopen fatbdec fmlb script ann = bas(bid,bexp,e)
    | fold (OPENbdec l)           e seq bas loc fopen fatbdec fmlb script ann = List.foldl fopen e l
    | fold (ATBDECbdec a)         e seq bas loc fopen fatbdec fmlb script ann = fatbdec(a,e)
    | fold (MLBFILEbdec (s,so))   e seq bas loc fopen fatbdec fmlb script ann = fmlb(s,so,e)
    | fold (ANNbdec (ann',bdec))  e seq bas loc fopen fatbdec fmlb script ann = ann(ann',bdec,e)

end
