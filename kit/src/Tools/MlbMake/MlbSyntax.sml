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
           | SCRIPTSbdec of atbdec list
           | ANNbdec of string * bdec

	fun supported_annotation s =
	    case s of
		"safeLinkTimeElimination" => true
	      | _ => false

end
