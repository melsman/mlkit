signature MLB_PROJECT =
    sig
	structure Bid :
	    sig eqtype bid and longbid
		val bid : string -> bid
		val longbid : bid list -> longbid
		val pp_bid : bid -> string
		val pp_longbid : longbid -> string
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

	val parse : string -> bdec 
	(* [parse mlbfile] parses a basis file mlbfile. Prints an 
	 * error message and raises Fail on error. *)

	val dep : string -> unit   
	(* [dep mlbfile] parses mlbfile (and the mlb-files it mentions, 
	 * recursively) and writes dependency information to disk in 
	 * .d-files. *)

	val sources : string -> string list  
        (* [sources mlbfile] returns the list of sources (.sml- and
	 * .sig-files) mentioned in mlbfile. *)

    end