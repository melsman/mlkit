signature MLB_PROJECT =
    sig
	structure Bid :
	    sig eqtype bid and longbid
		val bid : string -> bid
		val longbid : bid list -> longbid
		val pp_bid : bid -> string
		val pp_longbid : longbid -> string
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

	(* scriptpath p is optional in MLBFILEbdec; only useful in the context of
	 * SMLserver as SCRIPTSbdec. *)

	val parse : string -> bdec 
	(* [parse mlbfile] parses a basis file mlbfile. Prints an 
	 * error message and raises Fail on error. *)

	val depDir : string ref  
	(* The directory in which dependency files are stored; the 
	 * default is "PM" *)
	val dep : string -> unit   
	(* [dep mlbfile] parses mlbfile (and the mlb-files it mentions, 
	 * recursively) and writes dependency information to disk in 
	 * .d-files. *)

	datatype srctype = SRCTYPE_ALL | SRCTYPE_SCRIPTSONLY | SRCTYPE_ALLBUTSCRIPTS

	val sources : srctype -> string -> (string * string) list  
        (* [sources srctype mlbfile] returns the list of sources (.sml- and
	 * .sig-files) mentioned in mlbfile, with the second components 
	 * of the pairs being the hosting mlbfiles. The srctype specifies which
	 * sources are included in the resulting list.  *)

    end