signature MLB_PROJECT =
    sig
	eqtype bid
	val bid : string -> bid
	val pp_bid : bid -> string

	datatype bexp = SEQbexp of bexp * bexp
                      | LETbexp of bdec * bexp
                      | FILEbexp of string        (* file.sml *)
                      | BIDbexp of bid
	              | EMPTYbexp 

             and bdec = SEQbdec of bdec * bdec
                      | LOCALbdec of bdec * bdec
                      | BASbdec of bid * bexp
                      | OPENbdec of bexp
		      | USEbdec of string         (* file.mlb *)
	              | EMPTYbdec 

	val parse : string -> bdec (* Parses a basis file (i.e., an mlb-file) *)

	val dep : string -> unit   (* Write dependency information to disk in .d-files;
				    * the argument is a basis file (an mlb-file) *)

	val sources : string -> string list  

        (* [sources mlbfile] returns the list of sources (.sml- and
           .sig-files) mentioned in mlbfile. *)

    end