signature MLB_PROJECT =
sig
  structure MS :
    sig
      structure Bid :
        sig
          eqtype bid and longbid
          val bid : string -> bid
          val longbid : bid list -> longbid
          val longopen : longbid -> bid * longbid option
          val pp_bid : bid -> string
          val pp_longbid : longbid -> string
        end

      type atbdec = string (* path.{sml,sig} *)

      datatype bexp = BASbexp of {bdec:bdec}
                    | LETbexp of bdec * bexp
                    | LONGBIDbexp of Bid.longbid

           and bdec = SEQbdec of bdec * bdec
	            | EMPTYbdec
                    | LOCALbdec of bdec * bdec
                    | BASISbdec of Bid.bid * bexp
                    | OPENbdec of Bid.longbid list
	            | ATBDECbdec of atbdec
		    | MLBFILEbdec of string
	            | ANNbdec of string * bdec
    end

    (* scriptpath p is optional in MLBFILEbdec; only useful in the context of
     * SMLserver as SCRIPTSbdec. *)

  val parse : string -> MS.bdec * string
  (* [parse mlbfile] parses a basis file mlbfile. Also returns a hash. Prints an
   * error message and raises Fail on error. *)

  val depDir : string ref
  (* The directory in which dependency files are stored; the
   * default is "MLB" *)

  val dep : string -> unit
  (* [dep mlbfile] parses mlbfile (and the mlb-files it mentions,
   * recursively) and writes dependency information to disk in
   * .d-files. *)

  type BG
  structure Atom :
    sig
      type atom
      val toString : atom -> string
      val fromString : string -> atom
      val compare : (atom * atom) -> order
    end
  type File
  type SmlFile = Atom.atom
  val project : File -> SmlFile
  val fold : ((SmlFile * (string*string) * string list * 'a) -> 'a) -> 'a -> BG -> 'a
  val done : File * BG -> (File * (string*string) * string list) list * BG
  val initial : BG -> (File * (string*string) * string list) list
  (* The second components of the pairs being the hosting mlbfiles (the third
   * components contains annotations, that is, flags that the compiler should
   * apply during compilation of the source file). *)

(*  val pp_bg : BG -> string *)
  val sources : string -> BG
  (* [sources mlbfile] returns the dependency graph of sources (.sml- and
   * .sig-files) mentioned in mlbfile *)
end
