
signature MANAGER =
  sig

    structure ErrorCode : ERROR_CODE

    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list

    val comp : string -> unit   (* [comp path] compiles path into file `run'.
			         * Log's, vcg's and linkfile are put in current directory.
				 * May raise PARSE_ELAB_ERROR. *)

    structure PickleBases : sig
      type modcode
      type name
      type Basis0
      type Basis1
      type InfixBasis
      type ElabBasis
      type opaq_env
      type IntBasis
      type hce

      type ElabBasesInfo = {ebfile : string,
                            infixElabBasis : InfixBasis*ElabBasis,
                            used : bool ref} list

      type longtycon and longstrid and longid
      type funid and sigid

      type longids = {funids:funid list, sigids:sigid list, longstrids:longstrid list,
                      longtycons:longtycon list, longvids:longid list}

      val unpickleLnkFile : string -> modcode
      val pickleNB        : string -> string -> (name list * Basis0) * (name list * Basis1) -> unit
      val unpickleBases0  : string list -> hce * ElabBasesInfo
      val unpickleBases1  : hce -> string list -> opaq_env * IntBasis

      val compute_actual_deps : ElabBasis -> ElabBasesInfo -> longids -> string list

      val add_longstrid : longstrid -> longids -> longids

    end
  end
