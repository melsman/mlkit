
signature MANAGER =
  sig
    
    structure ErrorCode : ERROR_CODE

    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list
(*    
    val build : string -> unit  (* build pmfile  builds the project pmfile. 
				 * May raise PARSE_ELAB_ERROR. *)
*)
    val comp : string -> unit   (* comp path  compiles path into file `run'.
				 * Log's, vcg's and linkfile are put in current directory. 
				 * May raise PARSE_ELAB_ERROR. *)

    val elab : string -> unit   (* elab path  elaborates path
				 * May raise PARSE_ELAB_ERROR. *)
  end
