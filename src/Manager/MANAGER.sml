
signature MANAGER =
  sig
    
    structure ErrorCode : ERROR_CODE

    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list

    val comp : string list -> unit   (* comp paths  compiles paths into file `run'.
				      * Log's, vcg's and linkfile are put in current directory. 
				      * May raise PARSE_ELAB_ERROR. *)
  end
