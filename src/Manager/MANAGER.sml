
signature MANAGER =
  sig
    
    structure ErrorCode : ERROR_CODE

    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list
    
    val build : string -> unit                (* build the project. 
					       * May raise PARSE_ELAB_ERROR. *)

    val comp : string -> unit   (* comp s  compiles (!Flags.source_directory ^ s ^ ".sml")
				 * into (!Flags.target_directory ^ s ^ ".exe") 
				 * Log's, vcg's and linkfile are put in target directory. 
				 * May raise PARSE_ELAB_ERROR. *)

    val elab : string -> unit                 (* elab s  elaborates only; s is a file name
					       * relative to source_dir.
					       * May raise PARSE_ELAB_ERROR. *)

    val interact : unit -> unit               (* Menu-system to control flags *)
    val read_script : unit -> unit            (* Read script file *)

    val kit : unit -> unit              (* Read the script file `kit.script' in the
					 * current directory. Then run interact(). *)

    val reset : unit -> unit                  (* Resets the system *)
    val commit : unit -> unit
  end
