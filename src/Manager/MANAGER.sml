
(*$MANAGER*)

signature MANAGER =
  sig
    val show : unit -> unit                   (* show()  shows the program state *)

    val load : string -> unit                 (* load(name)  loads consult file 
					       * `name' into program state. *)

    val build : unit -> unit                  (* build the project *)

(*
    val comp : string -> unit                 (* comp(filename)  compiles file 
					       * (!source_dir ^ filename) *) 

    val compile : {sourcename: string,        (* compile{sourcename,targetname,linkname,logname} *)
		   targetname: string,        (* compiles file sourcename and writes     *)
		   linkname: string,          (* the result into files linkname and      *)
                   logname : string,          (* targetname. Log information is printed  *) 
                   vcgname : string} -> bool  (* on file named logname.                  *)
                                              (* The strings are absolute paths. Returns  *)
                                              (* true if the program is not empty.       *)
*)

    exception PARSE_ELAB_ERROR
    val comp : string -> unit   (* comp s  compiles (!Flags.source_directory ^ s ^ ".sml")
				 * into (!Flags.target_directory ^ s ^ ".exe") 
				 * Log's, vcg's and linkfile are put in target directory. 
				 * May raise PARSE_ELAB_ERROR. *)

    val elab : string -> unit                 (* elab s  elaborates only; s is a file name
					       * relative to source_dir *)

    val interact : unit -> unit               (* Menu-system to control flags *)
    val read_script : unit -> unit            (* Read script file *)

    val reset : unit -> unit
    val commit : unit -> unit
  end
