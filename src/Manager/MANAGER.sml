
(*$MANAGER*)

signature MANAGER =
  sig
    val show : unit -> unit                   (* show()  shows the program state *)

    val load : string -> unit                 (* load(name)  loads consult file 
					       * `name' into program state. *)

    val save : string -> unit                 (* save(name)  saves a consult file 
					       * `name' from program state. *)

    val wipe : unit -> unit                   (* wipe()  wipes the program state 
					       * for cache information. *)
    val touch : string -> unit

    val delete : string -> unit               (* delete(name)  deletes `name' 
					       * from program state. *)

    val add : string -> unit                  (* add(name)  adds `name' to the 
					       * end of program state. *)

    val insert : string * string -> unit      (* insert(name, name0)  inserts `name' 
					       * before `name0' in program state. *)

    val build : unit -> unit                  (* build the system *)

    val comp : string -> unit                 (* comp(filename)  compiles file 
					       * (!source_dir ^ filename) *) 

    val compile : {sourcename: string,        (* compile{sourcename,targetname,linkname,logname} *)
		   targetname: string,        (* compiles file sourcename and writes     *)
		   linkname: string,          (* the result into files linkname and      *)
                   logname : string,          (* targetname. Log information is printed  *) 
                   vcgname : string} -> bool  (* on file named logname.                  *)
                                              (* The strings are absolute paths. Returns  *)
                                              (* true if the program is not empty.       *)

    val interact : unit -> unit               (* Menu-system to control flags *)
    val read_script : unit -> unit            (* Read script file *)

    val reset : unit -> unit
    val commit : unit -> unit

    val elab : string -> unit                 (*elab s = elaborate only; s is a file name
					       relative to source_dir*)
  end
