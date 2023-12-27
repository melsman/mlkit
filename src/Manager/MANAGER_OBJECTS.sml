
signature MANAGER_OBJECTS =
  sig
    include MANAGER_OBJECTS0
    type modcode
    type target
    type linkinfo

    val backend_name : string (* native or kam *)

    exception PARSE_ELAB_ERROR of ParseElab.ErrorCode.ErrorCode list
    val error : string -> 'a    (* raises PARSE_ELAB_ERROR[] *)
    val warn : string -> unit   (* prints warning *)
    val quot : string -> string (* surrounds string with single quotes *)

    type filename (*= string*)
    val mk_filename         : string -> filename
    val filename_to_string  : filename -> string
    val funid_from_filename : filename -> funid
    val funid_to_filename   : funid -> filename

    structure Environment : ENVIRONMENT

    structure SystemTools :
      sig
(*
        val change_dir : string -> {cd_old:unit->unit,file:string}
	(* [change_dir p] cd's to the dir part of p and returns the
	 * file part of p together with a function for changing to the
	 * original directory. *)
*)
	val delete_file : string -> unit
	val maybe_create_dirs : {prepath:string,dirs:string} -> unit
	val maybe_create_mlbdir : {prepath:string} -> unit
      end

    structure ModCode :
      sig
	val empty : modcode
	val seq : modcode * modcode -> modcode
	val mk_modcode : target * linkinfo * string -> modcode
	(* Use emit or mk_exe to actually emit code.
	 * The string is a program unit name. *)
	val exist : modcode -> bool
	val emit : absprjid * modcode -> modcode
	val mk_exe : absprjid * modcode * string list * string -> unit
        (* produces executable `string' in target directory the string
	 * list is a list of external object files as generated by a
	 * foreign compiler (e.g., gcc). *)
	val mk_exe_all_emitted : modcode * string list * string -> unit
	val size : modcode -> int (* for debugging *)
	val target_files : modcode -> string list
        (* [target_files mc] returns the paths to the emitted target_files
	 * for mc; dies if not all files are emitted. *)
	val pu : modcode Pickle.pu
	val dirMod : string -> modcode -> modcode
	    (* [dirMod d mc] replaces paths p in mc with
	     * paths d/f where f is the file of p (for path-abstraction) *)
	val dirMod' : string -> modcode -> modcode
	    (* [dirMod' d mc] replaces paths p in mc with
	     * paths d/p *)
        val subMod : string -> modcode -> modcode
        val mk_sharedlib : modcode * string list * (*name*) string * (*sofile*) string -> unit
      end

    val mlbdir : unit -> string (* based on flags, returns a relative path to a directory
				 * in which to store object code. *)

    val mk_repl_runtime : (unit -> string) option

    val toJSString : (string -> string) option
    val export_basis_js : unit -> bool

  end
