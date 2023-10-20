(* Global flags *)

signature FLAGS =
  sig

    (*
     * MLKit warnings: collected during compilation and printed all at once at
     * the end of the compilation of a program.  The printing is done in
     * Manager which also resets the warnings.
     *)

    type Report
    val warn : Report -> unit
    val warn_string : string -> unit
    val report_warnings : unit -> unit
    val reset_warnings : unit -> unit

    (*
     * Testing and toggling of flags.
     *)

    val is_on               : string -> bool
    val is_on0              : string -> unit -> bool    (* to avoid lookup *)
    val turn_on             : string -> unit
    val turn_off            : string -> unit
    val lookup_flag_entry   : string -> bool ref        (* deprecated - do not
							 * use this funtion; use
							 * is_on0 instead! *)

    val get_string_entry    : string -> string
    val lookup_string_entry : string -> string ref
    val lookup_stringlist_entry : string -> string list ref
    val get_stringlist_entry: string -> string list
    val lookup_int_entry    : string -> int ref

    (* To allow the binary distribution of the Kit to be stored in
     different directories on different systems---to make the Kit
     relocatable---we must allow the object files for the basis
     library to be moved to another location after building the
     system, and still have the object files being reused. To
     this end, we pass as an argument to the Kit executable the
     directory in which the Kit is located (installed). The
     string ref Flags.install_dir is set to this directory during
     launch of the Kit. The install_dir reference is also used for
     locating the runtime system and the bytecode interpreter. *)

    val install_dir : string ref


    val raggedRight: bool ref  (* set to true if PrettyPrinting should
				  be allowed to exceed `colwidth' by as
				  much as it likes in order not to omit
				  information
                               *)

    val chat: bool ref         (* true if a message is to be printed
				  for each phase of compilation
			       *)

    val DEBUG_COMPILER:		bool ref

    val print_types : bool ref

    val log : TextIO.outstream ref
    val colwidth : int ref

    val timings_stream : TextIO.outstream option ref   (* optional stream for exporting
							  timings (`KITtimings')
						       *)

    (* Program Points. *)
    val print_all_program_points : bool ref    (* if true then print all program points,
						  otherwise print program_points in the
						  list below.
					       *)
    val program_points: int list ref           (* contains the program points that
						  should be included in program listing
					       *)
    val region_paths : (int*int) list ref

    (* Generic system to document options and let them appear on command
     * lines. *)

    type bentry = {long: string,           (* long option for use with mlkit command
					    *   using `--', script files, and internally
					    *   in the mlkit to lookup the current setting
					    *   during execution. *)
		   short: string option,   (* short option used in commands with - *)
		   menu: string list,      (* menu path; nil means no-show *)
		   item: bool ref,         (* the actual flag *)
		   neg: bool,              (* should negated flag be introduced?
					    *   -no_opt,  --no_optimiser *)
		   desc: string}           (* description string; format manually
					    *   with new-lines *)

    type 'a entry = {long: string,
		     short: string option,
		     menu: string list,
		     item: 'a ref,
		     desc: string}

    type baentry = {long: string,           (* long option for use with mlkit command
					     *   using `--', script files, and internally
					     *   in the mlkit to lookup the current setting
					     *   during execution. *)
		    short: string option,   (* short option used in commands with - *)
		    menu: string list,      (* entry::path; nil means no-show*)
		    item: bool ref,         (* the actual flag *)
		    on: unit->unit,         (* function to apply to turn entry on *)
		    off: unit->unit,        (* function to apply to turn entry off;
					     * a toggling function can be made from
					     * these two and the item. *)
		    desc: string}           (* description string; format manually
					     *   with new-lines *)


    (* Functions to add entries dynamically; remember to add a description
     * telling what the flag is used for. If a nil-menu is given, the
     * entry is not shown in help and the option cannot be given at the
     * command line. *)

    val add_bool_entry        : bentry -> (unit -> bool)
    val add_string_entry      : string entry -> (unit -> string)
    val add_stringlist_entry  : string list entry -> (unit -> string list)
    val add_int_entry         : int entry -> (unit -> int)
    val add_bool_action_entry : baentry -> (unit -> bool)

    (* Read and interpret option list by looking in directory and
     * the extra nullary list and unary list *)

    val read_options : {nullary: (string*(unit->unit))list,
			unary: (string*(string->unit))list,
			options: string list} -> string list

    (* help key  provides help information for the key *)
    val help : string -> string
    val help_nodash : string -> string

    (* help_all()  provides help on all options in the directory *)
    val help_all : unit -> string

    val help_all_nodash_noneg : unit -> string

    type options = {desc : string, long : string list, short : string list,
                    kind : string option, default : string option, menu: string list}

    val getOptions : unit -> options list
    val getOptions_noneg : unit -> options list
    val menu_width : int

    (* Blocked entries are flag entries that do not show up in help
       information and that cannot be altered by commandline. Blocking an
       entry makes it possible to specialize man-pages and --help info for
       ReML and SMLtoJs. It also guards against mis-configurations and some
       non-supported combination of flags. *)
    val block_entry : string -> unit

    structure Statistics :
      sig
        val no_dangling_pointers_changes       : int ref
        val no_dangling_pointers_changes_total : int ref
	val reset : unit -> unit
      end

    val has_sml_source_ext : string -> bool
  end
