(* Global flags *)

signature FLAGS =
  sig

    exception ParseScript of string

    (*Warnings*)

    type Report
    val warn : Report -> unit
    val warn_string : string -> unit
    val report_warnings : unit -> unit
    val reset_warnings : unit -> unit
      
    (*Warnings are collected during compilation and printed all at once at
     the end of the compilation of a program.  The printing is done in
     Manager which also resets the warnings.*)

    type state
    val get_state           : unit -> state
    val reset_state         : state -> unit    
    val is_on               : string -> bool 
    val turn_on             : string -> unit
    val turn_off            : string -> unit
    val lookup_flag_entry   : string -> bool ref
    val add_string_entry    : string * string ref -> unit
    val get_string_entry    : string -> string
    val lookup_string_entry : string -> string ref
    val add_flag: string * string * bool ref -> unit
      (* add_flag(key, menu_txt, boolref)  adds a new flag to the flag list. 
       * The flag may then be tested on and adjusted using the primitives
       * above. *)

    val add_flag_to_menu: string list* string * string * bool ref -> unit
    val add_string_to_menu: string list* string * string * string ref -> unit

     (* add_flag_to_menu(path,key,menu_text,flag);
        add 'flag' with menu text 'menu_text' and dynamic search key 'key' to
        the sub-menu given by 'path'. The path is a list of menu texts, e.g., 
        ["Backends","C or HP PA_RISC","Compile into HP PA-RISC"]. If the path
        is not present in the menu, the menu is not changed.
     *)
     

    val raggedRight: bool ref  (* set to true if PrettyPrinting should
				  be allowed to exceed `colwidth' by as
				  much as it likes in order not to omit
				  information
                               *)

    val chat: bool ref      (* true if a message is to be printed for each phase of compilation *)

    val DEBUG_LEXING:	    bool ref
    val DEBUG_PARSING:      bool ref
    val DEBUG_ELABDEC:	    bool ref
    val DEBUG_ELABTOPDEC:   bool ref 
    val DEBUG_ENVIRONMENTS: bool ref 
    val DEBUG_ERRORPRINT:   bool ref
    val DEBUG_EVALDEC:      bool ref
    val DEBUG_EVALTOPDEC:   bool ref
    val DEBUG_FLEXRECORDS:  bool ref
    val DEBUG_STATOBJECTS:  bool ref
    val DEBUG_TYPES:        bool ref
    val DEBUG_TYVARS:	    bool ref

   (*Compiler debug options: *)
    val DEBUG_COMPILER:		bool ref
    val DEBUG_DECISIONTREE:	bool ref
    val DEBUG_MATCHCOMPILER:	bool ref
    val DEBUG_OPTIMISER:	bool ref

    val optimiser :             bool ref

    (*Region inference debug options*)

    val print_effects : bool ref
    val print_types : bool ref
    val print_regions : bool ref
    val print_word_regions : bool ref

    (* Lambda->KAM compile debug options *)

    (* State *)
    val target_file_extension : string ref
    val log : TextIO.outstream ref
    val colwidth : int ref
    val indent_ccode : bool ref

    (*Project Manager.  These ref's are necessary because Flags.interact
     depends on, e.g., Manager.build_project but module Flags cannot depend on
     module Manager because Manager depends on Flags.  Perhaps interact
     should be in Manager instead of in Flags?*)

    val build_project_ref : (string -> unit)ref
    val comp_ref : (string -> unit)ref
    val test_ref : (unit -> unit)ref

    val auto_import_basislib : bool ref
    val basislib_project : string ref           (* absolute path to basislib.pm *)

    (* Program Points. *)
    val print_all_program_points : bool ref    (* if true then print all program points,
						* otherwise print program_points in the list below. *)
    val program_points: int list ref           (* contains the program points that 
                                                * should be included in program listing *)
    val region_paths : (int*int) list ref 

    (* Reading script. *)
    val read_script : string -> unit
    val show_script_entries : unit -> unit

    (* interactive toggling of flags: *)

    val interact: unit -> unit

  end;
