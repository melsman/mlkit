(* Global flags *)

functor Flags (structure Crash : CRASH
	       structure Report : REPORT
	       structure PP : PRETTYPRINT
	       val raggedRight : bool ref
	       val colwidth : int ref) : FLAGS =
  struct

    structure NewList = List
    structure ListPair = Edlib.ListPair
    structure List = Edlib.List

    fun outLine (s) = print(s ^ "\n")
    fun quote s = "\"" ^ String.toString s ^ "\""
    fun die s = Crash.impossible ("Flags." ^ s)

     (* To introduce a new dynamic flag, do the following:

            Use the function add_flag_to_menu from within a module.

        OR

        (a) declare a new boolean reference, r
        (b) add r to the menu (with a menu text) at an appropriate place
        (c) add r to the `Adding initial entries' section below, together with 
	    a search key (a string), which can be used by the modules that 
            want to access the flag.
        (d) recompile this functor and the functor which uses the new flag

     *)


    val install_dir = ref "You_did_not_set_path_to_install_dir"
     
    (* Pretty Printing *)

    val raggedRight = raggedRight

    (* Debugging Flags *)
    val print_type_name_stamps  = ref false
    val DEBUG_COMPILER          = ref false
    val chat                    = ref false

    val optimiser               = ref true
    val type_check_lambda       = ref true    (* enforce checking as the default *)

    (* Elimination of polymorphic equality *)
    val eliminate_polymorphic_equality = ref true

   (* Region inference *)
    val print_effects = ref false
    val print_regions = ref true
    val print_word_regions = ref false
    val print_types  = ref false
    val warn_on_escaping_puts = ref false
    val all_multiplicities_infinite = ref false   
    val region_inference = ref true

    (* Optimiser *)
    val cross_module_opt = ref true
    val statistics_after_optimisation = ref false
    val minimize_fixs = ref true
    val contract = ref true
    val specialize_recursive_functions = ref true
    val unbox_function_arguments = ref true
    val eliminate_explicit_records = ref true
    val maximum_inline_size = ref 50            
    val maximum_specialise_size = ref 200

    (* Printing of intermediate forms *)
    val print_opt_lambda_expression = ref false
    val print_storage_mode_expression = ref false
    val print_drop_regions_expression = ref false
    val print_drop_regions_expression_with_storage_modes = ref false
    val print_physical_size_inference_expression = ref false
    val print_call_explicit_expression = ref false

    val disable_atbot_analysis = ref false
    val print_K_normal_forms = ref false
    val debug_which_at = ref false
    val enhanced_atbot_analysis = ref false

    (* Flags for region profiling. *)
    val region_profiling = ref false
    val print_region_flow_graph = ref false
    val print_all_program_points = ref false
    val program_points = (ref []): int list ref
    val region_paths = (ref[]): (int*int) list ref

    (* Flags for Lambda Backend *)

    val log_to_file = ref false
    val c_compiler = ref "gcc" (*or maybe "gcc -ansi" or "cc -Aa" *)

    val colwidth = colwidth

    val log = ref TextIO.stdOut
(*
    val indent_ccode = ref false;
*)
    (* Program manager *)

    fun dummy _ : unit = print "uninitialised function reference in Flags!"
    val comp_ref: (string -> unit)ref  = ref dummy
    val current_source_file = ref "sources.pm"
    val import_basislib = ref true
    val basislib_project = ref "You_did_not_set_basislib_project"

    val timings_stream : TextIO.outstream option ref = ref NONE

          (*************************************************)
          (*                                               *)
          (*                   warnings                    *)
          (*                                               *)
          (*************************************************)


    type Report = Report.Report

    local val warnings : Report list ref = ref [];
    in
      fun reset_warnings () = warnings := []

      fun warn report = warnings := report :: !warnings
      val warn_string = warn o Report.line

      fun report_warnings () = 
	    (case !warnings of
	       [] =>  ()
	     | reports =>
		 (if !log_to_file then
		    print ("\n*** " ^ Int.toString (NewList.length reports)
			   ^ " warning"
			   ^ (case reports of [_] => "" | _ => "s")
			   ^ " printed on log file\n")
		  else ();
		  let val reports = rev reports
		      val report = Report.//(Report.line " *** Warnings ***", Report.flatten reports)
		  in Report.print' report (!log)
		  end))
    end (*local*)


    (* -----------------------------------
     *         Parse functions 
     * ----------------------------------- *)

    type ('a, 'b) reader = ('a, 'b) StringCvt.reader

    fun scan_string getc cs =
      let fun loop (cs, acc) =
	    case getc cs
	      of SOME(#"\"", cs) => SOME(implode(rev acc),cs)
	       | SOME(c,cs) => loop(cs,c::acc)
	       | NONE => NONE
      in case getc (StringCvt.skipWS getc cs)
	   of SOME(#"\"", cs) => loop(cs, [])
	    | _ => NONE
      end

    fun getc [] = NONE
      | getc (c::cs) = SOME(c,cs)



          (*************************************************)
          (*           structure ParseScript               *)
          (*                                               *)
          (* Parsing settings (filenames and directories)  *)
          (* from a script file                            *)
          (*************************************************)

structure ParseScript: sig 
			 datatype const = INT of int | STRING of string | BOOL of bool
			 exception ParseScript of string
			 val parseScript : string -> (string * const) list 
		       end =

  (* syntax: 

     DEC::= val ID : TYPE  = CONST REST 
     REST::= ;
         |  DEC
     ID  ::= sequence_of_letters_underbars_and_primes
     TYPE::= int | string | bool
     CONST::= ml_integer_constant | ml_string_constant | ml_bool_constant
  
     blanks, tabs and newlines are separators;
     comments are enclosed in (* and *) and can be nested.
  *)

struct
  datatype ty = Int | String | Bool
  datatype const = INT of int | STRING of string | BOOL of bool
  type parse_result = (string * const) list
  type state  = string list * parse_result 
  type parser = state -> state
  exception ParseScript of string

  fun scan_token getc cs = 
    let val cs = StringCvt.skipWS getc cs
        fun loop (cs, acc) =
          case getc cs
	    of SOME (c,cs') => if Char.isAlphaNum c orelse c= #"_" orelse c= #"'" then loop(cs',c::acc)
			       else SOME(implode (rev acc), cs)  
	     | NONE => if acc=nil then NONE else SOME(implode(rev acc),cs)
    in loop (cs, [])
    end

  fun scan_id getc cs = 
    case scan_token getc cs
      of SOME res => res
       | NONE => raise ParseScript "I expect an identifier"

  fun scan_ty getc cs =
    let val cs = StringCvt.skipWS getc cs
    in case scan_token getc cs
	 of SOME("int",cs) => (Int,cs)
	  | SOME("string",cs) => (String,cs)
	  | SOME("bool",cs) => (Bool,cs)
	  | _ => raise ParseScript "I expect one of `int', `string', or `bool'"
    end

  fun scan_colon getc cs =
    let val cs = StringCvt.skipWS getc cs
    in case getc cs
	 of SOME(#":", cs) => cs
	  | _ => raise ParseScript "I expect `:'"
    end

  fun scan_eq getc cs =
    let val cs = StringCvt.skipWS getc cs
    in case getc cs
	 of SOME(#"=", cs) => cs
	  | _ => raise ParseScript "I expect `='"
    end

  fun scan_val getc cs =
    case scan_token getc cs
      of SOME("val",cs) => SOME cs
       | _ => NONE

  fun scan_dec getc cs =
    case scan_val getc cs
      of SOME cs =>
	let val (id, cs) = scan_id getc cs
	    val cs = scan_colon getc cs
	    val (ty, cs) = scan_ty getc cs
	    val cs = scan_eq getc cs
	    val (const, cs) =
	      case ty
		of Int => (case Int.scan StringCvt.DEC getc cs
			     of SOME(i,cs) => (INT i,cs)
			      | NONE => raise ParseScript "I expect an integer constant")
		 | String => (case scan_string getc cs
				of SOME(s,cs) => (STRING s,cs)
				 | NONE => raise ParseScript "I expect a string constant")
		 | Bool => (case Bool.scan getc cs
			      of SOME(b,cs) => (BOOL b,cs)
			       | NONE => raise ParseScript "I expect a bool constant")
	in SOME((id,const),cs)
	end
       | NONE => NONE

  fun scan_decs getc cs =
    case scan_dec getc cs
      of SOME(dec,cs) =>
	let val decs = scan_decs getc cs
	in dec::decs
	end
       | NONE => 
	let val cs = StringCvt.skipWS getc cs
	in case getc cs
	     of SOME (#";", cs) => scan_decs getc cs
	      | SOME _ => raise ParseScript "I expect `;' or `val' or `end of file'"
	      | NONE => []
	end
  
  fun drop_comments (l: char list) : char list =
    let fun loop(n, #"(" :: #"*" :: rest ) = loop(n+1, rest)
          | loop(n, #"*" :: #")" :: rest ) = loop(n-1, if n=1 then #" "::rest else rest)
          | loop(0, ch ::rest) = ch :: loop (0,rest)	  
          | loop(0, []) = []
          | loop(n, ch ::rest) = loop(n,rest)
          | loop(n, []) = raise ParseScript "unclosed comment"
    in
        loop(0,l)
    end;
  
  fun fromFile filename =
    let val is = TextIO.openIn filename 
        val s = TextIO.inputAll is handle E => (TextIO.closeIn is; raise E)
    in TextIO.closeIn is; s
    end

  fun parseScript(filename: string) = 
    ((scan_decs getc (drop_comments(explode(fromFile filename))))
     handle IO.Io {name,function,...} => raise ParseScript (name ^":"^function))
    handle ParseScript s => (TextIO.output(!log, "\n *parse error*: " ^ s ^ "\n");
			     raise ParseScript s)

end (* ParseScript *)


           
          (**************************************************)
          (*           structure Directory                  *)
          (*                                                *)
          (* Directory, holding associations for            *)
	  (* various settings and operations for toggling,  *)
	  (* etc. Also, readScript is here.                 *)
          (**************************************************)

    type bentry = {long: string,           (* long option for use with mlkit command 
					    *   using `--', script files, and internally
					    *   in the mlkit to lookup the current setting 
					    *   during execution. *)
		   short: string option,   (* short option used in commands with - *)
		   menu: string list,      (* entry::path; nil means no-show*)
		   item: bool ref,         (* the actual flag *)
		   neg: bool,              (* should negated flags be introduced? 
					    *   -no_opt,  --no_optimiser *)
		   desc: string}           (* description string; format manually 
					    *   with new-lines *)

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

    type 'a entry = {long: string,
		     short: string option,
		     menu: string list,
		     item: 'a ref,
		     desc: string}


structure Directory : sig
			val bool_entry          : bentry -> unit
			val string_entry        : string entry -> unit
			val int_entry           : int entry -> unit
			val bool_action_entry   : baentry -> unit

			val is_on               : string -> bool 
			val is_on0              : string -> unit -> bool 
			val turn_on             : string -> unit
			val turn_off            : string -> unit
			val add_string_entry    : string * string ref -> unit
			val add_int_entry       : string * int ref -> unit
			val add_bool_entry      : string * bool ref -> unit
			val get_string_entry    : string -> string
			val lookup_string_entry : string -> string ref
			val lookup_flag_entry   : string -> bool ref
			val lookup_int_entry    : string -> int ref
			val readScript          : string -> unit
			val show_script_entries : unit -> unit

			(* read and interpret option list by looking in directory and
			 * the extra nullary list and unary list *)
			val read_options : {nullary:(string*(unit->unit))list,
					    unary:(string*(string->unit))list,
					    options: string list} -> string list

			(* help key  provides help information for the key *)
			val help : string -> string

			(* help_all()  provides help on all options in the directory *)
			val help_all : unit -> string

		      end =
struct

    datatype entry0 = INT_ENTRY of int entry 
                    | BOOL_ENTRY of bentry
                    | BOOLA_ENTRY of baentry        (* action entry *)
                    | STRING_ENTRY of string entry

    structure M = OrderFinMap (structure Order = struct type T = string
							fun lt a (b:string) = a < b
						 end
			       structure PP = PP
			       structure Report = Report)

    val dir : entry0 M.map ref = ref M.empty

    fun bool_entry (e:bentry) : unit =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("bool_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => dir := M.add(#long e, BOOL_ENTRY e,
				case #short e
				  of SOME s => M.add(s, BOOL_ENTRY e, !dir)
				   | NONE => !dir)

    fun bool_action_entry (e:baentry) : unit =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("bool_action_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => dir := M.add(#long e, BOOLA_ENTRY e,
				case #short e
				  of SOME s => M.add(s, BOOLA_ENTRY e, !dir)
				   | NONE => !dir)

    fun string_entry (e:string entry) : unit =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("string_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => dir := M.add(#long e, STRING_ENTRY e,
				case #short e
				  of SOME s => M.add(s, STRING_ENTRY e, !dir)
				   | NONE => !dir)

    fun int_entry (e:int entry) : unit =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("int_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => dir := M.add(#long e, INT_ENTRY e, 
				case #short e
				  of SOME s => M.add(s, INT_ENTRY e, !dir)
				   | NONE => !dir)

    fun lookup_flag_entry (key) : bool ref =
      case M.lookup (!dir) key
	of SOME (BOOL_ENTRY e) => #item e
	 | SOME _ => die ("lookup_flag_entry: entry " ^ key ^ " is of wrong kind")
	 | NONE => die ("lookup_flag_entry: no entry " ^ key ^ " in directory")

    fun lookup_string_entry (key) : string ref =
      case M.lookup (!dir) key
	of SOME (STRING_ENTRY e) => #item e
	 | SOME _ => die ("lookup_string_entry: entry " ^ key ^ " is of wrong kind")
	 | NONE => die ("lookup_string_entry: no entry " ^ key ^ " in directory")

    fun lookup_int_entry (key) : int ref =
      case M.lookup (!dir) key
	of SOME (INT_ENTRY e) => #item e
	 | SOME _ => die ("lookup_int_entry: entry " ^ key ^ " is of wrong kind")
	 | NONE => die ("lookup_int_entry: no entry " ^ key ^ " in directory")

  val get_string_entry = ! o lookup_string_entry

  fun is_on0 (key: string) : unit -> bool = 
    case M.lookup (!dir) key
      of SOME (BOOL_ENTRY {item,...}) => (fn () => !item)
       | SOME (BOOLA_ENTRY {item,...}) => (fn () => !item)
       | SOME _ => die ("is_on0: entry " ^ key ^ " is of wrong kind")
       | NONE => die ("is_on0: no entry " ^ key ^ " in directory")

  fun is_on k = is_on0 k ()

  fun turn_on (key: string) : unit = 
    case M.lookup (!dir) key
      of SOME (BOOL_ENTRY e) => #item e := true
       | SOME (BOOLA_ENTRY e) => #on e ()
       | SOME _ => die ("turn_on: entry " ^ key ^ " is of wrong kind")
       | NONE => die ("turn_on: no entry " ^ key ^ " in directory")

  fun turn_off (key: string) : unit = 
    case M.lookup (!dir) key
      of SOME (BOOL_ENTRY e) => #item e := false
       | SOME (BOOLA_ENTRY e) => #off e ()
       | SOME _ => die ("turn_off: entry " ^ key ^ " is of wrong kind")
       | NONE => die ("turn_off: no entry " ^ key ^ " in directory")

  fun add_string_entry (long, item) = 
    string_entry {long=long, short=NONE, desc="", item=item, menu=nil} 

  fun add_int_entry (long, item) = 
    int_entry {long=long, short=NONE, desc="", item=item, menu=nil} 

  fun add_bool_entry (long, item) = 
    bool_entry {long=long, short=NONE, desc="", item=item, menu=nil, neg=false} 

  (* Read and interpret script and update directory according to 
   * parse result. *)
  local
    fun update_string (key, v) = lookup_string_entry key := v
    fun update_int (key, v) = lookup_int_entry key := v
    fun update_bool (key, v) = lookup_flag_entry key := v

    fun interpret (l:(string*ParseScript.const) list) : unit = 
      NewList.app (fn (s, ParseScript.STRING newval) => update_string(s,newval)
                    | (s, ParseScript.INT newval) => update_int(s,newval)
                    | (s, ParseScript.BOOL newval) => update_bool(s,newval)) l;
  in
    fun readScript script_file : unit = 
      if OS.FileSys.access (script_file, []) then
	    (print ("Reading script file " ^ quote script_file ^ "\n");
	     interpret (ParseScript.parseScript script_file) 
	     handle _ => print ("Error while reading script file\n"))
      else print ("No script file " ^ quote script_file ^ " present\n")
  end

  (* Write all possible entries which can be changed from *)
  (* the script file.                                     *)
  fun show_script_entries () =
    let
      fun value_s (BOOL_ENTRY {item=ref b,...}) = Bool.toString b
	| value_s (BOOLA_ENTRY {item=ref b,...}) = Bool.toString b
	| value_s (STRING_ENTRY {item=ref s,...}) = s
	| value_s (INT_ENTRY {item=ref i,...}) = Int.toString i

      val (dirEntriesName, dirEntriesValue) = 
	M.Fold (fn ((s,v),(l1,l2)) => (s::l1, value_s v::l2)) (nil,nil) (!dir)

      fun calc_width w [] = w
	| calc_width w (s::xs) = 
	    if w < (String.size s) then
	      calc_width (String.size s) xs
	    else
	      calc_width w xs

      val column_width = calc_width 0 (dirEntriesName @ dirEntriesValue)

      fun make_field s = StringCvt.padRight #" " column_width s
      
      val horizontal_column_line = StringCvt.padRight #"-" column_width ""
      fun horizontal_line 0 res = res ^ "+"
	| horizontal_line n res = horizontal_line (n-1) (res ^ "+" ^ horizontal_column_line)
      
      fun make_row [] s = s ^ "|"
	| make_row (x::xs) s = make_row xs (s ^ "|" ^ (make_field x))
      
    in
      (outLine (horizontal_line 2 "");
       outLine (make_row ["Name to use in script file", "Value of variable now"] "");
       outLine (horizontal_line 2 "");
       map (fn  (name, value) => outLine (make_row [name, value] ""))
       (ListPair.zip (dirEntriesName, dirEntriesValue))
           handle ListPair.Zip => die "zip" ;
       outLine (horizontal_line 2 ""))
    end

  fun lookup_notnull_menu dir key =
    let fun ok (BOOL_ENTRY{menu=nil,...}) = false
	  | ok (BOOLA_ENTRY{menu=nil,...}) = false
	  | ok (STRING_ENTRY{menu=nil,...}) = false
	  | ok (INT_ENTRY{menu=nil,...}) = false
	  | ok _ = true
    in
      case M.lookup dir key
	of r as SOME e => if ok e then r
			  else NONE
	 | NONE => NONE
    end

  (* read and interpret option list by looking in directory and
   * the extra nullary list and unary list *)

  fun opt s = case explode s
		of #"-":: #"-"::rest => SOME (implode rest)
		 | #"-"::rest => SOME (implode rest)
		 | _ => NONE

  fun negation s = case explode s
		     of #"n":: #"o":: #"_"::rest => SOME (implode rest)
		      | _ => NONE


  fun lookup_key key (l:(string *('a->unit))list) : ('a -> unit) option =
    let fun look nil = NONE
	  | look ((x,f)::rest) = if key=x then SOME f else look rest
    in look l
    end

  fun read_options  {nullary:(string*(unit->unit))list,
		     unary:(string*(string->unit))list,
		     options: string list} : string list =
    let	
      fun loop nil = nil
	| loop (all as s::ss) =
	case opt s
	  of SOME key => 
	    (case negation key
	       of SOME no_key =>
		 (case lookup_notnull_menu (!dir) no_key
		    of SOME (BOOL_ENTRY e) => 
		      if #neg e then (#item e := false; loop ss)
		      else raise Fail ("negation not allowed on option: " ^ no_key)
		     | SOME (BOOLA_ENTRY e) => (#off e (); loop ss)
		     | SOME _ => raise Fail ("negation not allowed on option: " ^ no_key)
		     | NONE => raise Fail ("unknown option: " ^ s))
		| NONE => 
		 (case lookup_notnull_menu (!dir) key
		    of SOME (BOOL_ENTRY e) => (#item e := true; loop ss)
		     | SOME (BOOLA_ENTRY e) => (#on e (); loop ss)
		     | SOME (STRING_ENTRY e) => 
		      (case ss
			 of s::ss => (#item e := s; loop ss)
			  | _ => raise Fail ("missing argument to " ^ s))
		     | SOME (INT_ENTRY e) => 
			 (case ss
			    of s::ss => 
			      (case Int.fromString s
				 of SOME i => (#item e := i; loop ss)
				  | NONE => raise Fail ("expecting integer argument to " ^ s))
			     | _ => raise Fail ("missing argument to " ^ s))
		     | NONE => 
			    let 
			      fun try_nullary exn =
				case lookup_key key nullary
				  of SOME f => (f(); loop ss)
				   | NONE => raise exn
			    in case lookup_key key unary
				 of SOME f => 
				   (case ss
				      of s::ss => (f s; loop ss)
				       | nil => try_nullary (Fail("missing argument to " ^ s)))
				  | NONE => try_nullary (Fail("unknown option: " ^ s))
			    end))
	   | NONE => all
    in loop options
    end

  (* help key  provides help information for the key *)
  fun help (key: string) :string =
    let fun opt (SOME s) = ", -" ^ s
	  | opt NONE = ""
	fun opt' (s0,SOME s) = ", -" ^ s0 ^ s
	  | opt' (s0,NONE) = ""
	fun opti (SOME s) = " N, -" ^ s ^ " N"
	  | opti NONE = " N"
	fun opts (SOME s) = " S, -" ^ s ^ " S"
	  | opts NONE = " S"
	fun bitem true = "on"
	  | bitem false = "off"
	fun indent s = 
	  map (fn s => "     " ^ s ^ "\n") (String.tokens (fn c => c = #"\n") s)
	val width = 60
	fun default(s, d) = StringCvt.padRight #" " (width - (String.size d)) s ^ " (" ^ d ^ ")\n"
	fun negation (e:bentry) =
	  if not(#neg e) then nil
	  else ["\n--no_", #long e, opt'("no_", #short e),"\n"] @
	    indent ("Opposite of --" ^ #long e ^ opt(#short e) ^ ".")
	fun negation' (e:baentry) =
	  ["\n--no_", #long e, opt'("no_", #short e),"\n"] @
	  indent ("Opposite of --" ^ #long e ^ opt(#short e) ^ ".")
    in
      String.concat
      (case lookup_notnull_menu (!dir) key 
	 of SOME (BOOL_ENTRY e) =>
	   default("--" ^ #long e ^ opt(#short e), bitem (!(#item e))) ::
	   indent (#desc e) @ negation e
	  | SOME (BOOLA_ENTRY e) => 
	   default("--" ^ #long e ^ opt(#short e), bitem (!(#item e))) ::
	   indent (#desc e) @ negation' e
	  | SOME (STRING_ENTRY e) => 
	   default("--" ^ #long e ^ opts(#short e), "\"" ^ String.toString(!(#item e)) ^ "\"") ::
	   indent (#desc e)
	  | SOME (INT_ENTRY e) =>
	   default("--" ^ #long e ^ opti(#short e), Int.toString (!(#item e))) ::
	   indent (#desc e)
	  | NONE => raise Fail ("no help available for option: " ^ key))
    end
	  
	
  (* help_all()  provides help on all options in the directory *)
  fun help_all () : string =
    let val dom = rev(M.dom (!dir))
      fun add (key, acc) =
	let                                (* add only if menu is non-empty and
					    * the entry is not a short key *)
	  fun check (SOME k) = if k = key then acc
			       else help key :: "\n" :: acc
	    | check NONE = help key :: "\n" :: acc
	in
	  case lookup_notnull_menu (!dir) key
	    of SOME (INT_ENTRY e) => check(#short e)
	     | SOME (BOOL_ENTRY e) => check(#short e)
	     | SOME (BOOLA_ENTRY e) => check(#short e)
	     | SOME (STRING_ENTRY e) => check(#short e)
	     | NONE => acc
	end
    in String.concat (foldl add nil dom)
    end

end (* Directory *)

structure Menu = Menu(structure Crash = Crash
		      val help_topic = Directory.help)


fun add_bool_entry e = 
  case #menu e
    of nil => Directory.bool_entry e
     | path => (Menu.add_flag_to_menu(#long e, path, #item e); 
		Directory.bool_entry e)

fun add_string_entry e = 
  case #menu e
    of nil => Directory.string_entry e 
     | path => (Menu.add_string_to_menu(#long e, path, #item e);
		Directory.string_entry e)

fun add_int_entry e = 
  case #menu e
    of nil => Directory.int_entry e 
     | path => (Menu.add_int_to_menu(#long e, path, #item e);
		Directory.int_entry e)

fun add_bool_entry0 (l,i) =
  add_bool_entry {long=l,short=NONE,neg=false,menu=nil,item=i,desc=""}

fun add_string_entry0 (l,i) =
  add_string_entry {long=l,short=NONE,menu=nil,item=i,desc=""}

fun add_bool_action_entry e =
  let fun toggle true = (#off e)()
	| toggle false = (#on e)()
  in
    case #menu e
      of nil => Directory.bool_action_entry e
       | path => (Menu.add_bool_action_to_menu(#long e, path, toggle, fn() => !(#item e));
		  Directory.bool_action_entry e)
  end

                     (*********************************)
                     (*  Construction of the Kit Menu *)
                     (*********************************)

  (*1. Printing of intermediate forms*)

local
  fun add (l, sh, s, r, desc) = add_bool_entry {long=l, short=sh, menu=["Printing of intermediate forms",s],
						item=r, neg=false, desc=desc}
in
  val _ = app add
  [
   ("print_opt_lambda_expression", SOME "Pole", "print optimised lambda expression", print_opt_lambda_expression,
    "Print Lambda Expression after optimisation."),
   ("print_storage_mode_expression", SOME "Psme", "print storage mode expression", print_storage_mode_expression,
    "Print Region Expression after storage mode analysis"),
   ("print_drop_regions_expression", SOME "Pdre", "print drop regions expression", print_drop_regions_expression,
    "Print Region Expression after dropping word regions and\n\
     \regions arguments with only get-effects."),
   ("print_drop_regions_expression_with_storage_modes", SOME "Pdresm", "print drop regions expression with storage modes", 
    print_drop_regions_expression_with_storage_modes,
    "Print Region Expression after dropping word regions and\n\
     \regions arguments with only get-effects. Also print\n\
     \atbot and attop annotations resulting from storage mode\n\
     \analysis."),
   ("print_physical_size_inference_expression", SOME "Ppse", "print physical size inference expression", 
    print_physical_size_inference_expression,
    "Print Region Expression after physical size inference."),
   ("print_call_explicit_expression", SOME "Pcee", "print call-explicit expression", print_call_explicit_expression,
    "Print Region Expression with call annotations."),
   ("print_normalized_program", NONE, "print normalized expression", ref false,
    "Print Region Expression after K-normalisation."),
   ("print_clos_conv_program", SOME "Pccp", "print closure converted expression", ref false,
    "Print Region Expression after closure conversion."),
   ("print_lift_conv_program", SOME "Plcp", "print lifted expression for the KAM", ref false,
    "Print Region Expression after lifting. Used for the\n\
     \compilation into byte code (KAM).")
    ]
end

  (*2. Layout*)

local
  fun add neg (l, sh, s, r, desc) = add_bool_entry {long=l, short=sh, menu=["Layout",s],
						    item=r, neg=neg, desc=desc}
in
  val _ = app (add false)
    [
     ("print_types", SOME "Ptypes", "print types", print_types,
      "Print types when printing intermediate forms. For Lambda\n\
       \Expressions, ordinary ML types are printed, whereas for\n\
       \Region Expressions, region types are printed."),
      ("print_effects", SOME "Peffects", "print effects", print_effects,
       "Print effects in region types.")
      ]
  val _ = add true
      ("print_regions", SOME "Pregions", "print regions ", print_regions,
       "Print region variables in types and expressions.")
  val _ = app (add false)
       [
       ("print_K_normal_forms", NONE, "print K-Normal Forms", print_K_normal_forms,
	"Print Region Expressions in K-Normal Form. Applicable,\n\
	 \only after storage mode analysis has been applied."),
       ("print_type_name_stamps", SOME "Ptypestamps", "print type name stamps and attributes", print_type_name_stamps,
	"Print type name stamps and their attributes in types\n\
	 \and expressions."),
       ("print_word_regions", SOME "Pwordregions", "print word regions ", print_word_regions,
	"Also print word regions that have been dropped.")
	]
  val _ = add true
	("raggedRight", NONE, "ragged right margin in pretty-printing", raggedRight,
	"Use ragged right margin in pretty-printing of\n\
	 \expressions and types.")
end

val _ = add_int_entry {long="width",short=SOME "w", menu=["Layout", "text width in pretty-printing"], 
		       item=colwidth,
		       desc="Column width used when pretty printing intermediate code."}

  (*3. Control*)

val _ = add_bool_entry {long="optimiser",short=SOME "opt", menu=["Control", "Optimiser", "optimiser"],
			item=optimiser,neg=true, 
			desc=
			"Enable optimisation of intermediate language code\n\
			\(Lambda Expressions). Which optimisations are performed\n\
			\is controlled by individual flags. The optimisations\n\
			\include function in-lining, function specialisation,\n\
			\fix-minimization, unboxing of function arguments, and\n\
			\elimination of unnecessary record constructions."}

val _ = add_bool_entry {long="cross_module_opt",short=SOME "cross_opt", 
			menu=["Control", "Optimiser", "cross module optimisation"],
			item=cross_module_opt,neg=true, 
			desc=
			"Enable cross-module optimisation including in-lining\n\
			\of small functions and specialisation of small\n\
                        \recursive functions. Which optimisations are performed\n\
                        \across modules is controlled by individual optimisation\n\
			\flags."}

val recompile_basislib = ref false
val _ = add_bool_entry {long="recompile_basislib",short=SOME "scratch", 
			menu=["Control", "recompile basis library"],
			item=recompile_basislib,neg=false, 
			desc=
			"Recompile basis library from scratch. This option\n\
			 \is useful together with other options that control\n\
			 \code generation."}

local
  fun add neg (l, s, r, desc) = add_bool_entry {long=l, short=NONE, menu=["Control", "Optimiser", s],
						item=r, neg=neg, desc=desc}
in
  val _ = app (add false)
  [
   ("statistics_after_optimisation", "statistics after optimisation", statistics_after_optimisation,
    "Report optimisation statistics after optimisation of\n\
     \Lambda Expression.")
    ]
  val _ = app (add true)
    [
    ("minimize_fixs", "minimize fixs", minimize_fixs,
     "Minimize fix constructs (Lambda Expression Optimiser)."), 	
     ("contract", "contract", contract,
      "Contract is responsible for in-lining, specialization,\n\
       \elimination of dead code, and much else (Lambda\n\
       \Expression Optimiser)."),
      ("specialize_recursive_functions", "specialize recursive functions", specialize_recursive_functions,
       "Specialise recursive functions. Use the option\n\
	\maximum_specialise_size to control which functions\n\
	\are specialised. If this flag is on, functions that are\n\
	\applied only once are specialised, no matter the setting\n\
	\of maximum_specialise_size (Lambda Expression Optimiser)."), 
       ("unbox_function_arguments", "unbox function arguments", unbox_function_arguments,
	"Unbox arguments to fix-bound functions, for which the\n\
	 \argument `a' is used only in contexts `#i a'. All call \n\
	 \sites are transformed to match the new function (Lambda\n\
	 \Expression Optimiser)."), 
       ("eliminate_explicit_records", "eliminate explicit records", eliminate_explicit_records,
	"Eliminate bindings of explicit records only used for\n\
	 \selections. Transform\n\
	 \      let r = (e1,...,en) in ... #i r .. #j r ...\n\
	 \into\n\
	 \      let x1=e1 in ... let xn=en in ... xi .. xj ...\n\
	 \(Lambda Expression Optimiser).")
   ]
end

local 
  fun add (l, s, r, d) = 
	add_int_entry {long=l,short=NONE, menu=["Control", "Optimiser", s], item=r, desc=d}
in
  val _ = add("maximum_inline_size", "maximum inline size", maximum_inline_size,
	      "Functions smaller than this size (counted in abstract\n\
	       \syntax tree nodes) are in-lines, even if they are used\n\
	       \more than once. Functions that are used only once are\n\
	       \always in-lined.")

  val _ = add("maximum_specialise_size", "maximum specialise size", maximum_specialise_size,
	      "Curried functions smaller than this size (counted in\n\
	      \abstract syntax tree nodes) are specialised if all\n\
	      \applications of the function within its own body are\n\
	      \applied to its formal argument, even if they are used\n\
	      \more than once. Functions that are used only once are\n\
	      \specialised no matter their size. See also the option\n\
	      \--specialize_recursive_functions.")
end

val preserve_tail_calls = ref false
val _ = add_bool_entry {long="preserve_tail_calls", short=SOME"ptc", item=preserve_tail_calls,
			menu=["Control", "preserve tail calls"], neg=true,
			desc=
			"Avoid the wrapping of letregion constructs around\n\
			 \tail calls. Turning on garbage collection\n\
			 \automatically turns on this option."}

val dangling_pointers = ref true
val _ = add_bool_entry {long="dangling_pointers", short=SOME"dangle", item=dangling_pointers,
			menu=["Control", "dangling pointers"], neg=true,
			desc=
			"When this option is disabled, dangling pointers\n\
			\are avoided by forcing values captured in\n\
			\closures to live at-least as long as the closure\n\
			\itself. So as to make garbage collection sound,\n\
			\this option is disabled by default when garbage\n\
			\collection is enabled."}

val tag_values = ref false
val _ = add_bool_entry {long="tag_values", short=SOME"tag", item=tag_values,
			menu=["Control", "tag values"], neg=false,
			desc=
			"Enable tagging of values as used when garbage\n\
			\collection is enabled for implementing pointer\n\
			\traversal."}

val _ = add_bool_entry {long="tag_pairs", short=NONE, item=ref false,
			menu=["Control", "tag pairs"], neg=false,
			desc=
			"Use a tagged representation of pairs for garbage\n\			
			 \collection. Garbage collection works fine with a\n\
			 \tag-free representation of pairs, so this option\n\
			 \is here for measurement purposes."}

local
  val gc = ref false
  fun off() = (gc := false; 
	       preserve_tail_calls := false;
	       dangling_pointers := true;
	       tag_values := false) 
  fun on() = (gc := true; 
	      preserve_tail_calls := true;	      
	      dangling_pointers := false;
	      tag_values := true) 
in
  val _ = add_bool_action_entry
    {long="garbage_collection", menu=["Control", "garbage collection"], 
     item=gc, on=on, off=off, short=SOME "gc",
     desc="Enable garbage collection. When enabled, regions are\n\
      \garbage collected during execution of the program. When\n\
      \garbage collection is enabled, all values are tagged. Due\n\
      \to region inference, for most programs, the garbage\n\
      \collector is invoked less often than for systems based\n\
      \only on garbage collection. When garbage collection is\n\
      \enabled, introduction of dangling pointers are avoided by\n\
      \forcing values captured in closures to live at-least as\n\
      \long as the closure. Moreover, enabling garbage\n\
      \collection implicitly enables the preservation of tail\n\
      \calls (see the option ``preserve_tail_calls''.)"}
end

local
  fun add neg (l, sh, s, r, desc) = add_bool_entry {long=l, short=sh, menu=["Control",s],
						    item=r, neg=neg, desc=desc}
in
  val _ = app (add false)
  [
   ("all_multiplicities_infinite", NONE, "all multiplicities infinite (for POPL 96)", 
    all_multiplicities_infinite,
    "Use only infinite regions. That is, store all values in\n\
     \infinite regions, which do not reside on the stack, but\n\
     \in the heap. With this flag disabled, all regions that\n\
     \can be inferred that values are allocated in them at\n\
     \most once are allocated on the stack."),
   ("disable_atbot_analysis", NONE, "all storage modes attop (for POPL 96)", disable_atbot_analysis,
    "Disable storage mode analysis. That is, turn all\n\
     \allocation directives into attop."),
   ("warn_on_escaping_puts", NONE, "warn on escaping put effects", warn_on_escaping_puts,
    "Enable the compiler to issue a warning whenever a \n\
     \region type scheme contains a put effect on a region\n\
     \that is not quantified."),
    ("report_file_sig", SOME "sig", "report signatures", ref false,
     "Report signatures for each file read."),
    ("quotation", SOME "quot", "quotation support", ref false,
     "Enable support for quotations and anti-quotations.\n\
      \When enabled, the datatype\n\
      \   datatype 'a frag = QUOTE of string\n\
      \                    | ANTIQUOTE 'a\n\
      \is available in the initial environment. Moreover,\n\
      \values of this datatype may be constructed using\n\
      \the quotation/antiquotation syntax:\n\
      \   val s = \"world\" \n\
      \   val a : string frag list = `hello ^s - goodbye`")
    ]
  val _ = add true
    ("import_basislib", SOME "basislib", "import Basis Library", import_basislib,
     "Import Basis Library automatically in your projects. If \n\
      \you wish to make use of the Standard ML Basis Library\n\
      \in your projects, this option should be turned on, unless\n\
      \you wish to import the Basis Library manually in your\n\
      \projects.")
  val _ = add true
     ("region_inference", SOME "ri", "region_inference", region_inference,
      "With this flag disabled, all values are allocated in\n\
       \global regions.")
end

val _ = app (fn (s, f) => Menu.add_action_to_menu ("", ["Control", s], f))
  [
   ("print entire menu", Menu.show_full_menu),
   ("print all flags and variables", Directory.show_script_entries)
   ]

  (*4. File menu*)

val _ = add_bool_entry 
     {long="log_to_file", short=NONE, menu=["File", "Log to file"], 
      neg=false, item=log_to_file, desc="Log to files instead of stdout."}
val _ = add_string_entry 
     {long="install_dir", short=NONE, menu=["File", "installation directory"], 
      item=install_dir,
      desc="Installation directory for the ML Kit. For normal\n\
       \execution you should not modify this value. However,\n\
       \if you wish to use the ML Kit with an altered runtime\n\
       \system and you do not wish to exchange the .o-files in\n\
       \the bin-subdirectory (for example because you are running\n\
       \the ML Kit on a shared system), you can update this\n\
       \setting and the system will try to link to a runtime\n\
       \system in the bin-subdirectory found in the new install\n\
       \directory."}

local 
  val script = ref "kit.script"
  fun read_script () = Directory.readScript (!script)
  fun value () = "(" ^ quote (!script) ^ ")"
in
  val _ = Menu.add_action_read_to_menu ("", ["File", "Read a script file"], 
					read_script, script)
  val _ = Menu.add_action_with_value_to_menu ("", ["File", "Read it again"], read_script, value)
end

  (*5. Profiling menu*)

local
  fun add neg (l, sh, s, r, desc) = add_bool_entry {long=l, short=sh, menu=["Profiling",s],
						    item=r, neg=neg, desc=desc}
in
  val _ = app (add false)
  [
   ("region_profiling", SOME "prof", "region profiling", region_profiling,
    "Enable region profiling. Object code stemming\n\
     \from compiling a program with region profiling enabled\n\
     \is instrumented with profiling information. When a program\n\
     \compiled with region profiling enabled is run, the program\n\
     \produces a profile file run.rp, which can then be read\n\
     \by the profiling tool rp2ps that comes with the ML Kit to\n\
     \produce profiling graphs of various forms."),
    ("print_region_flow_graph", SOME "Prfg", "print region flow graph", print_region_flow_graph,
     "Print a region flow graph for the program fragment\n\
     \and generate a .vcg-file, which can be viewed using\n\
     \the xvcg program."),
     ("print_all_program_points", SOME "Ppp", "print all program points", print_all_program_points,
      "Print all program points when printing physical size\n\
       \inference expressions. Use the menu item\n\
       \``print program points'' to print only some program\n\
       \points.")]
end

val _ = Menu.add_int_list_to_menu ("", ["Profiling", "print program points"], program_points)
(*
val _ = Menu.add_int_pair_list_to_menu ("", ["Profiling", "paths between two nodes in region flow graph"], region_paths)
*)

  (*6. Debug Kit*)

local
  fun add p n (l,sh,s,r,d) = add_bool_entry {long=l, short=sh, menu=p @[s],
					  item=r, neg=n, desc=d}
in
  val _ = app (add ["Debug", "Lambda"] true) 
  [
   ("type_check_lambda", NONE, "type check lambda expressions", type_check_lambda,
    "Type check lambda expression prior to performing region\n\
     \inference. Type checking is very fast and for normal use\n\
     \you should not disable this option. Type checking\n\
     \intermediate forms is very powerful for eliminating bugs\n\
     \in the compiler.")
   ]

  val _ = app (add ["Debug", "Manager"] false) 
  [
   ("debug_linking", NONE, "debug_linking", ref false,
    "Debug linking of target code by showing which object\n\
     \files are linked together."),
   ("debug_man_enrich", NONE, "debug compilation manager enrichment", ref false,
    "During interactive use, show information about why a\n\
     \program unit need be recompiled. In the ML Kit, a\n\
     \program unit (or a functor body) is recompiled if\n\
     \either (a) the program unit is modified, or (b)\n\
     \information about an identifier for which the program\n\
     \unit depends upon has changed.")
   ]

  val _ = app (add ["Debug"] false) 
  [
   ("chat", SOME "verbose", "chat", chat,
    "Print a message for each compilation step in the compiler."),
   ("debug_compiler", SOME "debug", "debug compiler", DEBUG_COMPILER,
    "Print intermediate forms of a program during compilation."),
   ("debug_which_at", NONE, "debug which_at (storage mode analysis)", debug_which_at,
    "Debug storage mode analysis.")
   ]
end

  (*7. Compile an sml file*)
  (*8. Compile it again*)

  local 
    fun comp_current_source_file () = !comp_ref (!current_source_file)
  in
    val _ = Menu.add_action_read_to_menu("", ["Compile file or project"],
					 comp_current_source_file,
					 current_source_file)
    val _ = Menu.add_action_with_value_to_menu("", ["Compile it again"], 
					       comp_current_source_file, 
					       fn () => "(" ^ quote (!current_source_file) ^ ")")
  end

  (*Entries not included in menu and exe-options, but in lookup functions*)

  val _ = app add_string_entry0
    [("c_compiler", c_compiler)                       (*e.g. "cc -Aa" or "gcc -ansi"*)
     ]

  val _ = app add_bool_entry0
    [
     ("enhanced_atbot_analysis", enhanced_atbot_analysis),
     ("eliminate_polymorphic_equality", eliminate_polymorphic_equality)
     ]

exception ParseScript = ParseScript.ParseScript

val is_on = Directory.is_on
val is_on0 = Directory.is_on0
val turn_on = Directory.turn_on
val turn_off = Directory.turn_off
val lookup_flag_entry = Directory.lookup_flag_entry
val get_string_entry = Directory.get_string_entry
val lookup_string_entry = Directory.lookup_string_entry
val lookup_int_entry = Directory.lookup_int_entry
val read_script = Directory.readScript
val show_script_entries = Directory.show_script_entries
val read_options = Directory.read_options
val help = Directory.help
val help_all = Directory.help_all

val interact = Menu.interact

val SMLserver = ref false
val WEBserver = ref "AOLserver"

structure Statistics = 
  struct
    val no_dangling_pointers_changes = ref 0
    val no_dangling_pointers_changes_total = ref 0
    fun reset() = (no_dangling_pointers_changes := 0;
		   no_dangling_pointers_changes_total := 0)
  end
end (* functor Flags *)  
   
  

structure profRegInf =
  struct
    val b = ref false
  end
