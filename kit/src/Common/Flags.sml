(* Global flags *)

structure Flags: FLAGS =
  struct
    structure PP = PrettyPrint
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


    fun has_sml_source_ext (s: string) :bool =
	case s of 
	    "sml" => true
	  | "sig" => true
	  | _ => false

    val install_dir = ref "You_did_not_set_path_to_install_dir"
     
    (* Pretty Printing *)

    val raggedRight = PrettyPrint.raggedRight

    (* Debugging Flags *)
    val DEBUG_COMPILER          = ref false
    val chat                    = ref false

    val type_check_lambda       = ref true    (* enforce checking as the default *)

    (* Elimination of polymorphic equality *)
    val eliminate_polymorphic_equality = ref true

   (* Region inference *)
    val print_types  = ref false
    val region_inference = ref true

    (* Printing of intermediate forms *)
    val print_opt_lambda_expression = ref false

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

    val colwidth = PrettyPrint.colwidth

    val log = ref TextIO.stdOut
(*
    val indent_ccode = ref false;
*)
    (* Program manager *)

    fun dummy _ : unit = print "uninitialised function reference in Flags!"
    val comp_ref: (string -> unit)ref  = ref dummy
    val current_source_file = ref "sources.pm"
    val import_basislib = ref true

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
		    print ("\n*** " ^ Int.toString (List.length reports)
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
			val bool_entry          : bentry -> (unit -> bool)
			val string_entry        : string entry -> (unit -> string)
			val stringlist_entry    : string list entry -> (unit -> string list)
			val int_entry           : int entry -> (unit -> int)
			val bool_action_entry   : baentry -> unit

			val is_on               : string -> bool 
			val is_on0              : string -> unit -> bool 
			val turn_on             : string -> unit
			val turn_off            : string -> unit
			val add_string_entry    : string * string ref -> (unit -> string)
			val add_stringlist_entry: string * string list ref -> (unit -> string list)
			val add_int_entry       : string * int ref -> (unit -> int)
			val add_bool_entry      : string * bool ref -> (unit -> bool)
			val get_string_entry    : string -> string
			val get_stringlist_entry: string -> string list
			val lookup_stringlist_entry : string -> string list ref
			val lookup_string_entry : string -> string ref
			val lookup_flag_entry   : string -> bool ref
			val lookup_int_entry    : string -> int ref

			(* read and interpret option list by looking in directory and
			 * the extra nullary list and unary list *)
			val read_options : {nullary:(string*(unit->unit))list,
					    unary:(string*(string->unit))list,
					    options: string list} -> string list

			(* help key  provides help information for the key *)
			val help : string -> string

			(* help_all()  provides help on all options in the directory *)
			val help_all : unit -> string
      val getOptions  : unit -> 
                          {desc : string, long : string list, short : string list,
                           kind : string option, default : string option} list

		      end =
struct

    datatype entry0 = INT_ENTRY of int entry 
                    | BOOL_ENTRY of bentry
                    | BOOLA_ENTRY of baentry        (* action entry *)
                    | STRING_ENTRY of string entry
                    | STRINGLIST_ENTRY of string list entry

    structure M = OrderFinMap (struct type T = string
				      fun lt a (b:string) = a < b
			       end)

    val dir : entry0 M.map ref = ref M.empty

    fun bool_entry (e:bentry) : unit -> bool =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("bool_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => (dir := M.add(#long e, BOOL_ENTRY e,
				 case #short e of 
				     SOME s => M.add(s, BOOL_ENTRY e, !dir)
				   | NONE => !dir);
		    let val r = #item e
		    in fn () => !r
		    end)

    fun bool_action_entry (e:baentry) : unit =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("bool_action_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => dir := M.add(#long e, BOOLA_ENTRY e,
				case #short e
				  of SOME s => M.add(s, BOOLA_ENTRY e, !dir)
				   | NONE => !dir)

    fun string_entry (e:string entry) : unit -> string =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("string_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => (dir := M.add(#long e, STRING_ENTRY e,
				 case #short e of 
				     SOME s => M.add(s, STRING_ENTRY e, !dir)
				   | NONE => !dir);
		    let val r = #item e
		    in fn () => !r
		    end)

    fun stringlist_entry (e:string list entry) : unit -> string list =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("stringlist_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => (dir := M.add(#long e, STRINGLIST_ENTRY e,
				 case #short e of 
				     SOME s => M.add(s, STRINGLIST_ENTRY e, !dir)
				   | NONE => !dir);
		    let val r = #item e
		    in fn () => !r
		    end)

    fun int_entry (e:int entry) : unit -> int =
      case M.lookup (!dir) (#long e) 
	of SOME _ => die ("int_entry: entry " ^ (#long e) ^ " already in directory")
	 | NONE => (dir := M.add(#long e, INT_ENTRY e, 
				 case #short e of 
				     SOME s => M.add(s, INT_ENTRY e, !dir)
				   | NONE => !dir);
		    let val r = #item e
		    in fn () => !r
		    end)

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

    fun lookup_stringlist_entry (key) : string list ref =
      case M.lookup (!dir) key
	of SOME (STRINGLIST_ENTRY e) => #item e
	 | SOME _ => die ("lookup_stringlist_entry: entry " ^ key ^ " is of wrong kind")
	 | NONE => die ("lookup_stringlist_entry: no entry " ^ key ^ " in directory")

    fun lookup_int_entry (key) : int ref =
      case M.lookup (!dir) key
	of SOME (INT_ENTRY e) => #item e
	 | SOME _ => die ("lookup_int_entry: entry " ^ key ^ " is of wrong kind")
	 | NONE => die ("lookup_int_entry: no entry " ^ key ^ " in directory")

  val get_string_entry = ! o lookup_string_entry

  val get_stringlist_entry = ! o lookup_stringlist_entry

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

  fun add_stringlist_entry (long, item) = 
    stringlist_entry {long=long, short=NONE, desc="", item=item, menu=nil} 

  fun add_int_entry (long, item) = 
    int_entry {long=long, short=NONE, desc="", item=item, menu=nil} 

  fun add_bool_entry (long, item) = 
    bool_entry {long=long, short=NONE, desc="", item=item, menu=nil, neg=false} 

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
		     | SOME (STRINGLIST_ENTRY e) => 
			   let fun is_opt s = (String.sub(s,0) = #"-") handle _ => false
			       fun readToOpt (all as [s],acc) = 
				   if is_opt s then (rev acc, all)
				   else (case OS.Path.ext s of
					     SOME ext => if has_sml_source_ext ext then (rev acc, all)
							 else (rev (s::acc),nil)
					   | _ => (rev (s::acc),nil))
				 | readToOpt (all as s::ss,acc) = 
				       if is_opt s then (rev acc,all)
				       else readToOpt(ss,s::acc)
				 | readToOpt (nil,acc) = (rev acc,nil)
			       val (args,rest) = readToOpt (ss,nil)
			   in (#item e := args; loop rest)
			   end
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

  datatype kindOfHelp = HELP | OPTIONS | DEFAULTS

  (* help key  provides help information for the key *)
  fun help' (key: string) =
    let 
  fun optToList NONE = []
    | optToList (SOME a) = [a]

	fun bitem true = "on"
	  | bitem false = "off"
  fun opt (SOME s) = ", -" ^ s
	  | opt NONE = ""

	fun negationNew (e:bentry, kind) =
	  if not(#neg e) then []
	  else [{long = ["no_" ^ (#long e)], short = map (fn x => "no_" ^ x) (optToList (#short e)),
           kind = kind, default = NONE, desc = "Opposite of --" ^ #long e ^ opt(#short e) ^ "."}] 

	fun negationNew' (e:baentry, kind) =
	  [{long = ["no_" ^ (#long e)], short = map (fn x => "no_" ^ x) (optToList (#short e)),
      kind = kind, default = NONE, desc = "Opposite of --" ^ #long e ^ opt(#short e) ^ "."}] 

    in
      (case lookup_notnull_menu (!dir) key 
	 of SOME (BOOL_ENTRY e) =>
     {long = [#long e], short = optToList (#short e), kind = NONE, 
      default = SOME (bitem (!(#item e))), desc = #desc e} ::
      (negationNew (e,NONE))
	  | SOME (BOOLA_ENTRY e) => 
     {long = [#long e], short = optToList (#short e), default = SOME (bitem (!(#item e))),
      desc = #desc e, kind = NONE} ::
      negationNew' (e,NONE)
	  | SOME (STRING_ENTRY e) => 
     {long = [#long e], short = optToList (#short e),
      default = let val a = (String.toString(!(#item e)))
                in if a = "" then NONE else SOME a
                end,
      desc = #desc e, kind = SOME "S"} :: []
	  | SOME (STRINGLIST_ENTRY e) => 
     {long = [#long e], short = optToList (#short e), default = NONE,
      desc = #desc e, kind = SOME "S"} :: []
	  | SOME (INT_ENTRY e) =>
     {long = [#long e], short = optToList (#short e), default = SOME (Int.toString (!(#item e))),
      desc = #desc e, kind = SOME "N"} :: []
	  | NONE => raise Fail ("no help available for option: " ^ key))
    end

  fun print_help tail x =
    let
      val width = 60
      fun indent s = 
        map (fn s => "     " ^ s ^ "\n") (String.tokens (fn c => c = #"\n") s)

      fun addBetween _ [] = []
        | addBetween _ (x::[]) = [x]
        | addBetween s (x::y::zz) = x:: s :: (addBetween s (y :: zz))

      fun pkind NONE = ""
        | pkind (SOME k) = " " ^ k

      fun p {long,short,kind,default,desc} = 
        let
          val name = String.concat (
                      (addBetween ", " 
                        (List.map (fn x => "--" ^ x ^ (pkind kind)) long)) @
                      (List.map (fn x => ", -" ^ x ^ (pkind kind)) short) @ [" "]) 
          val firstline = case default
                          of NONE => name ^ "\n"
                           | SOME default => StringCvt.padRight #" " (width - (String.size default)) name ^ "(" ^ default ^ ")\n"
          val body = indent desc
        in String.concat(firstline :: body @ [tail])
        end
    in String.concat (List.map p x)
    end

    fun help x = print_help "" (help' x)
	
  (* help_all()  provides help on all options in the directory *)
  fun help_all' () =
    let val dom = rev(M.dom (!dir))
      fun add (key, acc) =
	let        (* add only if menu is non-empty and
              * the entry is not a short key *)
	  fun check (SOME k) = if k = key then acc
			       else (help' key) @ acc
	    | check NONE =  (help' key) @ acc
	in
	  case lookup_notnull_menu (!dir) key
	    of SOME (INT_ENTRY e) => check(#short e)
	     | SOME (BOOL_ENTRY e) => check(#short e)
	     | SOME (BOOLA_ENTRY e) => check(#short e)
	     | SOME (STRING_ENTRY e) => check(#short e)
	     | SOME (STRINGLIST_ENTRY e) => check(#short e)
	     | NONE => acc
	end
    fun cmp c ([],[]) = EQUAL
      | cmp c ([],_) = LESS
      | cmp c (_,[]) = GREATER
      | cmp c (x::xs,y::ys) = case c (x,y) 
                              of EQUAL => cmp c (xs,ys)
                               | GREATER => GREATER
                               | LESS => LESS
    in Listsort.sort 
        (fn ({long = l1,...},{long = l2,...}) => cmp String.compare (l1,l2))
        (foldl add [] dom)
    end

  fun help_all () = print_help "\n" (help_all' ())
  val getOptions = help_all'

end (* Directory *)

structure Menu = Menu(val help_topic = Directory.help)

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

fun add_stringlist_entry e = 
  case #menu e of 
      nil => Directory.stringlist_entry e 
    | path => ( (* Menu.add_string_to_menu(#long e, path, #item e); *)
	       Directory.stringlist_entry e)

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
  fun add (l, sh, s, r, desc) : unit = 
      (add_bool_entry {long=l, short=sh, menu=["Printing of intermediate forms",s],
		       item=r, neg=false, desc=desc}; 
       ())
in
  val _ = add  ("print_opt_lambda_expression", SOME "Pole", "print optimised lambda expression", 
		print_opt_lambda_expression, "Print Lambda Expression after optimisation.")
end

  (*2. Layout*)

local
  fun add neg (l, sh, s, r, desc) : unit = (add_bool_entry {long=l, short=sh, menu=["Layout",s],
							    item=r, neg=neg, desc=desc}; ())
in
  val _ = app (add false)
    [
     ("print_types", SOME "Ptypes", "print types", print_types,
      "Print types when printing intermediate forms. For Lambda\n\
       \Expressions, ordinary ML types are printed, whereas for\n\
       \Region Expressions, region types are printed.")
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

val recompile_basislib = ref false
val _ = add_bool_entry {long="recompile_basislib",short=SOME "scratch", 
			menu=["Control", "recompile basis library"],
			item=recompile_basislib,neg=false, 
			desc=
			"Recompile basis library from scratch. This option\n\
			 \is useful together with other options that control\n\
			 \code generation."}

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
  val gengc = ref false
  fun off() = (gc := false; 
	       preserve_tail_calls := false;
	       dangling_pointers := true;
	       tag_values := false) 
  fun on() = (gc := true; 
	      preserve_tail_calls := true;	      
	      dangling_pointers := false;
	      tag_values := true) 
  fun off_gengc() = (off(); (* We also turn gc off *)
		     gengc := false)
  fun on_gengc() = (on(); (* Gen GC needs gc to be turned on as well *)
		    gengc := true) 
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
  val _ = add_bool_action_entry
    {long="generational_garbage_collection", menu=["Control", "generational garbage collection"], 
     item=gengc, on=on_gengc, off=off_gengc, short=SOME "gengc",
     desc="Enable generational garbage collection. Same as option\n\
          \garbage collection except that two generations are used\n\
          \for each region."}
end

local
  fun add neg (l, sh, s, r, desc) : unit = (add_bool_entry {long=l, short=sh, menu=["Control",s],
							    item=r, neg=neg, desc=desc}; ())
in
  val _ = app (add false)
  [
(*
   ("all_multiplicities_infinite", NONE, "all multiplicities infinite (for POPL 96)", 
    all_multiplicities_infinite,
    "Use only infinite regions. That is, store all values in\n\
     \infinite regions, which do not reside on the stack, but\n\
     \in the heap. With this flag disabled, all regions that\n\
     \can be inferred that values are allocated in them at\n\
     \most once are allocated on the stack."),
*)
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

  val _ = add true
     ("repository", SOME "rep", "repository", ref true,
      "Use in-memory repository to avoid unnecessary\n\
      \recompilation. This flag should be disabled when\n\
      \compiling mlb-files, which make use of the file system\n\
      \as a repository.")
end

val _ = app (fn (s, f) => Menu.add_action_to_menu ("", ["Control", s], f))
  [
   ("print entire menu", Menu.show_full_menu)
   ]

  (*4. File menu*)

val _ = add_bool_entry 
     {long="log_to_file", short=NONE, menu=["File", "Log to file"], 
      neg=false, item=log_to_file, desc="Log to files instead of stdout."}

val _ = add_string_entry 
     {long="SML_LIB", short=NONE, menu=["File", "installation directory"], 
      item=install_dir,
      desc=
       "Installation directory for the MLKit standard library. For normal\n\
       \execution you should not modify this value. However,\n\
       \if you wish to use the MLKit with an altered runtime\n\
       \system you can update this\n\
       \setting and the system will try to link to a runtime\n\
       \system found in the new install\n\
       \directory."}

  (*5. Profiling menu*)

local
  fun add neg (l, sh, s, r, desc) = (add_bool_entry {long=l, short=sh, menu=["Profiling",s],
						     item=r, neg=neg, desc=desc}; ())
in
  val _ = app (add false)
  [
   ("region_profiling", SOME "prof", "region profiling", region_profiling,
    "Enable region profiling. Object code stemming\n\
     \from compiling a program with region profiling enabled\n\
     \is instrumented with profiling information. When a program\n\
     \compiled with region profiling enabled is run, the program\n\
     \produces a profile file run.rp, which can then be read\n\
     \by the profiling tool rp2ps that comes with the MLKit to\n\
     \produce profiling graphs of various forms."),
    ("print_region_flow_graph", SOME "Prfg", "print region flow graph", print_region_flow_graph,
     "Print a region flow graph for the program fragment\n\
     \and generate a .vcg-file, which can be viewed using\n\
     \the xvcg program."),
     ("print_all_program_points", SOME "Ppp", "print all program points", print_all_program_points,
      "Print all program points when printing physical size\n\
       \inference expressions.")]
end

val _ = Menu.add_int_list_to_menu ("", ["Profiling", "print program points"], program_points)
(*
val _ = Menu.add_int_pair_list_to_menu ("", ["Profiling", "paths between two nodes in region flow graph"], region_paths)
*)

  (*6. Debug Kit*)

local
    fun add p n (l,sh,s,r,d) : unit = (add_bool_entry {long=l, short=sh, menu=p @[s],
						       item=r, neg=n, desc=d}; ())
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
     \program unit need be recompiled. In the MLKit, a\n\
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
    "Print intermediate forms of a program during compilation.")
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

  val _ = add_string_entry0 ("c_compiler", c_compiler)
                        (*e.g. "cc -Aa" or "gcc -ansi"*)

  val _ = add_bool_entry0 ("enhanced_atbot_analysis", enhanced_atbot_analysis)
  val _ = add_bool_entry0 ("eliminate_polymorphic_equality", eliminate_polymorphic_equality)

  val _ = add_bool_entry 
      {long="compile_only", short=SOME "c", 
       menu=["Control","compile only"],
       item=ref false, neg=false, desc=
       "Compile only. Suppresses generation of executable"}

val is_on = Directory.is_on
val is_on0 = Directory.is_on0
val turn_on = Directory.turn_on
val turn_off = Directory.turn_off
val lookup_flag_entry = Directory.lookup_flag_entry
val get_string_entry = Directory.get_string_entry
val get_stringlist_entry = Directory.get_stringlist_entry
val lookup_string_entry = Directory.lookup_string_entry
val lookup_stringlist_entry = Directory.lookup_stringlist_entry
val lookup_int_entry = Directory.lookup_int_entry
val read_options = Directory.read_options
val help = Directory.help
val help_all = Directory.help_all
type options = {desc : string, long : string list, short : string list,
                kind : string option, default : string option} 
val getOptions = Directory.getOptions : unit -> options list

val interact = Menu.interact

val SMLserver = ref false

datatype compiler_mode = 
    LINK_MODE of string list  (* lnk-files *)
  | LOAD_BASES of string list   (* eb-files to be loaded; nil if normal *)
    
val compiler_mode : compiler_mode ref = ref (LOAD_BASES nil)
    
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
