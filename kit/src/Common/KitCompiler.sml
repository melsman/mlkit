
(* KitCompiler does the functor applications to build up the hierarchy
 * of structures, and builds the other stuff not directly relevant to
 * the build hierarchy. It provides a convenient top-level
 * interface. *)


signature KIT_COMPILER = 
  sig include MANAGER 
    val kitexe : string * string list -> OS.Process.status
  end 
    
functor KitCompiler(Execution : EXECUTION) : KIT_COMPILER =
  struct
    open Execution

    structure ManagerObjects =
	ManagerObjects(Execution)
      
    structure IntModules = 
	IntModules(structure ManagerObjects = ManagerObjects
		   structure Execution = Execution)

    structure Manager =
	Manager(structure ManagerObjects = ManagerObjects
		structure IntModules = IntModules)

    val import_basislib = Flags.lookup_flag_entry "import_basislib"

    local
	(* Directories and files *)

	val op ## = OS.Path.concat infix ##

	fun set_paths install_dir =
	    Flags.install_dir := install_dir

	val date = Date.fmt "%b %d, %Y" (Date.fromTimeLocal (Time.now()))

	fun print_greetings() =
	  if !Flags.SMLserver then
	    print("SMLserver (Apache) version " ^ Version.version ^ ", " ^ date ^ "\n" ^
		  "Based on the MLKit [" ^ backend_name ^ " Backend]\n")
	  else 
	    print("MLKit version " ^ Version.version ^ ", " ^ date ^ " [" ^
		  backend_name ^ " Backend]\n")

	fun cmd_name() = 
	  if !Flags.SMLserver then "smlserverc"
	  else if backend_name = "X86" then "mlkit" 
	       else "mlkit_kam"
	    
	fun print_usage() = print ("\nUsage: " ^ cmd_name() ^ " [OPTION]... [file.sml | file.sig | file.mlb]\n\n" ^
				   "Options:\n\n")

	val options = [("version", ["v","V"], ["Print MLKit version information and exit."]),
		       ("man", [], ["Print man-page and exit."]),
		       ("help", [], ["Print help information and exit."]),
		       ("help S", [], ["Print help information about an option and exit."])
		       ]

	fun print_indent nil = ()
	  | print_indent (s::ss) = (print ("     " ^ s ^ "\n"); print_indent ss)

      	fun print_options() = 
              app (fn (t, s, l) => (print("--" ^ t ^ 
                                           (String.concat (List.map (fn x => ", -" ^ x) s)) ^
                                           "\n"); print_indent l; print "\n"))
                                  options
	local 
	    (* is overloading of options allowed? *)
	    val unary_options =
		[("help", fn s => (print "\n"; 
				   print (Flags.help s); 
				   print "\n";
				   raise Fail ""))]
		
	    val nullary_options =
		[("version", fn () => (print_greetings(); 
				       raise Fail "")),
		 ("man", fn () => (print(Man.gen {cmd=cmd_name,date=date,
						  extraOptions=options,
						  version=Version.version});
				   raise Fail "")),
		 ("v", fn () => (print_greetings(); 
				 raise Fail "")),
		 ("V", fn () => (print_greetings(); 
				 raise Fail "")),
		 ("help", fn () => (print_greetings(); 
				    print_usage();
				    print_options();
				    print (Flags.help_all()); 
				    raise Fail ""))]
		
	    fun go_files files = ((Manager.comp files; OS.Process.success) 
				   handle Manager.PARSE_ELAB_ERROR _ => OS.Process.failure)
	    fun go_options options =
		let val rest = Flags.read_options{options=options, nullary=nullary_options,
						  unary=unary_options}
		in go_files rest
		end
	in
  	    (* fun die s = Crash.impossible ("KitCompiler." ^ s) *)
  	    fun kitexe(root_dir, args) = 
		(let 
		     val baseDir = 
			 case Environment.getEnvVal "SML_LIB" of 
			     SOME v => v
			   | NONE => raise Fail ("An MLKit library install directory must be provided\n" ^
						 "in an environment variable SML_LIB or as a path-definition\n" ^
						 "in either the system wide path-map " ^ Configuration.etcdir ^ "/mlkit/mlb-path-map\n" ^
						 "or in your personal path-map ~/.mlkit/mlb-path-map.")
		 in
		     (set_paths baseDir; go_options args)
		 end
		 handle Fail "" => OS.Process.success
		      | Fail s => (print ("*** Error: " ^ s ^ "\n"); 
				   OS.Process.failure))
		
	end
    in
	open Manager
	val kitexe = kitexe
    end
	
  end (*KitCompiler*)
