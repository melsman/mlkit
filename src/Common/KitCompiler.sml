
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


    fun cmd_name() = OS.Path.file(CommandLine.name())

    structure ManagerObjects =
	ManagerObjects(struct
                         structure Execution = Execution
                         val program_name = cmd_name
                       end)

    structure ModCodeMini = struct
      type modcode = ManagerObjects.modcode
      type linkinfo = ManagerObjects.linkinfo
      type target = ManagerObjects.target
      val seq = ManagerObjects.ModCode.seq
      val empty = ManagerObjects.ModCode.empty
      val mk_modcode = ManagerObjects.ModCode.mk_modcode
      val emit = ManagerObjects.ModCode.emit
    end
                     
    structure IntModules = 
	IntModules(structure ManagerObjects = ManagerObjects
                   structure ModCodeMini = ModCodeMini
		   structure Execution = Execution
                   val mlbdir = ManagerObjects.mlbdir)

    structure Manager =
	Manager(structure ManagerObjects = ManagerObjects
		structure IntModules = IntModules)

    val import_basislib = Flags.lookup_flag_entry "import_basislib"

    local
	(* Directories and files *)

	fun set_paths install_dir =
	    Flags.install_dir := install_dir

	val date = Version.configure_date

	fun print_greetings() =
	    let val version = 
                    (Version.version 
                     ^ "; " ^ date ^ ")")
              val msg = 
                  if !Flags.SMLserver then "SMLserver Compiler " ^ version ^ "\n"
                  else if backend_name = "SmlToJs" then "SmlToJs " ^ version ^ "\n"
                  else ("MLKit " ^ version ^ " [" 
                        ^ backend_name ^ " Backend]\n")
            in print msg
	    end
	    
	fun print_usage() = print ("\nUsage: " ^ cmd_name() ^ " [OPTION]... [file.sml | file.sig | file.mlb]\n\n" ^
				   "Options:\n\n")

	val options = [("version", ["v","V"], ["Print version information and exit."]),
		       ("help", [], ["Print extended help information and exit."]),
		       ("help S", [], ["Print help information about an option and exit."]),
		       ("man", [], ["Print man-page and exit."])
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
		
	    fun go_files [file] = 
		((Manager.comp file; OS.Process.success) 
		 handle Manager.PARSE_ELAB_ERROR _ => OS.Process.failure)
	      | go_files _ = (print_greetings(); print_usage(); print_options(); raise Fail "")

	    fun go_options options =
		let val rest = Flags.read_options{options=options, nullary=nullary_options,
						  unary=unary_options}
		    val baseDir = 
			 case ManagerObjects.Environment.getEnvVal "SML_LIB" of 
			     SOME v => v
			   | NONE => raise Fail ("A library install directory must be provided in an\n" ^
						 "environment variable SML_LIB or as a path-definition\n" ^
						 "in either the system wide path-map " ^ Configuration.etcdir ^ "/" ^ (cmd_name()) ^ "/mlb-path-map\n" ^
						 "or in your personal path-map ~/." ^ (cmd_name()) ^ "/mlb-path-map.")

		in set_paths baseDir ; go_files rest
		end
	in
  	    (* fun die s = Crash.impossible ("KitCompiler." ^ s) *)
  	    fun kitexe(root_dir, args) = 
		go_options args
		handle Fail "" => OS.Process.success
		     | Fail s => (print ("* Error: " ^ s ^ ".\n");
                                  print ("* Exiting!\n");
				  OS.Process.failure)
	end
    in
	open Manager
	val kitexe = kitexe
    end
	
  end (*KitCompiler*)
