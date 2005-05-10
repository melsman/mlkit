
(* KitCompiler does the functor applications to build up the hierarchy
 * of structures, and builds the other stuff not directly relevant to
 * the build hierarchy. It provides a convenient top-level
 * interface. *)


signature KIT_COMPILER = 
  sig include MANAGER 
    val kitexe : string * string list -> OS.Process.status
    val build_basislib : unit -> unit
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

	val kitsrc_path = OS.FileSys.getDir()   (* assumes we are in kit/src/ directory *)
	val _ = Flags.install_dir := OS.Path.mkCanonical(OS.Path.concat(kitsrc_path, ".."))
	val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../bin"))
	val kitbinkit_path = OS.Path.joinDirFile{dir=kitbin_path, file="kit"}
	val basislib_file = "basis.pm"

	fun set_paths install_dir =
	  let 
	    val bindir = OS.Path.concat(install_dir, "bin")
	    fun set_path entry file = 
	      Flags.lookup_string_entry entry := OS.Path.concat(bindir, file);
	  in
	    Flags.install_dir := install_dir;
	    
	    Flags.basislib_project := 
	    (OS.Path.mkCanonical (OS.Path.concat(install_dir, "basis/" ^ basislib_file)))
	  end

	val date = Date.fmt "%b %d, %Y" (Date.fromTimeLocal (Time.now()))

	fun print_greetings() =
	  if !Flags.SMLserver then
	    print("SMLserver (" ^ !Flags.WEBserver ^ ") version " ^ Version.version ^ ", " ^ date ^ "\n" ^
		  "Based on the MLKit [" ^ backend_name ^ " Backend]\n")
	  else 
	    print("ML Kit version " ^ Version.version ^ ", " ^ date ^ " [" ^
		  backend_name ^ " Backend]\n")

	fun cmd_name() = 
	  if !Flags.SMLserver then "smlserverc"
	  else if backend_name = "native" then "mlkit" 
	       else "mlkit_kam"
	    
	fun print_usage() = print ("\nUsage: " ^ cmd_name() ^ " [OPTION]... [file.sml | file.sig | file.pm | file.mlb]\n\n" ^
				   "Options:\n\n")

	val options = [("-script file", ["Read compiler options from `file'."]),
		       ("-version", ["Print MLKit version information and exit."]),
		       ("-help s", ["Print help information about an option and exit."]),
		       ("-help", ["Print help information and exit."]),
		       ("-load f1.eb ... fn.eb", ["Load export bases."]),
		       ("-link f1.lnk ... fn.lnk", ["Construct executable."])
		       ]

	fun print_indent nil = ()
	  | print_indent (s::ss) = (print ("     " ^ s ^ "\n"); print_indent ss)

      	fun print_options() = app (fn (t, l) => (print(t ^ "\n"); print_indent l; print "\n")) options
 
	local 
	    (* is overloading of options allowed? *)
	    val unary_options =
		[("script", Flags.read_script),
		 ("help", fn s => (print "\n"; 
				   print (Flags.help s); 
				   print "\n";
				   raise Fail ""))]
		
	    val nullary_options =
		[("version", fn () => (print_greetings(); 
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
	    fun kitexe(root_dir, args) = 
		(set_paths root_dir; go_options args)
		handle Fail "" => OS.Process.success
		     | Fail s => (print ("Error: " ^ s ^ "\n"); 
				  OS.Process.failure)
	end

	(* As default root directory we use the absolute path that corresponds to the kit-directory 
	 * in which the Kit is built. When the Kit is properly installed this directory can be changed
	 * by passing another directory than this to the Kit executable; we assume we are in the 
	 * kit/src directory... *)
	val default_root_dir = OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(), ".."))
	fun die s = Crash.impossible ("KitCompiler." ^ s)
      in
	open Manager

	fun build_basislib() =
	  let val memo = !import_basislib
	    fun postjob() = (OS.FileSys.chDir "../src"; import_basislib := memo) 
	  in (import_basislib := false;
	      print "\n ** Building basis library **\n\n";
	      OS.FileSys.chDir "../basis";
	      set_paths (OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(),"..")));
	      Manager.comp [basislib_file];
	      postjob()) handle exn => (postjob(); raise exn)
	  end
		    
	(* the first argument is the Kit installation directory *)
	val kitexe = fn a => 
	  let fun strip_install_dir (_, install_dir :: rest) = (install_dir, rest)
		| strip_install_dir _ = die ("strip_install_dir: An install directory must be " ^
					     "provided as an argument to the executable!")
	  in (kitexe o strip_install_dir) a
	  end
      end
	
  end (*KitCompiler*)
