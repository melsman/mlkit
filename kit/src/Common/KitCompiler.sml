
(* KitCompiler does the functor applications to build up the hierarchy
 * of structures, and builds the other stuff not directly relevant to
 * the build hierarchy. It provides a convenient top-level
 * interface. *)

functor ExecutionArgs() : EXECUTION_ARGS = (* The EXECUTION_ARGS signature is *)
  struct                                   (* in the file EXECUTION.sml *)
    structure Tools = Tools()
    structure Basics = Basics(structure Tools = Tools)
    structure AllInfo = Basics.AllInfo
    structure Name = Basics.Name
    structure Report = Tools.Report
    structure Crash = Tools.Crash
    structure IntFinMap = Tools.IntFinMap
    structure Flags = Tools.Flags
    structure PP = Tools.PrettyPrint
      
    structure TopdecParsing  = TopdecParsing(structure Basics = Basics)      
	
    structure Elaboration = Elaboration(structure TopdecParsing = TopdecParsing)
      
    structure FreeIds = FreeIds
      (structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure Environments = Basics.Environments
       structure ModuleEnvironments = Basics.ModuleEnvironments
       structure ElabInfo = AllInfo.ElabInfo
       structure Crash = Tools.Crash
       structure PP = PP)
	 
    structure Lvars = Lvars(structure Name = Name
			    structure Report = Report
			    structure PP = PP
			    structure Crash = Crash
			    structure IntFinMap = IntFinMap)
      
    structure Lvarset = Lvarset(structure Lvars = Lvars)
      
    structure Labels = AddressLabels(structure Name = Name)
      
    structure Con = Con(structure Name = Name
			structure Report = Report
			structure PP = PP
			structure Crash = Crash
			structure IntFinMap = IntFinMap)
      
    structure Excon = Excon(structure Name = Name
			    structure Report = Report
			    structure PP = PP
			    structure Crash = Crash
			    structure IntFinMap = IntFinMap)
  end (*ExecutionArgs*)

signature KIT_COMPILER = 
  sig include MANAGER 
    val build_basislib : unit -> unit
    val install : unit -> unit 
    val kit : string -> unit
    val kitexe : string * string list -> OS.Process.status
    structure Flags : FLAGS
    structure Crash : CRASH
  end 
    
functor KitCompiler(Execution : EXECUTION) : KIT_COMPILER =
  struct
    open Execution

    structure Basics = Elaboration.Basics
    structure Tools = Basics.Tools
    structure Flags = Tools.Flags
    structure Crash = Tools.Crash
    structure AllInfo = Basics.AllInfo

    structure OpacityElim = OpacityElim(structure Crash = Tools.Crash
					structure PP = Tools.PrettyPrint
					structure ElabInfo = AllInfo.ElabInfo
					structure Environments = Basics.Environments
					structure ModuleEnvironments = Basics.ModuleEnvironments
					structure OpacityEnv = Basics.OpacityEnv
					structure StatObject = Basics.StatObject
					structure TopdecGrammar = Elaboration.PostElabTopdecGrammar)

    structure ManagerObjects =
      ManagerObjects(structure ModuleEnvironments = Basics.ModuleEnvironments
		     structure OpacityElim = OpacityElim
		     structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
		     structure ElabRep = Elaboration.ElabRepository
		     structure RepositoryFinMap = Elaboration.RepositoryFinMap
		     structure Execution = Execution
		     structure Labels = Labels
		     structure InfixBasis = TopdecParsing.InfixBasis
		     structure FinMap = Tools.FinMap
		     structure PP = Tools.PrettyPrint
		     structure Name = Basics.Name
		     structure Flags = Tools.Flags
		     structure Crash = Tools.Crash)
      
    structure ParseElab = ParseElab
      (structure Parse = TopdecParsing.Parse
       structure Timing = Tools.Timing
       structure ElabTopdec = Elaboration.ElabTopdec
       structure ModuleEnvironments = Basics.ModuleEnvironments
       structure PreElabTopdecGrammar = TopdecParsing.PreElabTopdecGrammar
       structure PostElabTopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure ErrorTraverse = ErrorTraverse
	 (structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
	  structure ElabInfo = AllInfo.ElabInfo
	  structure Report = Tools.Report
	  structure PrettyPrint = Tools.PrettyPrint
	  structure Crash = Tools.Crash)
       structure InfixBasis = TopdecParsing.InfixBasis
       structure TopLevelReport =
		      TopLevelReport(structure FunId = Basics.FunId
				     structure SigId = Basics.SigId
				     structure StrId = Basics.StrId
				     structure Ident = Basics.Ident
				     structure InfixBasis = TopdecParsing.InfixBasis
				     structure StatObject = Basics.StatObject
				     structure Environments = Basics.Environments
				     structure ModuleStatObject = Basics.ModuleStatObject
				     structure ModuleEnvironments = Basics.ModuleEnvironments
				     structure Report = Tools.Report
				     structure Crash = Tools.Crash)
       structure BasicIO = Tools.BasicIO
       structure Report = Tools.Report
       structure PP = Tools.PrettyPrint
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash)

    structure IntModules = 
      IntModules(structure Name = Basics.Name
		 structure LexBasics = Basics.LexBasics
		 structure ModuleEnvironments = Basics.ModuleEnvironments
		 structure ParseElab = ParseElab
		 structure OpacityElim = OpacityElim
		 structure ManagerObjects = ManagerObjects
		 structure CompilerEnv = Execution.CompilerEnv
		 structure ElabInfo = AllInfo.ElabInfo
		 structure Environments = Basics.Environments
		 structure CompileBasis = Execution.CompileBasis
		 structure FreeIds = FreeIds
		 structure Execution = Execution
		 structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
		 structure Crash = Tools.Crash
		 structure Report = Tools.Report
		 structure PP = Tools.PrettyPrint
		 structure Flags = Tools.Flags)

    structure Manager =
      Manager(structure ManagerObjects = ManagerObjects
	      structure OpacityElim = OpacityElim
	      structure Name = Basics.Name
	      structure Environments = Basics.Environments
	      structure ModuleEnvironments = Basics.ModuleEnvironments
	      structure ParseElab = ParseElab
	      structure IntModules = IntModules
	      structure FreeIds = FreeIds
	      structure Timing = Tools.Timing
	      structure Crash = Tools.Crash
	      structure Report = Tools.Report
	      structure PP = Tools.PrettyPrint
	      structure Flags = Tools.Flags)

    val import_basislib = Flags.lookup_flag_entry "import_basislib"


      local

	(* Directories *)

	val kitsrc_path = OS.FileSys.getDir()   (* assumes we are in kit/src/ directory *)
	val _ = Flags.install_dir := OS.Path.mkCanonical(OS.Path.concat(kitsrc_path, ".."))
	val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../bin"))
	val kitbinkit_path = OS.Path.joinDirFile{dir=kitbin_path, file="kit"}

	fun set_paths install_dir =
	  let 
	    val bindir = OS.Path.concat(install_dir, "bin")
	    fun set_path entry file = 
	      Flags.lookup_string_entry entry := OS.Path.concat(bindir, file);
	  in
	    Flags.install_dir := install_dir;
	    
	    Flags.basislib_project := 
	    (OS.Path.mkCanonical (OS.Path.concat(install_dir, "basislib/basislib.pm")))
	  end

	val date = Date.fmt "%b %d, %Y" (Date.fromTimeLocal (Time.now()))
	val version = "4.1.3"

	fun print_greetings() =
	  if !Flags.SMLserver then
	    print("SMLserver (" ^ !Flags.WEBserver ^ ") version " ^ version ^ ", " ^ date ^ "\n" ^
		  "Based on the ML Kit [" ^ backend_name ^ " Backend]\n")
	  else 
	    print("ML Kit version " ^ version ^ ", " ^ date ^ " [" ^
		  backend_name ^ " Backend]\n")

	fun cmd_name() = 
	  if !Flags.SMLserver then "smlserverc"
	  else if backend_name = "native" then "mlkit" 
	       else "mlkit_kam"
	    
	fun print_usage() = print ("\nUsage: " ^ cmd_name() ^ " [OPTION]... [file.sml | file.sig | file.pm]\n\n" ^
				   "Options:\n\n")

	val options = [("-script file", ["Read compiler options from `file'."]),
		       ("-version", ["Print ML Kit version information and exit."]),
		       ("-help s", ["Print help information about an option and exit."]),
		       ("-help", ["Print help information and exit."])
		       ]

	fun print_indent nil = ()
	  | print_indent (s::ss) = (print ("     " ^ s ^ "\n"); print_indent ss)

      	fun print_options() = app (fn (t, l) => (print(t ^ "\n"); print_indent l; print "\n")) options
 
	val unary_options =
	  [("script", Flags.read_script),
	   ("help", fn s => (print "\n"; 
			     print (Flags.help s); 
			     print "\n";
			     raise Fail ""))]

	val nullary_options =
	  [("version", fn () => raise Fail ""),
	   ("help", fn () => (print_usage();
			      print_options();
			      print (Flags.help_all()); 
			      raise Fail ""))]

	local 
	  fun go_files [file] = ((Manager.comp file; OS.Process.success) 
				 handle Manager.PARSE_ELAB_ERROR _ => OS.Process.failure)
	    | go_files nil = (Flags.interact(); OS.Process.success)
	    | go_files _ = (raise Fail "I expect at most one file name"; OS.Process.failure)

	  fun go_options options =
	    let val rest = Flags.read_options{options=options, nullary=nullary_options,
					      unary=unary_options}
	    in go_files rest
	    end
	in
	  fun kitexe(root_dir, args) = 
	    (print_greetings(); set_paths root_dir; go_options args)
	    handle Fail "" => OS.Process.success
		 | Fail s => (print ("Error: " ^ s ^ "\n"); 
			      OS.Process.failure)
	end

	(* As default root directory we use the absolute path that corresponds to the kit-directory 
	 * in which the Kit is built. When the Kit is properly installed this directory can be changed
	 * by passing another directory than this to the Kit executable; we assume we are in the 
	 * kit/src directory... *)
	val default_root_dir = OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(), ".."))
	fun arch_os() = 
	  case SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName()
	    of "X86-Linux" => "x86-linux"
	     | "HPPA-HPUX" => "hppa-hpux"
	     | "X86-BSD" => "x86-bsd"
	     | s => s
	fun die s = Crash.impossible ("KitCompiler." ^ s)
      in
	open Manager
		    
	fun build_basislib() =
	  let val memo = !import_basislib
	    fun postjob() = (OS.FileSys.chDir "../src"; import_basislib := memo) 
	  in (import_basislib := false;
	      print "\n ** Building basis library **\n\n";
	      OS.FileSys.chDir "../basislib";
	      set_paths (OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(),"..")));
	      Manager.comp "basislib.pm";
	      postjob()) handle exn => (postjob(); raise exn)
	  end

	(* the first argument is the Kit installation directory *)
	val kitexe = fn a => 
	  let fun strip_install_dir (_, install_dir :: rest) = (install_dir, rest)
		| strip_install_dir _ = die ("strip_install_dir: An install directory must be " ^
					     "provided as an argument to the executable!")
	  in (kitexe o strip_install_dir) a
	  end

	fun install() =
	  let 
	    val _ = print "\n ** Exporting compiler executable **\n\n"
	    val kitbinkitimage_path = OS.Path.concat(default_root_dir, "bin/kit." ^ arch_os())
	    val os = TextIO.openOut kitbinkit_path
	    val _ = (TextIO.output(os, "sml @SMLload=" ^ kitbinkitimage_path ^ " " ^ 
				   default_root_dir ^ " $*"); 
		     TextIO.closeOut os)
	    val _ = OS.Process.system("chmod a+x " ^ kitbinkit_path)
	      handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
				 kitbinkit_path ^ "' failed***\n");
			   OS.Process.failure)
	  in SMLofNJ.exportFn(kitbinkit_path, kitexe)
	  end
	
	fun kit scriptfile = (print_greetings();
			      set_paths default_root_dir;
			      Flags.read_script scriptfile;
			      Flags.interact())

	val comp = fn f => (set_paths default_root_dir;
			    Manager.comp f)

      end
	
  end (*KitCompiler*)

structure ExecutionArgs = ExecutionArgs()
structure BuildCompile = BuildCompile (ExecutionArgs)
