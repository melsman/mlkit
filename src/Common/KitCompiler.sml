
(* KitCompiler does the functor application to build up the hierarchy
 * of structures, and builds the other stuff not directly relevant to
 * the build heirarchy. It provides a convenient top-level
 * interface. *)

(* Parts of KitCompiler has been moved to new units: Elaboration and
 * Execution. *)

functor KitCompiler() : sig include MANAGER 
			    structure Flags : FLAGS
			    structure Crash : CRASH
			end  =
  struct
    structure Tools   = Tools()
    structure Basics  = Basics(structure Tools = Tools)
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

    structure Execution = Execution(structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
				    structure FreeIds = FreeIds
				    structure Basics = Basics
				    structure Lvars = Lvars
				    structure Lvarset = Lvarset
				    structure Con = Con
				    structure Excon = Excon
				    structure Labels = Labels)

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
		     structure Execution = Execution
		     structure Labels = Labels
		     structure InfixBasis = TopdecParsing.InfixBasis
		     structure FinMap = Tools.FinMap
		     structure PP = Tools.PrettyPrint
		     structure Name = Basics.Name
		     structure Flags = Flags
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
      open Manager

  end;


structure KitCompiler = KitCompiler ()

structure K = struct
  open KitCompiler

  local

    (* To ease the setup process, we setup the Kit to compile programs
     * for the architecture that the Kit itself is compiled
     * under. Thus, cross-compiling is not possible without modifying the
     * following code. *)

    (* Directories *)

    val kitsrc_path = OS.FileSys.getDir()   (* assumes we are in kit/src/ directory *)
    val _ = Flags.install_dir := OS.Path.mkCanonical(OS.Path.concat(kitsrc_path, ".."))
    val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../bin"))
    val kitbinkit_path = OS.Path.joinDirFile{dir=kitbin_path, file="kit"}

    fun set_paths root_dir =
      let val kitsrc_dir = OS.Path.concat(root_dir, "src")
      in
	Flags.lookup_string_entry "path_to_runtime" := 
	(if Flags.is_on "enable_lambda_backend" then
	   OS.Path.concat(kitsrc_dir, "RuntimeWithGC/runtimeSystem.o")
	 else
	   OS.Path.concat(kitsrc_dir, "Runtime/runtimeSystem.o"));
        Flags.lookup_string_entry "path_to_runtime_prof" := 
	(if Flags.is_on "enable_lambda_backend" then 
	   OS.Path.concat(kitsrc_dir, "RuntimeWithGC/runtimeSystemProf.o")
	 else
	   OS.Path.concat(kitsrc_dir, "Runtime/runtimeSystemProf.o"));

        Flags.lookup_string_entry "path_to_runtime_gc" := 
	OS.Path.concat(kitsrc_dir, "RuntimeWithGC/runtimeSystemGC.o");
	Flags.lookup_string_entry "path_to_runtime_gc_prof" := 
	OS.Path.concat(kitsrc_dir, "RuntimeWithGC/runtimeSystemGCProf.o");
	
	Flags.basislib_project := 
	(OS.Path.mkCanonical (OS.Path.concat(kitsrc_dir, "../basislib/basislib.pm")))
      end

    fun set_paths_x86_linux install_dir =
      let fun set_path(entry, path) = 
	    Flags.lookup_string_entry entry := OS.Path.concat(install_dir, path);
      in
	Flags.install_dir := install_dir;
	set_path("path_to_runtime", "runtime/runtimeSystem.o");
	set_path("path_to_runtime_prof", "runtime/runtimeSystemProf.o");
	set_path("path_to_runtime_gc", "runtime/runtimeSystemGC.o");
	set_path("path_to_runtime_gc_prof", "runtime/runtimeSystemGCProf.o");
	Flags.basislib_project := OS.Path.concat(install_dir, "basislib/basislib.pm")
      end

    fun arch_os() = SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName()
    val date = Date.fmt "%B %d, %Y" (Date.fromTimeLocal (Time.now()))
    val version = "3.1"
    val greetings = "ML Kit with Regions, Version " ^ version ^ ", " ^ date ^ "\n" ^
                    "Using the " ^ arch_os() ^ " " ^ Flags.get_string_entry "kit_backend" ^ " backend\n"
    val usage = "kit [-script | -timings | -nobasislib | -reportfilesig | -logtofiles " ^
                "| -prof | -gc | -delay_assembly | -chat | -nodso | -version | -help] file"
    local 
      datatype source = SML of string | PM of string
      fun determine_source (s:string) : source option = 
	case OS.Path.ext s
	  of SOME "sml" => SOME(SML s)
	   | SOME "sig" => SOME(SML s)
	   | SOME "pm" => SOME(PM s)
	   | SOME ext => (print "File name must have extension `.pm', `.sml', or `.sig'.\n";
			  print ("The file name you gave me has extension `" ^ ext ^ "'.\n"); NONE)
	   | NONE => (print "File name must have extension `.pm', `.sml', or `.sig'.\n";
		      print "The file name you gave me has no extension.\n"; NONE)
      exception Version

      fun loop_args_and_go (l, script) =
	let
	  fun go [] = (Flags.interact(); OS.Process.success)
	    | go [file] = ((case determine_source file
			      of SOME (SML s) => (comp s; OS.Process.success) 
			       | SOME (PM s) => (build s; OS.Process.success)
			       | NONE => OS.Process.failure)
			   handle PARSE_ELAB_ERROR _ => OS.Process.failure)
	    | go _ = (print "Error: I expect at most one file name.\n"; OS.Process.failure)
	  fun loop ("-script"::script::rest, _) = loop (rest, script)
	    | loop ("-timings"::rest, script) = 
	    (let val os = TextIO.openOut "KITtimings"
	     in Flags.timings_stream := SOME os;
	       loop (rest, script)
	     end handle _ => (print "Error: I could not open file `KITtimings' for writing.\n"; 
			      OS.Process.failure))
	    | loop ("-nobasislib"::rest, script) = (Flags.auto_import_basislib := false; loop (rest, script))
	    | loop ("-reportfilesig"::rest, script) = (Flags.turn_on "report_file_sig"; loop (rest, script))
	    | loop ("-logtofiles"::rest, script) = (Flags.turn_on "log_to_file"; loop (rest, script))
	    | loop ("-prof"::rest, script) = (Flags.turn_on "region_profiling"; loop (rest, script))
	    | loop ("-gc"::rest, script) = (Flags.turn_on "garbage_collection"; loop (rest, script))
	    | loop ("-delay_assembly"::rest, script) = (Flags.turn_on "delay_assembly"; loop (rest, script))
	    | loop ("-chat"::rest, script) = (Flags.chat := true; loop (rest, script))
	    | loop ("-nodso" ::rest, script) = (Flags.turn_off "delay_slot_optimization"; loop(rest, script))
	    | loop ("-version"::rest, script) = loop (rest, script) (*skip*)
	    | loop ("-help"::rest,script) = (print usage; loop(rest, script))
	    | loop (rest,script) = (Flags.read_script script; go rest)
	in loop(l, script)
	end
    in
      fun greet install_dir =
	(print greetings; 
	 print ("Installation directory is " ^ install_dir ^ "\n"); 
	 OS.Process.success)

      fun kitexe_x86_linux (root_dir, args) =
	case args
	  of ["-version"] => greet root_dir
	   | _ => (print greetings;
		   set_paths_x86_linux root_dir; 
		   loop_args_and_go (args, "kit.script"))

      val default_root_dir = OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(), ".."))
      fun kitexe(_, args) = 
	case args
	  of ["-version"] => greet default_root_dir
	   | _ => (print greetings; set_paths default_root_dir; loop_args_and_go (args, "kit.script"))
    end
	      
  in
    fun build_basislib() =
      let val memo = !Flags.auto_import_basislib
	  fun postjob() = (OS.FileSys.chDir "../src"; Flags.auto_import_basislib := memo) 
      in (Flags.auto_import_basislib := false;
	  print "\n ** Building basis library **\n\n";
	  OS.FileSys.chDir "../basislib";
	  set_paths (OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(),"..")));
	  build "basislib.pm";
	  postjob()) handle exn => (postjob(); raise exn)
      end

    fun die s = Crash.impossible ("KitCompiler." ^ s)

    fun install_x86_linux() =
      let fun strip_install_dir (_, install_dir :: rest) = (install_dir, rest)
	    | strip_install_dir _ = die "strip_install_dir: An install directory must be \
                                    \provided as an argument to the executable!"
      in
	print "\n ** Exporting compiler executable **\n\n";
	Flags.lookup_string_entry "c_libs" := "-lm";
	SMLofNJ.exportFn(kitbinkit_path, kitexe_x86_linux o strip_install_dir)
      end

    fun install() =
      let val _ = print "\n ** Exporting compiler executable **\n\n"
	  fun kit_image() =
	    case arch_os()
	      of "X86-Linux" => (Flags.lookup_string_entry "c_libs" := "-lm";
				      "kit.x86-linux")
	       | "HPPA-HPUX" => (Flags.lookup_string_entry "c_libs" := "-lM";
				      "kit.hppa-hpux")
	       | "SPARC-Solaris" => (Flags.lookup_string_entry "c_libs" := "-lm";
					  "kit.sparc-solaris")
	       | "SUN-OS4" => die "install: Configuration unknown"
	       | _ => die "install: Configuration unknown"
	  val kitbinkitimage_path = OS.Path.joinDirFile{dir=kitbin_path, file=kit_image()}
	  val os = TextIO.openOut kitbinkit_path
	  val _ = (TextIO.output(os, "sml @SMLload=" ^ kitbinkitimage_path ^ " $*"); TextIO.closeOut os)
	  val _ = OS.Process.system("chmod a+x " ^ kitbinkit_path)
	    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
			       kitbinkit_path ^ "' failed***\n");
			 OS.Process.success)
      in SMLofNJ.exportFn(kitbinkit_path, kitexe)
      end

    fun kit scriptfile = (print greetings;
			  set_paths default_root_dir;
			  Flags.read_script scriptfile;
			  Flags.interact())

    val kitexe = fn a => kitexe a
  end

  val cd = OS.FileSys.chDir
  val pwd = OS.FileSys.getDir

end (*structure K*)
