
(* KitCompiler does the functor application to build up the hierarchy
 * of structures, and builds the other stuff not directly relevant to
 * the build heirarchy. It provides a convenient top-level
 * interface. *)

(* Parts of KitCompiler has been moved to new units: Elaboration and
 * Execution. *)

functor KitCompiler() : sig include MANAGER 
			    structure Flags : FLAGS
			end  =
  struct
    structure Tools   = Tools()
    structure Basics  = Basics(structure Tools = Tools)
    structure AllInfo = Basics.AllInfo

    structure TopdecParsing  = TopdecParsing(structure Basics = Basics)      

    structure Elaboration = Elaboration(structure TopdecParsing = TopdecParsing)

    structure Execution = Execution(structure Elaboration = Elaboration)

    structure OpacityElim = OpacityElim(structure Crash = Tools.Crash
					structure PP = Tools.PrettyPrint
					structure ElabInfo = AllInfo.ElabInfo
					structure Environments = Basics.Environments
					structure ModuleEnvironments = Basics.ModuleEnvironments
					structure OpacityEnv = Basics.OpacityEnv
					structure StatObject = Basics.StatObject
					structure TopdecGrammar = Elaboration.PostElabTopdecGrammar)

    structure Flags = Tools.Flags

    structure ManagerObjects =
      ManagerObjects(structure ModuleEnvironments = Basics.ModuleEnvironments
		     structure OpacityElim = OpacityElim
		     structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
		     structure CompilerEnv = Execution.CompilerEnv
		     structure ElabRep = Elaboration.ElabRepository
		     structure CompileBasis = Execution.CompileBasis
		     structure Compile = Execution.Compile
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
		 structure FreeIds = Execution.FreeIds
		 structure Compile = Execution.Compile
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
	      structure FreeIds = Execution.FreeIds
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
    val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../bin"))
    val kitbinkit_path = OS.Path.joinDirFile{dir=kitbin_path, file="kit"}

    fun set_paths() = 
      (Flags.lookup_string_entry "path_to_runtime" := 
       OS.Path.concat(kitsrc_path, "Runtime/runtimeSystem.o");
       Flags.lookup_string_entry "path_to_runtime_prof" := 
       OS.Path.concat(kitsrc_path, "Runtime/runtimeSystemProf.o");
       Flags.basislib_project := 
       (OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../basislib/basislib.pm"))))

    val date = Date.fmt "%B %d, %Y" (Date.fromTimeLocal (Time.now()))
    val version = "3"
    val greetings = "ML Kit with Regions, Version " ^ version ^ ", " ^ date ^ "\n" ^
                    "Using the " ^ Flags.get_string_entry "kit_backend" ^ " backend\n"

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
    in
      fun kitexe (_, ["-version"]) = (print greetings; OS.Process.success)
	| kitexe (_, l) =
	let 
	    fun exit () = OS.Process.failure
	    fun go [] = (Flags.interact(); OS.Process.success)
	      | go [file] = ((case determine_source file
				of SOME (SML s) => (comp s; OS.Process.success) 
				 | SOME (PM s) => (build s; OS.Process.success)
				 | NONE => exit())
			     handle PARSE_ELAB_ERROR _ => exit())
	      | go _ = (print "Error: I expect at most one file name.\n"; exit())
	    fun loop ("-script"::script::rest, _) = loop (rest, script)
	      | loop ("-timings"::rest, script) = 
	         (let val os = TextIO.openOut "KITtimings"
		  in Flags.timings_stream := SOME os;
		    loop (rest, script)
		  end handle _ => (print "Error: I could not open file `KITtimings' for writing.\n"; exit()))
	      | loop ("-nobasislib"::rest, script) = (Flags.auto_import_basislib := false; loop (rest, script))
	      | loop ("-reportfilesig"::rest, script) = (Flags.lookup_flag_entry "report_file_sig" := true; loop (rest, script))
	      | loop ("-logtofiles"::rest, script) = (Flags.lookup_flag_entry "log_to_file" := true; loop (rest, script))
	      | loop ("-prof"::rest, script) = (Flags.lookup_flag_entry "region_profiling" := true; loop (rest, script))
	      | loop ("-version"::rest, script) = loop (rest, script) (*skip*)
	      | loop (rest,script) = (Flags.read_script script; go rest)
	in print greetings;
	   set_paths(); 
	   loop (l, "kit.script")
	end
    end
	      
  in
    fun build_basislib() =
      let val memo = !Flags.auto_import_basislib
	  fun postjob() = (OS.FileSys.chDir "../src"; Flags.auto_import_basislib := memo) 
      in (Flags.auto_import_basislib := false;
	  print "\n ** Building basis library **\n\n";
	  OS.FileSys.chDir "../basislib";
	  set_paths();
	  build "basislib.pm";
	  postjob()) handle exn => (postjob(); raise exn)
      end

    fun install() =
      let val _ = print "\n ** Installing compiler executable **\n\n"
          fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
	  fun kit_image() =
	    case arch_os()
	      of ("X86", "Linux") => (Flags.lookup_string_entry "c_libs" := "-lm";
				      "kit.x86-linux")
	    | ("HPPA", "HPUX") => (Flags.lookup_string_entry "c_libs" := "-lM";
				   "kit.hppa-hpux9")
	    | ("SUN", "OS4") => "unknown"
	    | _ => "unknown"
	  val kitbinkitimage_path = OS.Path.joinDirFile{dir=kitbin_path, file=kit_image()}
	  val os = TextIO.openOut kitbinkit_path
	  val _ = (TextIO.output(os, "sml @SMLload=" ^ kitbinkitimage_path ^ " $*"); TextIO.closeOut os)
	  val _ = OS.Process.system("chmod a+x " ^ kitbinkit_path)
	    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ kitbinkit_path ^ "' failed***\n");
			 OS.Process.success)
      in SMLofNJ.exportFn(kitbinkit_path,kitexe)
      end

    fun kit scriptfile = (print greetings;
			  set_paths();
			  Flags.read_script scriptfile;
			  Flags.interact())
  end

  val cd = OS.FileSys.chDir
  val pwd = OS.FileSys.getDir

end (*structure K*)


