
(* KitCompiler does the functor application to build up the hierarchy
 * of structures, and builds the other stuff not directly relevant to
 * the build heirarchy. It provides a convenient top-level
 * interface. *)

(* Parts of KitCompiler has been moved to new units: Elaboration and
 * Execution. *)

functor KitCompiler() : sig include MANAGER 
                            val test : unit -> unit
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
					structure ElabInfo = AllInfo.ElabInfo
					structure Environments = Basics.Environments
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

      structure TestEnv = TestEnv(structure TestInfo = TestInfo (structure Flags = Tools.Flags)
				  structure Flags = Tools.Flags
				  structure Manager = Manager
				  structure Basics = Basics
				  structure Timing = Tools.Timing)

      val test = TestEnv.test
      open Manager

  end;


structure KitCompiler = KitCompiler ()

structure K = struct
  open KitCompiler

  local

    (* Directories *)

    val kitsrc_path = OS.FileSys.getDir()   (* assumes we are in kit/src/ directory *)
    val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../bin"))
    val kitbinkit_path = OS.Path.joinDirFile{dir=kitbin_path, file="kit"}
    val kitbinkitimage_path = OS.Path.joinDirFile{dir=kitbin_path, file="kit.hppa-hpux9"}

    fun set_paths() = 
      (Flags.lookup_string_entry "path_to_runtime" := 
       OS.Path.concat(kitsrc_path, "Runtime/Version17/runtimeHPUX.o");
       Flags.lookup_string_entry "path_to_runtime_prof" := 
       OS.Path.concat(kitsrc_path, "Runtime/Version17/runtimeHPUXProf.o");
       Flags.lookup_string_entry "test_env_directory" := 
       (OS.Path.mkCanonical (OS.Path.concat(kitsrc_path, "../TestEnv"))))

    val date = Date.fmt "%B %d, %Y" (Date.fromTimeLocal (Time.now()))
    val version = "2.1"
    val greetings = "ML Kit with Regions, Version " ^ version ^ ", " ^ date ^ "\n"

    val _ = Flags.lookup_string_entry "kit_architecture" := "HPUX"
    val _ = Flags.lookup_string_entry "kit_version" := "ML_to_HPPA_on_HPUX"

  in
    fun kit() = (print greetings;
		 set_paths();
		 Flags.read_script "kit.script";
		 Flags.interact())

    fun install() =
      let val os = TextIO.openOut kitbinkit_path
	  val _ = (TextIO.output(os, "sml110 @SMLload=" ^ kitbinkitimage_path); TextIO.closeOut os)
	  fun kitexe (_, []) = (kit(); OS.Process.success)
	    | kitexe _ = (print "usage: kit\n"; OS.Process.failure) 
      in SMLofNJ.exportFn(kitbinkit_path,kitexe)
      end
  end



(*
  fun i a = ((*OS..chDir "/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/src/" ;*)
	     Flags.lookup_string_entry "path_to_kit_script"
	     := "../bin/ML_to_HPPA_on_HPUX/kit.script" ;
	     (*this path means that you must be in the src/ directory,
	      which is where you will be, if you want Make.again{} etc.
	      to work.  20/06/1997 13:32. tho.*)
	     print ("Script file is " ^ Flags.get_string_entry "path_to_kit_script" ^ "\n\
	      \Reading script file.\n") ;
	     Flags.read_script () ;
	     Flags.interact ()) ;
  fun ib a = ((*System.cd "/usr/local/topps/MLKit/version2_onwards/hojfeld/kit/src/" ;*)
	     Flags.lookup_string_entry "path_to_kit_script"
	     := "../bin/basislib.script" ;
	     (*this path means that you must be in the src/ directory,
	      which is where you will be, if you want Make.again{} etc.
	      to work.  20/06/1997 13:32. tho.*)
	     print ("Script file is " ^ Flags.get_string_entry "path_to_kit_script" ^ "\n\
	      \Reading script file.\n") ;
	     Flags.read_script () ;
	     Flags.interact ()) ;
*)
end (*structure K*)
