(*$TestInfo: TEST_INFO FLAGS*)
functor TestInfo (structure Flags : FLAGS) : TEST_INFO =
  struct

    (*****************************************************************************)
    (* The module defines global values which are used by the test environment.  *)
    (*                                                                           *)
    (* Values which can be changed can be found by searching after               *)
    (*                    (*--USER--*)                                           *)
    (*****************************************************************************)

    (*-------------------------------------*)
    (* Variables declaring the strategies  *)
    (* If changed, then remember to make   *)
    (* a runtime system which matches the  *)
    (* strategy, and insert an old version *)
    (* to compare with if exists.          *)    
    (*-------------------------------------*)

    (*-------------------------------------------*)
    (*          The strategy type                *)
    (*-------------------------------------------*)

    datatype strategy = ACCEPTANCE_STRATEGY of {kit_script     : string, 
						runtime_system : unit -> string,
						strategy_name  : string, (* single word *)
						comment        : string,
						exec_opt       : string,
						old_dir        : string Option}
                      | PERFORMANCE_STRATEGY of {kit_script            : string,
						 runtime_system        : unit -> string,
						 strategy_name         : string,
						 show_compiler_timings : bool,
						 comment               : string}

    fun path_to_runtime_suspension () = !(Flags.lookup_string_entry "path_to_runtime")
    fun path_to_runtime_prof_suspension () = !(Flags.lookup_string_entry "path_to_runtime_prof")
    val test_env_directory = Flags.lookup_string_entry "test_env_directory"
    val kit_version = Flags.lookup_string_entry "kit_version"

    (*-------------------------------------------*)
    (* Variables controlling the ACCEPTANCE test *)
    (*-------------------------------------------*)

    local
      (* These strategies can be changed, deleted or new inserted. *) 
      fun acceptance_strategy_HPUX_HPPA () = 
	    ACCEPTANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_suspension,
				 strategy_name = "NoProf", (* single word *)
				 comment = "Region profiling not enabled.",
				 exec_opt = "",
				 old_dir = Some (!test_env_directory ^
						 "Output_HPPA_on_HPUX/Acceptance/NoProf/")}
      
      fun acceptance_strategy_HPUX_HPPA_prof () = 
	    ACCEPTANCE_STRATEGY {kit_script = "prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_prof_suspension,
				 strategy_name = "Prof", (* single word *)
				 comment = "Region profiling enabled. A profile datafile is not exported. \
				  \ Profiling every 500000 microsec.",
				 exec_opt = "-microsec 500000 -noDatafile -noStat",
				 old_dir = Some (!test_env_directory ^
						 "Output_HPPA_on_HPUX/Acceptance/Prof/")}

      fun acceptance_strategy_HPUX_C () = 
	    ACCEPTANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_suspension,
				 strategy_name = "NoProf", (* single word *)
				 comment = "Region profiling not enabled.",
				 exec_opt = "",
				 old_dir = Some (!test_env_directory ^
						 "Output_C_on_HPUX/Acceptance/NoProf/")}
      
      fun acceptance_strategy_HPUX_C_prof () = 
	    ACCEPTANCE_STRATEGY {kit_script = "prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_prof_suspension,
				 strategy_name = "Prof", (* single word *)
				 comment = "Region profiling enabled. A profile datafile is not exported. \
				  \ Profiling every 4 second.",
				 exec_opt = "-sec 4 -noDatafile -noStat",
				 old_dir = Some (!test_env_directory ^
						 "Output_C_on_HPUX/Acceptance/Prof/")}

      fun acceptance_strategy_SUN_OS4_C () = 
	    ACCEPTANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_suspension,
				 strategy_name = "NoProf", (* single word *)
				 comment = "Region profiling not enabled.",
				 exec_opt = "",
				 old_dir = Some (!test_env_directory ^
						 "Output_C_on_SUN_OS4/Acceptance/NoProf/")}
      
      fun acceptance_strategy_SUN_OS4_C_prof () = 
	    ACCEPTANCE_STRATEGY {kit_script = "prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_prof_suspension,
				 strategy_name = "Prof", (* single word *)
				 comment = "Region profiling enabled. A profile datafile is not exported. \
				  \ Profiling every 30 second.",
				 exec_opt = "-sec 30 -noDatafile -noStat",
				 old_dir = Some (!test_env_directory ^
						 "Output_C_on_SUN_OS4/Acceptance/Prof/")}
    in
      fun acceptance_strategies () = (*USER*)
	    (case !kit_version of
	       "ML_to_C_on_HPUX" => [acceptance_strategy_HPUX_C (),
				     acceptance_strategy_HPUX_C_prof ()]
	     | "ML_to_C_on_SUN_OS4" => [acceptance_strategy_SUN_OS4_C ()]
	     | "ML_to_HPPA_on_HPUX" => [acceptance_strategy_HPUX_HPPA ()]
	     | _ => [])
    end (*local*)

    (* Test programs, located in directory Sources, can be added to this list. *) 
    val acceptance_suite_files = [
(*KILL 20/06/1997 19:45. tho.:
				  ("kitfib35",None),
*)
				  ("kitreynolds2",None),
				  ("kitreynolds3",None), 
				  ("kitdangle",None),
				  ("kitdangle3",None),
				  ("kitloop2",None), 
				  ("kittmergesort",None), 
				  ("kitqsort36c",None), (*new 31/03/1997 15:28. tho.*)
				  ("kitmandelbrot",None),
				  ("kitlife35u",None), (*new 31/03/1997 15:28. tho.*) 
(*KILL 20/06/1997 19:44. tho.:
				  ("klife_eq",None),
				  ("kitkbjul9",None), 
*)
				  ("kkb_eq",None), 
				  ("kitknuth_bendix36c",None) (*new 31/03/1997 15:28. tho.*) 
(*KILL 20/06/1997 19:44. tho.:
				  , ("kitsimple",None)
*)
				  ] (*--USER--*)

    val acceptance_suite_projects = [
				     ("FuhMishra",None),
				     ("life",None),
				     ("compose",None),
				     ("minilist",None),
(*KILL 20/06/1997 19:45. tho.:
				     ("sma",None),
				     ("fromto",None),
				     ("nlength10000",None),
*)
				     ("scan",Some "../Sources/scanfiles"),
				     ("effect", None),
				     ("exceptions",None),
				     ("hello",None),
				     ("proj",None),
				     ("tail",None),
				     ("refs",None),
				     ("trees",None),
				     ("fold",None),
				     ("testdyn1",None),
				     ("testdyn2",Some "../Sources/input_to_testdyn2")
(*KILL 20/06/1997 19:45. tho.:
				     , ("scan_rev1", None),
				     ("scan_rev2", None)
*)
				     ]

    (*--------------------------------------------*)
    (* Variables controlling the PERFORMANCE test *)
    (*--------------------------------------------*)

    local
      (* These strategies can be changed, deleted or new inserted. *) 
      val show_compiler_timings = true
      fun performance_strategy_HPUX () = 
	    PERFORMANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				  runtime_system = path_to_runtime_suspension, 
				  strategy_name = "NoProf",
				  show_compiler_timings = show_compiler_timings,
				  comment = ("Region profiling not enabled.")}

      fun performance_strategy_SUN_OS4 () = 
	    PERFORMANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				  runtime_system = path_to_runtime_suspension, 
				  strategy_name = "NoProf",
				  show_compiler_timings = show_compiler_timings,
				  comment = ("Region profiling not enabled.")}
    in
      fun performance_strategies () = (*--USER--*)
	    (case !kit_version of
	       "ML_to_C_on_HPUX" => [performance_strategy_HPUX ()]
	     | "ML_to_C_on_SUN_OS4" => [performance_strategy_SUN_OS4 ()]
	     | "ML_to_HPPA_on_HPUX" => [
(*KILL 20/06/1997 19:46. tho.:
					performance_strategy_HPUX ()
*)
					]
	     | _ => [])
    end (*local*)

    (* Test programs, located in directory Sources, can be added to this list. *) 
    val performance_suite_files = [
(*KILL 20/06/1997 19:47. tho.:
				   ("kitfib35",None),
				   ("kitreynolds2",None),
				   ("kitreynolds3",None),
				   ("kitdangle",None),
				   ("kitdangle3",None),
				   ("kitloop2",None),
				   ("kittmergesort",None),
				   ("kitqsort36c",None), (*new 31/03/1997 15:28. tho.*)
				   ("kitmandelbrot",None),
				   ("kitlife35u",None), (*new 31/03/1997 15:28. tho.*) 
				   ("klife_eq",None), 
				   ("kitkbjul9",None),
				   ("kkb_eq",None),
				   ("kitknuth_bendix36c",None), (*new 31/03/1997 15:28. tho.*)
				   ("kitsimple",None)
*)
				   ] (*--USER--*)

    val performance_suite_projects = [
(*KILL 20/06/1997 19:47. tho.:
				      ("msort", None)
*)
				      ]

  end (*functor TestInfo*)


