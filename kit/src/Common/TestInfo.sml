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
						old_dir        : string option}
                      | PERFORMANCE_STRATEGY of {kit_script            : string,
						 runtime_system        : unit -> string,
						 strategy_name         : string,
						 show_compiler_timings : bool,
						 comment               : string}

    fun path_to_runtime_suspension () = !(Flags.lookup_string_entry "path_to_runtime")
    fun path_to_runtime_prof_suspension () = !(Flags.lookup_string_entry "path_to_runtime_prof")
    val test_env_directory = Flags.lookup_string_entry "test_env_directory"
    val kit_version = Flags.lookup_string_entry "kit_version"
    val _ = Flags.add_flag_to_menu
              (["Test environment"],
	       "quicker_acceptance_test", "quicker acceptance test", ref true)
    val quicker_acceptance_test = Flags.lookup_flag_entry "quicker_acceptance_test"
    fun leave_out_if_quicker xs = if !quicker_acceptance_test then [] else xs

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
				 old_dir = SOME (OS.Path.concat(!test_env_directory,
								"Output_HPPA_on_HPUX/Acceptance/NoProf/"))}
      
      fun acceptance_strategy_HPUX_HPPA_prof () = 
	    ACCEPTANCE_STRATEGY {kit_script = "prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_prof_suspension,
				 strategy_name = "Prof", (* single word *)
				 comment = "Region profiling enabled. A profile datafile is not exported. \
				  \ Profiling every 500000 microsec.",
				 exec_opt = "-microsec 500000 -noDatafile -noStat",
				 old_dir = SOME (OS.Path.concat(!test_env_directory,
								"Output_HPPA_on_HPUX/Acceptance/Prof/"))}

      fun acceptance_strategy_HPUX_C () = 
	    ACCEPTANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_suspension,
				 strategy_name = "NoProf", (* single word *)
				 comment = "Region profiling not enabled.",
				 exec_opt = "",
				 old_dir = SOME (OS.Path.concat(!test_env_directory,
								"Output_C_on_HPUX/Acceptance/NoProf/"))}
      
      fun acceptance_strategy_HPUX_C_prof () = 
	    ACCEPTANCE_STRATEGY {kit_script = "prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_prof_suspension,
				 strategy_name = "Prof", (* single word *)
				 comment = "Region profiling enabled. A profile datafile is not exported. \
				  \ Profiling every 4 second.",
				 exec_opt = "-sec 4 -noDatafile -noStat",
				 old_dir = SOME (OS.Path.concat(!test_env_directory,
								"Output_C_on_HPUX/Acceptance/Prof/"))}

      fun acceptance_strategy_SUN_OS4_C () = 
	    ACCEPTANCE_STRATEGY {kit_script = "no_prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_suspension,
				 strategy_name = "NoProf", (* single word *)
				 comment = "Region profiling not enabled.",
				 exec_opt = "",
				 old_dir = SOME (OS.Path.concat(!test_env_directory,
								"Output_C_on_SUN_OS4/Acceptance/NoProf/"))}
      
      fun acceptance_strategy_SUN_OS4_C_prof () = 
	    ACCEPTANCE_STRATEGY {kit_script = "prof.script", (*--USER--*)
				 runtime_system = path_to_runtime_prof_suspension,
				 strategy_name = "Prof", (* single word *)
				 comment = "Region profiling enabled. A profile datafile is not exported. \
				  \ Profiling every 30 second.",
				 exec_opt = "-sec 30 -noDatafile -noStat",
				 old_dir = SOME (OS.Path.concat(!test_env_directory,
								"Output_C_on_SUN_OS4/Acceptance/Prof/"))}
    in
      fun acceptance_strategies () = (*USER*)
	    (case !kit_version of
	       "ML_to_C_on_HPUX" => [acceptance_strategy_HPUX_C ()]
		 @ leave_out_if_quicker [acceptance_strategy_HPUX_C_prof ()]
	     | "ML_to_C_on_SUN_OS4" => [acceptance_strategy_SUN_OS4_C ()]
	     | "ML_to_HPPA_on_HPUX" => [acceptance_strategy_HPUX_HPPA ()]
		 @ leave_out_if_quicker [acceptance_strategy_HPUX_HPPA_prof ()]
	     | _ => [])
    end (*local*)

    (* Test programs, located in directory Sources, can be added to this list. *) 
    fun acceptance_suite_files () =
          [("kitfib12.sml",NONE),
	   ("kitdangle.sml",NONE)] (*--USER--*)

    fun acceptance_suite_projects () =
          [("kitreynolds2_fast.pm",NONE),
	   ("kitreynolds3_fast.pm",NONE),
	   ("kitloop2.pm",NONE), 
	   ("kittmergesort.pm",NONE), 
	   ("kitqsort36c.pm",NONE),
	   ("kitmandelbrot_fast.pm",NONE)]
	@ leave_out_if_quicker
	  [("kitlife35u.pm",NONE)]
	@ [("klife_eq_fast.pm",NONE),
	   ("kitkbjul9_fast.pm",NONE)]
        @ leave_out_if_quicker
	  [("kkb_eq.pm",NONE),
	   ("kitknuth_bendix36c.pm",NONE)]
	@ [("kitsimple.pm",NONE),
	   ("tststrcmp.pm",NONE),
	   ("FuhMishra.pm",NONE),
	   ("life.pm",NONE)]
	@ leave_out_if_quicker
	  [("compose.pm",NONE),
	   ("minilist.pm",NONE),
	   ("sma.pm",NONE),
	   ("fromto.pm",NONE)]
	@ [("nlength10000.pm",NONE),
	   ("scan.pm",SOME "../Sources/scanfiles"),
	   ("effect.pm", NONE)]
	@ leave_out_if_quicker
	  [("exceptions.pm",NONE),
	   ("hello.pm",NONE),
	   ("proj.pm",NONE),
	   ("tail.pm",NONE),
	   ("refs.pm",NONE),
	   ("trees.pm",NONE),
	   ("fold.pm",NONE)]
	@ [("testdyn1.pm",NONE),
	   ("testdyn2.pm",SOME "../Sources/input_to_testdyn2")]
	@ leave_out_if_quicker
	  [("scan_rev1.pm", NONE),
	   ("scan_rev2.pm", NONE),
	   ("fft.pm", NONE)]

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
      fun tic98_Unbox_Untag () =
	    PERFORMANCE_STRATEGY {kit_script = "tic98_Unbox_Untag.script", (*--USER--*)
				  runtime_system = path_to_runtime_suspension, 
				  strategy_name = "tic98_Unbox_Untag",
				  show_compiler_timings = false,
				  comment = "Lists are unboxed. Values are not tagged."}
      fun tic98_Box_Untag () =
	    PERFORMANCE_STRATEGY {kit_script = "tic98_Box_Untag.script", (*--USER--*)
				  runtime_system = path_to_runtime_suspension, 
				  strategy_name = "tic98_Box_Untag",
				  show_compiler_timings = false,
				  comment = "Lists are boxed. Values are not tagged."}
      fun tic98_Box_Tag () =
	    PERFORMANCE_STRATEGY {kit_script = "tic98_Box_Tag.script", (*--USER--*)
				  runtime_system = path_to_runtime_suspension, 
				  strategy_name = "tic98_Box_Tag",
				  show_compiler_timings = false,
				  comment = "Lists are boxed. Values are Tagged. Equality elimination is disabled."}
    in
      fun performance_strategies () = (*--USER--*)
	    (case !kit_version of
	       "ML_to_C_on_HPUX" => [performance_strategy_HPUX ()]
	     | "ML_to_C_on_SUN_OS4" => [performance_strategy_SUN_OS4 ()]
	     | "ML_to_HPPA_on_HPUX" => [performance_strategy_HPUX () 
					(*tic98_Box_Tag(), tic98_Box_Untag(), tic98_Unbox_Untag()*)]
	     | _ => [])
    end (*local*)

    (* Test programs, located in directory Sources, can be added to this list. *) 
    val performance_suite_files =
          [("kitfib35.sml",NONE),
	   ("kitdangle.sml",NONE),
	   ("kitdangle3.sml",NONE)]  (*--USER--*)

(*      [("tic98fib.sml", NONE),("tic98sieve.sml", NONE),("tic98life.sml", NONE),("tic98lifem.sml", NONE),
       ("tic98msort.sml", NONE),("tic98mbrot.sml", NONE),("tic98kkb.sml", NONE),("tic98simpl.sml", NONE)] *)

    val performance_suite_projects = 
          [("kitreynolds2.pm",NONE),
	   ("kitreynolds3.pm",NONE),
	   ("kitloop2.pm",NONE),
	   ("kittmergesort.pm",NONE),
	   ("kitqsort36c.pm",NONE),
	   ("kitmandelbrot.pm",NONE),
	   ("kitlife35u.pm",NONE),
	   ("klife_eq.pm",NONE), 
	   ("kitkbjul9.pm",NONE),
	   ("kkb_eq.pm",NONE),
	   ("kitknuth_bendix36c.pm",NONE),
	   ("kitsimple.pm",NONE),
	   ("fft.pm",NONE),
	   ("msort.pm", NONE)]

  end (*functor TestInfo*)


