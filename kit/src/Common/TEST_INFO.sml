
(*The information used by TestEnv to test the kit*)
signature TEST_INFO =
  sig
    (*-----------------------------------------------------------*)
    (* Variables controlling the ACCEPTANCE and PERFORMANCE test *)
    (*-----------------------------------------------------------*)
    datatype strategy = ACCEPTANCE_STRATEGY of {kit_script     : string, 
						runtime_system : unit -> string,
						strategy_name  : string, (* single word *)
						comment        : string,
						exec_opt       : string,
						old_dir        : string option}
                      | PERFORMANCE_STRATEGY of {kit_script : string,
						 runtime_system : unit -> string,
						 strategy_name : string,
						 show_compiler_timings : bool,
						 comment : string}
      
    val acceptance_strategies  : unit -> strategy list
    val performance_strategies : unit -> strategy list

    val acceptance_suite_files     : unit -> (string*string option) list
    val acceptance_suite_projects  : unit -> (string*string option) list
    val performance_suite_files    : (string*string option) list
    val performance_suite_projects : (string*string option) list

    (*The reason some values are suspended (e.g., the `runtime_system' field)
     is that they depend on ref's from Flags that may be updated, and we want
     TestEnv to be sensitive to these updates.

     When the dynamic flag "quicker_acceptance_test" is on,
     the two suites of test programs (acceptance_suite_projects &
     acceptance_suite_files) are smaller and there are fewer
     acceptance strategies (acceptance_strategies).*)
  end
