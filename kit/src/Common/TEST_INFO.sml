(*$TEST_INFO*)

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
						old_dir        : string Option}
                      | PERFORMANCE_STRATEGY of {kit_script : string,
						 runtime_system : unit -> string,
						 strategy_name : string,
						 show_compiler_timings : bool,
						 comment : string}
      
    val acceptance_strategies  : unit -> strategy list
    val performance_strategies : unit -> strategy list

    val acceptance_suite_files     : (string*string Option) list
    val acceptance_suite_projects  : (string*string Option) list
    val performance_suite_files    : (string*string Option) list
    val performance_suite_projects : (string*string Option) list

    (*The reason some values are suspended (e.g., the `runtime_system' field)
     is that they depend on ref's from Flags that may be updated, and we want
     TestEnv to be sensitive to these updates.*)
  end
