
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [X86 Backend] **\n\n";
	 CM.make());

structure K = KitX86()

local
  fun enable s = K.Flags.turn_on s
  fun disable s =  K.Flags.turn_off s
(*
  val _ = enable "debug_compiler"
  val _ = disable "import_basislib"
*)
in
  val _ = (
(*	   
	   enable "print_types";
	   enable "print_rho_levels";
	   enable "print_rho_types";
	   enable "debug_compiler";
*)

	   enable "garbage_collection";

	   disable "region_inference";
	   enable "scratch";
	   K.build_basislib();
	   disable "scratch";
	   enable "region_inference";

	   K.build_basislib();

	   disable "garbage_collection";
	   K.build_basislib();

	   disable "garbage_collection";
	   enable "region_profiling"; 
	   enable "log_to_file";
	   enable "print_all_program_points";
	   enable "print_region_flow_graph";	   
	   enable "print_call_explicit_expression";
	   K.build_basislib();
	   disable "print_call_explicit_expression";
	   disable "print_region_flow_graph";	   
	   disable "print_all_program_points";
	   disable "log_to_file";

	   enable "garbage_collection";
	   enable "region_profiling"; 
	   K.build_basislib();

           disable "garbage_collection";
	   disable "region_profiling"; 

	   K.install()
)
end
