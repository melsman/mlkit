
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [X86 Backend] **\n\n";
	 CM.make());

structure K = KitX86()

local
  fun enable s = K.Flags.turn_on s
  fun disable s = K.Flags.turn_off s
  fun enable_disable enables disables f =
    (app enable enables; app disable disables; f();
     app disable enables; app enable disables)

  fun comp_GC() =
    enable_disable 
    ["garbage_collection", "scratch"] 
    ["region_inference"] 
    K.build_basislib

  fun comp_RI_GC() =
    enable_disable 
    ["garbage_collection"] 
    []
    K.build_basislib

  fun comp_RI() = K.build_basislib()

  fun comp_RI_PROF() = 
    enable_disable 
    ["region_profiling", "log_to_file", "print_all_program_points",
     "print_region_flow_graph", "print_call_explicit_expression"]
    []
    K.build_basislib

  fun comp_RI_GC_PROF() = 
    enable_disable 
    ["region_profiling", "garbage_collection"]
    []
    K.build_basislib
in

val _ = (enable "print_types"; enable "print_rho_types")
(*
           disable "import_basislib";
	   enable "print_types";
	   enable "print_rho_levels";
	   enable "print_rho_types";
	   enable "debug_compiler";
*)

  val _ = comp_RI_GC()

  val _ = comp_RI()
(*  val _ = comp_GC() *)

  val _ = comp_RI_GC_PROF()
  val _ = comp_RI_PROF()

  val _ = K.install()

end
