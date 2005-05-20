

val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [X86 Backend] **\n\n";
	 CM.make' "Compiler/native.cm");

(*
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler with SML-NJ profiling enabled [X86 Backend] **\n\n";
         Compiler.Profile.setProfMode true;
	 CM.make' "Compiler/native.cm");
*)

local
  fun enable_disable enables disables f =
    (app Main.enable enables; app Main.disable disables; f();
     app Main.disable enables; app Main.enable disables)

  fun comp_nodangle() =
    enable_disable 
    ["dangling_pointers_statistics", "scratch"] 
    ["dangling_pointers"] 
    Main.build_basislib      

  fun comp_contractregions() =
    enable_disable 
    ["contract_regions", "scratch"] 
    [] 
    Main.build_basislib      

  fun comp_nouncurry() =
    enable_disable 
    ["scratch"] 
    ["uncurry"] 
    Main.build_basislib            

  fun comp_GC() =
    enable_disable 
    ["garbage_collection", "scratch"] 
    ["region_inference"] 
    Main.build_basislib

  fun comp_RI_GC() =
    enable_disable 
    ["garbage_collection"] 
    []
    Main.build_basislib

  fun comp_RI_no_opt() =
    enable_disable 
    [] 
    ["opt"]
    Main.build_basislib

  fun comp_RI_GenGC() =
    enable_disable 
    ["generational_garbage_collection", "garbage_collection"] 
    []
    Main.build_basislib

  fun comp_RI_GC_TP() =
    enable_disable 
    ["garbage_collection", "tag_pairs"] 
    []
    Main.build_basislib

  fun comp_RI() = Main.build_basislib()

  fun comp_RI_PROF() = 
    enable_disable 
    ["region_profiling", "log_to_file", "print_all_program_points",
     "print_region_flow_graph", "print_call_explicit_expression"]
    []
    Main.build_basislib

  fun comp_RI_GC_PROF() = 
    enable_disable 
    ["region_profiling", "garbage_collection"]
    []
    Main.build_basislib

  fun comp_RI_GenGC_PROF() = 
    enable_disable 
    ["region_profiling", "generational_garbage_collection", "garbage_collection"]
    []
    Main.build_basislib

  fun comp_RI_GC_TP_PROF() = 
    enable_disable 
    ["region_profiling", "garbage_collection", "tag_pairs"]
    []
    Main.build_basislib
in
(*  val _ = (enable "print_types"; enable "print_rho_types") *)

(*
           disable "import_basislib";
	   enable "print_types";
	   enable "print_rho_levels";
	   enable "print_rho_types";
	   enable "debug_compiler";
*)

(*  val _ = comp_nodangle() *)

(*  val _ = comp_GC() *)

(*  val _ = comp_RI_GC_TP() *)

(*  val _ = comp_RI_GC_TP_PROF() *)

(*  val _ = comp_nouncurry() *)

(*  val _ = comp_contractregions() *)

  val _ = comp_RI()

(*
  val _ = comp_RI_GC()
  val _ = comp_RI_no_opt()
  val _ = comp_RI_PROF()
  val _ = comp_RI_GC_PROF()  
*)
(*
  val _ = comp_RI_GenGC()
*)
(*
  val _ = comp_RI_GenGC_PROF()
*)
  val _ = Main.install()

end
