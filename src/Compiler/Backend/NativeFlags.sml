(* Shared by the X64 and ARM64 pipelines; initialized before region analysis. *)
structure NativeFlags =
struct
  fun regionOption (name,short,item,neg,desc) =
    Flags.add_bool_entry
      {long = name, short = short, item = item, neg = neg,
       menu = ["Control Region Analyses",name], desc = desc}

  val regionInference = regionOption
    ("region_inference",SOME "ri",ref true,true,
     "With this flag disabled, all values are allocated in global regions.")
  val regionProfile = regionOption
    ("region_profile",SOME "rp",RegionProfiling.enabled,false,
     "Emit packed object descriptors, IR metadata and safe-point polls for rpview.")
  val printRegionFlowGraph = regionOption
    ("print_region_flow_graph",SOME "Prfg",ref false,false,
     "Print the region flow graph as text.")
  val printAllProgramPoints = regionOption
    ("print_all_program_points",SOME "Ppp",ref false,false,
     "Print all program points when printing physical size inference expressions.")

  fun runtimeBool (name,item) =
    Flags.add_bool_entry
      {long = name, short = NONE, neg = false, item = item,
       menu = ["REPL",name], desc = "Forward profiler option to the REPL runtime."}
  fun runtimeString (name,item) =
    Flags.add_string_entry
      {long = name, short = NONE, item = item,
       menu = ["REPL",name], desc = "Forward profiler option to the REPL runtime."}
  val _ = runtimeBool ("rp_paused",RegionProfiling.paused)
  val _ = runtimeBool ("rp_report",RegionProfiling.report)
  val _ = runtimeBool ("rp_gc_samples",RegionProfiling.gcSamples)
  val _ = runtimeString ("rp_file",RegionProfiling.file)
  val _ = runtimeString ("rp_interval",RegionProfiling.interval)
end
