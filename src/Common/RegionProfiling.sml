(* Internal configuration shared with the repository and manager. Only the
 * native backend registers command-line options against these references. *)
structure RegionProfiling =
struct
  val enabled = ref false
  val timeEnabled = ref false
  val allEnabled = ref false
  val allPaused = ref false
  val paused = ref false
  val report = ref false
  val gcSamples = ref false
  val file = ref "profile.prof"
  val interval = ref "10ms"
end
