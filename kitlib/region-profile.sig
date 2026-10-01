(* Snapshots require -region_profile and runtime -rp.
 * With no enabled session these operations are no-ops. Start/pause take a
 * boundary snapshot only on a state transition; sample works while paused.
 * GC plus parallelism and explicit sampling across C callbacks are unsupported.
 * Timed requests wait for safe ML points; see doc/region-profiler.md. *)
signature REGION_PROFILE =
sig
  val start : unit -> unit
  val pause : unit -> unit
  val sample : unit -> unit
  val mark : string -> unit
  val flush : unit -> unit
end
