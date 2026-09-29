(* Explicit snapshots require -region_profile -no_gc and runtime -rp.
 * With no enabled session these operations are no-ops. Start/pause take a
 * boundary snapshot only on a state transition; sample works while paused.
 * M1 does not provide periodic sampling or sampling across C callbacks. *)
signature REGION_PROFILE =
sig
  val start : unit -> unit
  val pause : unit -> unit
  val sample : unit -> unit
  val mark : string -> unit
  val flush : unit -> unit
end
