(* Experimental wall-time sampling: compile with -rp, run with +RTS -tp.
 * Single-thread macOS ARM64 static executables only. Operations are no-ops
 * without an enabled session. They never trigger region snapshots. *)
signature TIME_PROFILE =
sig
  val start : unit -> unit
  val pause : unit -> unit
  val flush : unit -> unit
end
