(* FragAssembler.sig
 *
 * Fragment assembler / reassembler for out-of-order packets.
 *)
signature TCP_STATE =
sig
  type conn  = int
  type pkID = int
  type packet = pkID * string


  type state

  val emptyState : unit -> state


  (* Insert a packet into the global state.
	 If its is succesful, returns the new state, else return NONE
   *)
  val insert : conn * packet * state -> state option

  (* extracts a prefix from the state *)
  val extract : state -> (conn * string * state) option

  (* closes a message by removing it from the state, if it exists. Returns the new state. *)
  val closeConn : conn * state -> state


  (* manual garbage collection *)
  val stateSize : state -> int
  val copyState : state -> state (* Must be exomorph, so it is copied to new regions *)
  val gcState : state -> unit
end
