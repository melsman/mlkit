signature SERVICE =
sig
  type serviceState
  type conn = int
  val service : serviceState * conn * string -> conn option * serviceState
  val emptySs : unit -> serviceState
  val timeToGC : serviceState -> bool
  val copySs : serviceState -> serviceState
en

signature DRIVER =
sig
  val run : string -> unit
end

functor Driver (structure TcpState: TCP_STATE
				structure Service: SERVICE) :> DRIVER =
struct
  open TcpState
  open SimpleIO

  exception EOF

  (* read line from file. (msgID, packetID, data) *)
  fun read is =
	  case inputLine is of
	    NONE => NONE
	  | SOME line => 
		  case String.fields (fn c => c = #";") line of
			(msgID::pkID::rest) => (
			  case (Int.fromString msgID, Int.fromString pkID) of
				(SOME msgID', SOME pkID') => SOME
				(msgID', (pkID', String.concatWith ";" rest))
			  | (_, _) => NONE
			)
		  | _ => NONE

  fun loop is (arg as (state, ss)) =
	  let val arg' = 
		case read is of
		  NONE => raise EOF
		| SOME (msgID, packet) =>
			  case insert (msgID, packet, state) of
				 NONE => arg
			   | SOME state1 =>
				(case extract state1 of
				   NONE => (print ("state size: " ^ Int.toString (stateSize state1) ^ " bytes\n");
							(state1, ss))
				  | SOME (msgID, msg, state2) =>
					 let
					   val msg' = msg ^ ""
					   val (connOpt, ss') = Service.service (ss, msgID, msg')

					   (* Double copy GC on the assemlber state *)
					   (* val state' = if stateSize state2 > 10000 then *)
					   (* 				  let val temp = copyState state2 *)
					   (* 					  val _ = forceResetting state2 *)
					   (* 					  val _ = forceResetting state *)
					   (* 					val _ = print ("state size: " ^ Int.toString (stateSize state2) ^ " bytes\n") *)
					   (* 				  in copyState temp *)
					   (* 				  end *)
                       (*              else state2 *)
					   (* val _ = print ("state size: " ^ Int.toString (stateSize state') ^ " bytes\n") *)
					   val temp = copyState state2
					   val _ = forceResetting state
					   val state' = copyState temp
					   val state'' =
						 case connOpt of
						   NONE => state'
						 | SOME conn => closeConn (conn, state')
					   (* val _ = print ("state size: " ^ Int.toString (stateSize state'') ^ " bytes\n")  *)

					   (* Double copy GC on the service state *)
					   val ss'' = if Service.timeToGC ss' then
									let val temp = Service.copySs ss'
									  val _ = forceResetting ss'
									in Service.copySs temp
									end
								  else ss'
					 in
					   (state'', ss'')
					 end
			  )
	  in loop is arg'
  	  end
			

  fun run file =
	  let
		val is = openIn file
		val state = emptyState ()
		val serviceState = Service.emptySs ()
	  in
		(loop is (state, serviceState) handle EOF => (
		  closeIn is;
		  print ("Service loop exited\n")
		)
		)
	  end
end

