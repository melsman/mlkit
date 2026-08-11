structure TcpState :> TCP_STATE =
struct
	
  type conn = int
  type pkID = int
  type packet = pkID * string

  type assembler =
	{prefix: packet list,
	 bag: packet list,
	 cPkID: int}

  datatype state = St of (conn * {prefix: (pkID * string) list, bag: (pkID  * string) list, cPkID: int}) list

  (* state helper functions *)
  fun emptyState () : state = St []

  fun mapState f (St lst) =
	  let fun go [] acc = List.rev acc
			| go ((c, a) :: rest) acc = go rest (f (c, a) :: acc)
	  in St (go lst [])
	  end

  fun foldlState f acc (St lst) = List.foldl f acc lst
  fun findState f (St lst) = List.find f lst
  fun prependState (St lst) (conn, assembler) = St ((conn, assembler) :: lst)


  (* inserts elm into sorted list according to f *)
  fun insertInList f elm lst =
	  let fun go [] acc = List.rev (elm :: acc)
			| go (x :: xs) acc =
			  if f (elm, x) then List.revAppend (acc, elm :: x :: xs)
			  else go xs (x :: acc)
	  in go lst []
	  end

  (* updatePrefix updates the prefix of an assembler by moving packets from the
	 bag to the prefix if they fit *)
  fun updatePrefix (assembler: assembler) : assembler =
	let
	  fun go {prefix, bag, cPkID} =
		case bag of
		  [] => {prefix=prefix, bag=[], cPkID=cPkID}
		| (p :: ps) =>
		  if #1 p = cPkID then
			go {prefix = p :: prefix, bag=ps, cPkID=cPkID + 1}
		  else
			{prefix=prefix, bag=bag, cPkID=cPkID}
	in
	   go assembler
	end

  fun insertPacket (conn: conn) (packet: packet) (state: state) : state =
	case findState (fn (c, _) => c = conn) state of
	  SOME (c, {prefix, bag, cPkID}) =>
		let
		  val bag' = insertInList (fn ((offset1, _), (offset2, _)) => offset1 < offset2) packet bag 
		  val assembler' = updatePrefix {prefix=prefix, bag=bag', cPkID=cPkID}
		in
		  mapState (fn (c', a) => if c' = conn then (c', assembler') else (c', a)) state
		end
	| NONE =>
        let
		  val bag' = [packet]
		  val assembler' = updatePrefix {prefix=[], bag=bag', cPkID=0}
		in
		  prependState state (conn, assembler')
		end

  (* inserts a new packet into the state, returning SOME updated state
	 This function can in the future return NONE on error, e.g. if the state is
	 full *)
  fun insert (conn, packet, state) =
	SOME (insertPacket conn packet state)

  (* find the first assembler with a non-empty prefix, return its conn, the full message, and the updated state *)
  fun extract state =
	case findState (fn (_, {prefix, bag, cPkID}) => prefix <> []) state of
	  SOME (conn, {prefix, bag, cPkID}) =>
		let
		  val msg = String.concat (List.map (fn (_, s) => s) (List.rev prefix))
		  val state' =
			mapState (
			  fn (c, {prefix, bag, cPkID}) =>
				if c = conn then
				  (c, {prefix=[], bag=bag, cPkID=cPkID})
				else (c, {prefix=prefix, bag=bag, cPkID=cPkID})
			) state
		in
		  SOME (conn, msg, state')
		end
	| NONE => NONE

  fun closeConn (conn, state) =
	mapState (
	  fn (c, {prefix, bag, cPkID}) =>
		if c = conn then
		  (c, {prefix=[], bag=[], cPkID=0})
		else (c, {prefix=prefix, bag=bag, cPkID=cPkID})
	) state


  fun copyPacketList lst =
	  List.rev (List.foldl (fn ((pkID, s), acc) => (pkID, s ^ "") :: acc) [] lst)

  fun stateSize `[r1 r2 r3] (St (lst : (int * {prefix: (int * string`r1)`r2 list`r2,
											   bag: (int * string`r1)`r2 list`r2,
											   cPkID: int}`r3
									   ) list`r2)) : int =
	  Region.memoryUsageOfRegion `[r1] ()
	+ Region.memoryUsageOfRegion `[r2] ()
	+ Region.memoryUsageOfRegion `[r3] ()

  fun copyState state =
	mapState (
	  fn (c, {prefix, bag, cPkID}) =>
		  (c, {prefix = copyPacketList prefix,
			   bag = copyPacketList bag,
			   cPkID = cPkID})
	) state

  fun gcState state =
	resetRegions state;
end
	
