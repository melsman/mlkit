(*
 * service.sml
 *
 * Maintains per-connection partial prefixes until a message is complete.
 * A message is considered complete when two newlines: "\n\n", are found.

 * When a message completes, it is printed to stdout and removed from the
 * service state.
 *)

structure Service : SERVICE =
struct
  type conn = int
  type serviceState = (conn * string) list

  fun emptySs () : serviceState = []

  (* removeConn removes the entry for connection c from the state, if it exists *)
  fun removeConn (_: conn, []: serviceState) = []
    | removeConn (c, (c',buf)::rest) =
        if c = c' then rest else (c',buf)::removeConn (c, rest)

  (* lookupConn looks up the buffer for connection c in the state, returning
	 SOME buf if found, or NONE if not found *)
  fun lookupConn (_: conn, []: serviceState) = NONE
    | lookupConn (c, (c',buf)::rest) =
        if c = c' then SOME buf else lookupConn (c, rest)

  (* addConn adds chunk to the buffer for connection c, or creates a new entry
	 if c is not in the state *)
  fun addConn (c, chunk, ss) =
	  let
		fun appendChunk (_: conn, _: string, []: serviceState) = []
		  | appendChunk (c, chunk, (c',buf)::rest) =
			if c = c' then (c, buf ^ chunk)::rest else (c',buf)::appendChunk (c, chunk, rest)
	  in
		case lookupConn (c, ss) of
		  SOME _ => appendChunk (c, chunk, ss)
		| NONE => (c, chunk) :: ss
	  end

  (* Check if string s contains "\n\n". Returns SOME s', where s' is the string
	 up untill "\n\n", and NONE otherwise *)
  fun findEnd (s:string) : string option =
	let
	  fun go (i:int) =
		if i >= String.size s - 1 then NONE
		else if String.substring (s, i, 2) = "\n\n" then
			SOME (String.substring (s, 0, i + 2))
		else go (i + 1)
	in
	  go 0
	end

  fun copySs [] = []
	| copySs ((c, buf)::rest) = (c, buf ^ "") :: copySs rest

  fun timeToGC`[r1 r2 r3] (ss : (conn * string`r1)`r3 list`r2) : bool =
	  let val total = Region.memoryUsageOfRegion `[r1] ()
					+ Region.memoryUsageOfRegion `[r2] ()
					+ Region.memoryUsageOfRegion `[r3] ()
		  val live = Size.size (Size.list (Size.tup2 Size.int Size.string))  ss
	  in live * 10 < total
	  end
  (* fun timeToGC ss = false *)

  (* fun timeToGC `[r1 r2 r3] (ss : (conn * string`r1)`r3 list`r2) : bool = *)
  (* 	  let val total = Region.memoryUsageOfRegion `[r1] () *)
  (* 					+ Region.memoryUsageOfRegion `[r2] () *)
  (* 					+ Region.memoryUsageOfRegion `[r3] () *)
  (* 	  in if total < 1000000 then false *)
  (*        else *)
  (* 		   let val live = Size.size (Size.list (Size.tup2 Size.int Size.string))  ss *)
  (* 		   in live * 4 < total *)
  (* 		   end *)
  (* 	  end *)

  fun service (ss, c, chunk) =
	let val tempState = addConn (c, chunk, ss)
	 in
	  case lookupConn (c, tempState) of
		SOME buf =>
		  (case findEnd buf of
			 SOME s =>
				(print ("msg is: \n" ^ s);
				(SOME c, removeConn (c, tempState)))
		   | NONE => (NONE, tempState))
	  | NONE => (NONE, tempState)
	end
end
