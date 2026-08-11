(* A mock server implementation *)
(* Using regions to make sure we can reset state between requests *)
(* and that we can reset request data after each request *)

open SimpleIO

(* Define the type of subservice *)
datatype subservice =
    Exit
  | Nothing
  | Chat
  | History

type state = string list

(* Take up to n elements from the list l *)
(* Returns the whole list if n > length l *)
(* \/ a,r. (list`r a, int) -> list`r a *)
fun takeSafe (l, n) =
	let
	  val len = List.length l
	in
	  if n < len then
		List.take (l, n)
	  else
	  	l
	end

(* Copy a list of strings to a new region *)
(* \/ r,r'. list`r string -> list`r' string *)
fun copyList [] = []
  | copyList (x::xr) = (x ^ "") :: copyList xr

(* Copy a string to a new region *)
(* \/ r,r'. string`r -> string`r' *)
fun copyString `[r1 r2] (s : string`r1) : string`r2 = s ^ ""

(* Mock request function *)
(* Reads data from a file, which contains all simulated network data *)
(* Splits the data into service type and actual data, delimiter is `:` *)
(* \/ r.  -> string`r *)
fun req `r (inStream : instream) : subservice * string`r =
	let with r1
	  val line : string`r1 option = inputLine inStream 
	in
	  case line of
		SOME l => (
		  	case String.fields (fn c => c = #":") l of
			  ("exit"::rest) => (Exit, "")
			| ("chat"::rest) => (Chat, String.concatWith ":" rest)
			| ("history"::rest) => (History, "")
			| _ => (Nothing, "")
		)
	  | NONE =>
		    (Exit, "")
	end

(* Mock response function *)
(* Sends data to the client by printing it to the console *)
(* \/ r. string`r ->  unit *)
fun resp `r (data: string`r) : unit =
	print ("Response sent to client: " ^ data)

local
  (* chat subservice *)
  (* appends data to state and returns a response *)
  (* \/ r,r',r'',r'''. (string`r' list`r, string`r'') -> (string`r''', string`r' list`r) *)
  fun chat `[rs1 rs2] (state : string`rs1 list`rs2, data: string) : string * string`rs1 list`rs2 =
	  let
		val temp = data ^ "" :: copyList state
		val _ = resetRegions `[rs1 rs2] ()
		val response = "Chat received: " ^ data
		val newState = copyList (takeSafe (temp, 100))
	  in
		(response, newState)
	  end

  (* history subservice *)
  (* returns the last 2 chat messages from state *)
  (* \/ r,r',r'',r'''. (string`r' list`r) -> string`r''' *)
  fun history (state: state) : string =
	  let
		val recentHistory = takeSafe (state, 2)
		val revList = List.rev recentHistory
		val response : string = String.concatWith "> " (revList)
	  in
		response
	  end

  (* service function *)
  (* responsible for handling requests and running corresponding subroutines *)
  (* Carries state between requests and operates in a loop *)
  (* \/ r,r'.(string,r) -> (string,r') *)
  fun service (inStream : instream) (state : state) : unit =
	  let
		val state' =
		  let with r3 r4
			val (subservice, data : string`r3) = req (inStream) : subservice * string`r3
		  in
			case subservice of
			  Exit => (
				print "Exiting service loop...\n";
				raise Fail "Service exited"
		)
	| Nothing => (
		print "No valid subservice requested. Continuing...\n";
		state
	  )
	| Chat => (
		let
		  val (response : string`r4, state') = chat (state, data)
		in
		  resp (response : string`r4);
		  state'
		end
	  )
	| History => (
		let
		  val response : string`r4 = history state
		in
		  resp (response : string`r4);
		  state
		end
	  )
		  end
	  in
		service inStream state'
	  end
in
  fun run () = (
	print "Starting mock server\n";

	let with rs1 rs2
	  (* initializations *)
	  val inStream : instream = openIn "mock_requests.txt"
      val state = nil : string`rs1 list`rs2
	in
	  service inStream state handle Fail msg => (
		closeIn inStream;
		print ("Service loop exited with: " ^ msg ^"\n")
	)
	end
  )
end

val _ = run () 
