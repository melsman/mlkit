signature NS_DEBUG = 
  sig
    val addMsg  : quot -> unit
    val logMsgs : unit -> unit
  end

structure NsDebug :> NS_DEBUG =
  struct
    local
      fun log (s: string):unit =
	prim("@Ns_Log", (0, s))
      val msgs : quot list ref = ref [] 
    in
      fun addMsg m = msgs := m :: !msgs
      fun logMsgs () =
	let
	  val msg = 
	    "\nNsDebug:\n" ^ (String.concatWith "\n" (List.map Quot.toString (List.rev (!msgs))))
	in
	  log msg
	end
    end
  end