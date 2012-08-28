structure LinuxInfo : INFO = struct

fun withFile s f =
    let val is = TextIO.openIn s
    in (f is before TextIO.closeIn is)
       handle _ => (TextIO.closeIn is; NONE)
    end
	
local
    fun readEntry e line =
      let
	fun removeTrailingNl l =
	  case rev l
	    of #"\n" :: l => SOME(implode(rev l))
	     | _ => NONE
	fun loop (nil, #":" :: l) = removeTrailingNl l
	  | loop (c::cs,k::ks) = if c = k then loop(cs,ks)
				 else NONE
	  | loop _ = NONE
      in loop (explode e, explode line)
      end

    fun loopFile nil acc is = SOME (rev acc)
      | loopFile (all as e::es) acc is =
        Option.mapPartial 
           (fn line => case readEntry e line
                       of SOME res => loopFile es (res::acc) is
                        | NONE => loopFile all acc is)
           (TextIO.inputLine is)
    in
      fun getProcStatusEntries pid es =    
        withFile ("/proc/" ^ pid ^ "/status") 
        (loopFile es nil)
    end

  fun getInfo pid =
    case getProcStatusEntries pid ["VmSize", "VmRSS", "VmData", "VmStk", "VmExe"]
      of NONE => NONE
       | SOME l =>
	case map Int.fromString l
	  of [SOME size,SOME rss,SOME data,SOME stk,SOME exe] => 
	    SOME {size=size,rss=rss,data=data,stk=stk,exe=exe}
	   | _ => NONE
(*
    case (getProcStatusEntryInt pid "VmRSS",
	  getProcStatusEntryInt pid "VmSize")
      of (SOME rss, SOME size) => SOME {rss=rss,size=size}
       | _ => NONE
*)
(*
    fun getInfo' stat =
	let val line = TextIO.inputLine stat
	in
	    TextIO.closeIn stat;
	    line
	end
	    handle ex => (TextIO.closeIn stat;
			  raise ex)

    fun getInfo (pid) =
	let val stat = TextIO.openIn ("/proc/" ^ pid ^ "/stat")
	    val statContents = getInfo' stat
	    fun isDelimiter c = (c = #" ")
	    val data = String.tokens isDelimiter statContents
	    fun getInt n =
		valOf (Int.fromString (List.nth (data, n)))
	in if List.nth (data,2) = "Z" then raise Fail (pid ^ " is now a zombie")
	   else
	    {size = getInt 22,  (*was 22*)
	     rss =  4 * 1024 * (3 + getInt 23)}    (*was 23*)
	end

    fun isDelimiter #" "  = true
      | isDelimiter #"\t" = true
      | isDelimiter #"\n" = true
      | isDelimiter _     = false

    fun getInfo pid =
      let (* val _ = print "Entering getInfo\n" *)
	val tmpfile = "/tmp/kittest." ^ pid
	val _ = OS.FileSys.remove tmpfile handle _ => ()
	  val _ = OS.Process.system ("/bin/ps -hp " ^ pid ^ " -o pid,rss,vsz,stat > " ^ tmpfile)
	    handle ? => (print "Error1\n"; raise ?)
	  val is = TextIO.openIn tmpfile
	    handle ? => (print "Error2\n"; raise ?)
	  val statContents = getInfo' is
	  val _ = OS.FileSys.remove tmpfile handle _ => ()
	  val data = String.tokens isDelimiter statContents
(*	  val _ = print ("StatContents is " ^ statContents) *)
(*	  val _ = print (List.nth (data,3) ^ "\n") *)
	  fun getInt n = valOf (Int.fromString (List.nth (data, n)))
      in if List.nth (data,3) = "Z" then NONE
	 else SOME {rss = getInt 1, size = getInt 2}
      end handle _ => NONE
*)
end
