structure Info: INFO =
struct

    (*************************************************************)
    (* Linux                                                     *)
    (*************************************************************)

    fun getInfo' stat =
	let val line = TextIO.inputLine stat
	in
	    TextIO.closeIn stat;
	    line
	end
	    handle ex => (TextIO.closeIn stat;
			  raise ex)
(*
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
*)
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
	  fun isDelimiter c = (c = #" " orelse c = #"\t" orelse c = #"\n")
	  val data = String.tokens isDelimiter statContents
	  val _ = print ("StatContents is " ^ statContents)
(*	  val _ = print (List.nth (data,3) ^ "\n") *)
	  fun getInt n = valOf (Int.fromString (List.nth (data, n)))
      in if List.nth (data,3) = "Z" then NONE
	 else SOME {rss = getInt 1, size = getInt 2}
      end handle _ => NONE

end (* structure Info *)