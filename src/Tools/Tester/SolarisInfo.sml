structure Info: INFO =
struct

    (*************************************************************)
    (* Solaris                                                   *)
    (*************************************************************)

    fun getInfo' stat =
	let val contents = BinIO.inputAll stat
	in
	    BinIO.closeIn stat;
	    contents
	end
            handle ex => (BinIO.closeIn stat;
			  raise ex)

    fun getInfo (pid) =
	let 
	    val stat = BinIO.openIn ("/proc/" ^ pid ^ "/psinfo")
	    val statContents = getInfo' stat
	    val size = Word32.toInt (PackWord32Big.subVec (statContents, 11))
	    val rss = Word32.toInt (PackWord32Big.subVec (statContents, 12))
	in
	    {size = size * 1024,
	     rss = rss * 1024}
	end

end (* structure Info *)

