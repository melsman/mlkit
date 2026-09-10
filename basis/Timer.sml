(* Timer -- new basis 1995-03-20, 1995-09-14, 1995-11-06, 1997-03-07 *)

(* Under DOS, real time and cpu time are the same *)

(** SigDoc *)
structure Timer :> TIMER =
  struct

    type tusage = {gcSec : int,  gcUsec : int,
                   sysSec : int, sysUsec : int,
                   usrSec : int, usrUsec : int}

    fun getrutime_ () : tusage = prim("sml_getrutime", ())

    open Time

    type cpu_timer  = {usr : time, sys : time, gc : time}
    type real_timer = time

    val fromSeconds = fromSeconds o LargeInt.fromInt
    val fromMicroseconds = fromMicroseconds o LargeInt.fromInt

    fun CPUTimer rutime =
	let val {gcSec, gcUsec, sysSec, sysUsec, usrSec, usrUsec}
	        = rutime
	in {usr = fromSeconds usrSec + fromMicroseconds usrUsec,
	    sys = fromSeconds sysSec + fromMicroseconds sysUsec,
	    gc = fromSeconds gcSec + fromMicroseconds gcUsec}
	end

    fun startCPUTimer () = CPUTimer (getrutime_ ())

    (* The runtime reports no collector time of its own (the gc fields
       of sml_getrutime are zero), so the collector's share of the user
       time is what it reports, and its system time is zero. *)
    fun checkCPUTimes {usr, sys, gc} =
	let val {gcSec, gcUsec, sysSec, sysUsec, usrSec, usrUsec}
	        = getrutime_ ()
	    val gc = fromSeconds gcSec + fromMicroseconds gcUsec - gc
	    val usr = fromSeconds usrSec + fromMicroseconds usrUsec - usr
	    val sys = fromSeconds sysSec + fromMicroseconds sysUsec - sys
	in {nongc = {usr = usr - gc, sys = sys},
	    gc = {usr = gc, sys = zeroTime}}
	end

    fun checkCPUTimer timer =
	let val {nongc, gc} = checkCPUTimes timer
	in {usr = #usr nongc + #usr gc, sys = #sys nongc + #sys gc}
	end

    fun checkGCTime timer = #usr (#gc (checkCPUTimes timer))

    fun startRealTimer () = now ()

    fun checkRealTimer time1 = now () - time1

    (* Removed 1995-11-03, added again 1997-03-07 *)

    fun totalCPUTimer _ = CPUTimer Initial.initial_rutime
    fun totalRealTimer _ =
        let val {sec,usec} = Initial.initial_realtime
        in Time.+(Time.fromSeconds (Int.toLarge sec),
                  Time.fromMicroseconds (Int.toLarge usec))
        end

  end
