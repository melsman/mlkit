
signature TIMING =
  sig 
    val timing_begin   : unit -> unit 
    val timing_end     : string -> unit
    val timing_end_res : string * 'a -> 'a
    val timing         : string -> ('a -> 'b) -> 'a -> 'b
    val new_file       : string -> unit
(*
    val get_timings    : unit -> (string *
                                  ({name: string,
				    non_gc: Time.time,
				    system: Time.time,
				    gc: Time.time,
				    wallclock: Time.time}) list) list
*)
    val reset_timings  : unit -> unit
      
    (* Local timers used manually (and temporarily) in modules. *)
    (* There is only one local timer available.                 *)
    (* A local_time is updated. A new time is therefore _not_   *)
    (* returned in function local_timing_end.                   *)
    type local_time
    val empty_local_time : unit -> local_time
    val local_timing_begin : unit -> unit
    val local_timing_end : local_time -> unit
    val reset_local_time : local_time -> unit
    val pp_local_timing : local_time -> string

  end

functor Timing(structure Flags: FLAGS
	       structure Crash: CRASH) : TIMING =

  struct
    
    fun die s = Crash.impossible ("Timing." ^ s)

    fun msg(s: string) = (TextIO.output(!Flags.log, s); TextIO.flushOut (!Flags.log))
    fun msg'(s: string) = (TextIO.output(TextIO.stdOut, s); TextIO.flushOut (TextIO.stdOut))

    fun raise_again s e = 
	(print ("Function " ^ s ^ " raises exception " ^ General.exnName e ^ "\n");
	 raise e)

    val compiler_timings = ref false;
    val _ = Flags.add_bool_entry {long="compiler_timings", short=SOME "timings", item=compiler_timings,
				  neg=false, menu=["Debug", "compiler timings"],
				  desc="Show compiler timings for each compilation phase."}

    val t = ref (Timer.startCPUTimer())
    val rt = ref (Timer.startRealTimer())

    val timingNow = ref false

    val timings : (string *
                   ({name: string,
		     non_gc: Time.time,
		     system: Time.time,
		     gc: Time.time,
		     wallclock: Time.time}) list) list ref = ref []

    fun timing_begin () =
      if !compiler_timings then 
	(if !timingNow then
	   die "Only one timer available"
	 else
	   (t := Timer.startCPUTimer(); 
	    timingNow := true;
	    rt := Timer.startRealTimer()))
      else ()

    fun Time_plus (t1,t2) = Time.+(t1,t2)
	handle E => raise_again "Time_plus" E

    fun add_time_elems {name=name1: string, non_gc=non_gc1: Time.time,
			system=system1: Time.time,
			gc=gc1: Time.time, 
			wallclock=wallclock1: Time.time}
                       {name=name2: string, non_gc=non_gc2: Time.time,
			system=system2: Time.time,
			gc=gc2: Time.time, 
			wallclock=wallclock2: Time.time} =
      if name1<>name2 then
	die "Can only add timeelements with same name."
      else
	{name = name1,
	 non_gc=Time_plus(non_gc1,non_gc2),
	 system=Time_plus(system1,system2),
	 gc=Time_plus(gc1,gc2),
	 wallclock=Time_plus(wallclock1,wallclock2)}

    fun insert_time_elem [] time_elem = [time_elem]
      | insert_time_elem ((timings as {name=name1,...})::rest) 
                          (time_elem as {name=name2,...}) = 
	if name1=name2 then
	  (add_time_elems timings time_elem)::rest
	else
	  timings::(insert_time_elem rest time_elem)

    fun maybe_export_timings {name, non_gc=usr, system=sys, gc, wallclock=real} =
     (case !Flags.timings_stream
	of SOME os =>
	  (let fun out t = TextIO.output(os, name ^ " " ^ Time.toString t ^ "\n")
	   in out usr ; TextIO.flushOut os
	   end handle _ => die "maybe_export_timings.I could not write timings to timings stream")
	 | NONE => ())
	  handle E => raise_again "maybe_export_timings" E
      
    fun timing_end (name) = 
      if !compiler_timings orelse !timingNow then 
	(let 
           val _ = if !timingNow then ()
		   else die "timing_end called with no timer started"
	   val _ = timingNow := false
	   val {gc, sys=system, usr=non_gc} = Timer.checkCPUTimer (!t)
	   val wallclock = Timer.checkRealTimer (!rt)
	       handle E => Time.zeroTime
		   (* raise_again "timing_end.checkRealTimer" E *)

	   val padL = StringCvt.padLeft #" " 15 
	   
	   val time_elem = {name = name,
			    non_gc = non_gc,
			    system = system,
			    gc = gc,
			    wallclock = wallclock}

	   val _ = maybe_export_timings time_elem

	   val _ = timings :=
	     (case (!timings) 
		of [] => [("Unknown", [time_elem])]
		 | ((file,timings)::rest) =>
		  (file,insert_time_elem timings time_elem)::rest)
	 in
(*	   chat
	   (concat[name,"\n","\t",padL "non-gc", padL "system", padL "gc", padL"wallclock",
		   "\n\t",
		   padL (Time.toString non_gc),padL (Time.toString system),
		   padL (Time.toString gc),padL (Time.toString wallclock)]) *) ()
	 end  (*;
	   chat("\n") *)) handle E => raise_again "timing_end" E
      else ()
      
    fun timing_end_res (name, x) = (timing_end name; x)

    fun timing (name:string) (f: 'a -> 'b) (a: 'a) : 'b = 
      (timing_begin (); f a before timing_end name)

      
    (* The first list in timings will always be the *)
    (* current one.                                 *)
    fun new_file filename =
      if (!timingNow) then
	die "You have called Timing.new_file while a timer is active."
      else
	timings:= (filename,[])::(!timings)

    fun get_timings () = List.rev (!timings)
      
    fun reset_timings() = (timings := [];
			   timingNow := false)
      
    (* Local timers used manually (and temporarily) in modules. *)
    (* There is only one local timer available.                 *)
    type local_time = Time.time ref
    fun empty_local_time() = ref (Time.zeroTime)
    val local_timer = ref (Timer.startCPUTimer())
    val local_timer_on = ref false
    fun local_timing_begin () = 
      if !local_timer_on then
	die "Local timer already started."
      else
	(local_timer_on := true;
	 local_timer := Timer.startCPUTimer())
    fun local_timing_end timer =
      (timer := Time.+ (!timer, #usr(Timer.checkCPUTimer(!local_timer)));
       local_timer_on := false)
    fun reset_local_time timer = timer := Time.zeroTime
    fun pp_local_timing timer = StringCvt.padLeft #" " 15 (Time.toString (!timer))
	handle E => raise_again "pp_local_timing" E
end
