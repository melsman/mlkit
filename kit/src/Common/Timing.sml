(*$TIMING*)
signature TIMING =
  sig 
    val timing_begin   : unit -> unit 
    val timing_end     : string -> unit
    val timing_end_res : (string * 'a) -> 'a
    val new_file       : string -> unit
    val get_timings    : unit -> (string *
                                  ({name: string,
				    non_gc: Time.time,
				    system: Time.time,
				    gc: Time.time,
				    wallclock: Time.time}) list) list
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

(*$Timing: TIMING FLAGS*)
functor Timing(structure Flags: FLAGS
	       structure Crash: CRASH) : TIMING =

    (* Two versions provided, one of which uses Non Standard ML stuff *)
(*
       struct  (* Pure Standard ML *)
        fun timing_begin () = ()
	fun timing_end s = ()
        fun timing_end_res (s, x) = x
	fun new_file s = ()
	fun get_timings () = []
	fun reset_timings () = ()

	type local_time = unit
	fun empty_local_time () = ()
	fun local_timing_begin () = ()
	fun local_timing_end () = ()
	fun reset_local_time () = ()
	fun pp_local_timing () = ""

      end
*)

  struct (* SML/NJ *)


    structure String = Edlib.String
    
    fun die s = Crash.impossible ("Timing." ^ s)

    fun msg(s: string) = (TextIO.output(!Flags.log, s); TextIO.flushOut (!Flags.log))
    fun msg'(s: string) = (TextIO.output(TextIO.stdOut, s); TextIO.flushOut (TextIO.stdOut))
	  
    val show_compiler_timings = ref false;
    val _ = Flags.add_flag_to_menu (["Control"], "show_compiler_timings",
				    "show compiler timings",
				    show_compiler_timings)

    fun chat(s: string) = if !show_compiler_timings then (msg' s; msg s) else ()
                             
    val t = ref (Timer.startCPUTimer())
    val rt = ref (Timer.startRealTimer())

    val timingNow = ref false

    val timings : (string *
                   ({name: string,
		     non_gc: Time.time,
		     system: Time.time,
		     gc: Time.time,
		     wallclock: Time.time}) list) list ref = ref []

    fun timing_begin () = (if !timingNow = true then
			     die "Only one timer available"
			   else
			     (t := Timer.startCPUTimer(); 
			      timingNow := true;
			      rt := Timer.startRealTimer()))


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
	 non_gc=Time.+(non_gc1,non_gc2),
	 system=Time.+(system1,system2),
	 gc=Time.+(gc1,gc2),
	 wallclock=Time.+(wallclock1,wallclock2)}

    fun insert_time_elem [] time_elem = [time_elem]
      | insert_time_elem ((timings as {name=name1,...})::rest) 
                          (time_elem as {name=name2,...}) = 
	if name1=name2 then
	  (add_time_elems timings time_elem)::rest
	else
	  timings::(insert_time_elem rest time_elem)
      
    fun timing_end (name) = 
      (chat("\n");
       let 
	 val _ = timingNow := false
	 val {gc, sys=system, usr=non_gc} = Timer.checkCPUTimer (!t)
	 val wallclock = Timer.checkRealTimer (!rt)

	 val padL = String.padL " " 15 
	 val padC = String.padC " " 15 
	   
	 val time_elem = {name = name,
			  non_gc = non_gc,
			  system = system,
			  gc = gc,
			  wallclock = wallclock}

	 val _ = timings :=
	   (case (!timings) 
	      of [] => [("Unknown", [time_elem])]
	       | ((file,timings)::rest) =>
		(file,insert_time_elem timings time_elem)::rest)
       in
	 chat
	 (concat[name,"\n","\t",padL "non-gc", padL "system", padL "gc", padL"wallclock",
		  "\n\t",
		  padL (Time.toString non_gc),padL (Time.toString system),
		  padL (Time.toString gc),padL (Time.toString wallclock)])
       end;
       chat("\n"))
      
    fun timing_end_res (name, x) = (timing_end name; x)
      
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
    fun pp_local_timing timer = String.padL " " 15 (Time.toString (!timer))

end
