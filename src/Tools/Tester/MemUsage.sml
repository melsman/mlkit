signature MEM_USAGE =
  sig
    (* memUsage {cmd, out_file} - Spawn a child process
     * executing cmd and monitor its memory usage.  Output from
     * executing the command is redirected to out_file. *)

    type report = {count:int, rss:int, size: int,
		   data: int, stk: int, exe: int,
		   sys: Time.time, user: Time.time, real: Time.time}
    val pp_report : report -> string
    val add_report : report * report -> report
    val div_time : Time.time * int -> Time.time
    val div_report : report * int -> report
    val zero_report : report

    val memUsage: {cmd: string, args: string list,
                   out_file: string, eout_file: string option} -> report
  end

fun err exn s =
    (print ("MemUsage." ^ s ^ " raised " ^ exnMessage exn ^ "\n"); raise exn)

structure MemUsage : MEM_USAGE =
  struct

    type report0 = {count:int, rss:int, size: int,
		    data: int, stk: int, exe: int}
    type report = {count:int, rss:int, size: int,
		   data: int, stk: int, exe: int,
		   sys: Time.time, user: Time.time, real: Time.time}

    fun pp_report {count,rss,size,data,stk,exe,sys,user,real} =
      "count: " ^ (Int.toString count) ^ "\nrss: " ^ (Int.toString rss) ^
      "Kb.\nsize: " ^ (Int.toString size) ^ "Kb.\ndata: " ^ (Int.toString data) ^
      "Kb.\nstk: " ^ (Int.toString stk) ^ "Kb.\nexe: " ^ (Int.toString exe) ^
      "Kb.\nsys: " ^ (Time.toString sys) ^ "sec.\nuser: " ^ (Time.toString user) ^
      "sec.\nreal: " ^ (Time.toString real) ^ "sec.\n"

    fun add_report ({count, rss, size, data, stk, exe, sys, user, real} : report,
                    {count=count', rss=rss', size=size', data=data', stk=stk', exe=exe', sys=sys', user=user', real=real'})
        : report =
          {count=count+count', rss=rss+rss', size=size+size',
	   data=data+data', stk=stk+stk', exe=exe+exe',
	   sys=Time.+(sys,sys'), user=Time.+(user,user),real=Time.+(real,real')} handle X => err X "add_report"
    fun div_time (t,n) = Time.fromReal(Time.toReal t / real n) handle X => err X "div_time"
    fun div_report ({count, rss, size, data, stk, exe, sys, user, real=r} : report, n:int)
        : report =
          {count=count div n, rss=rss div n, size=size div n,
	   data=data div n, stk=stk div n, exe=exe div n,
	   sys=div_time(sys,n), user=div_time(user,n),real=div_time(r,n)} handle X => err X "div_report"
    val zero_report = {count=0, rss=0, size=0, data=0, stk=0, exe=0,
		       sys=Time.zeroTime, user=Time.zeroTime,real=Time.zeroTime}

    fun max i i' = if i > i' then i else i'

    fun new {count, size, rss, data, stk, exe}
      {size=size',rss=rss',data=data', stk=stk', exe=exe'} =
      {count=count+1,
       size=max size size',
       rss=max rss rss',
       data=max data data',
       stk=max stk stk',
       exe=max exe exe'}

    fun loop_and_monitor_child (pid:Posix.Process.pid) {cutime0,cstime0,realtimer} =
      let
	val pid_s = (Int.toString o SysWord.toInt o Posix.Process.pidToWord) pid

	val delay = Time.fromMilliseconds 50
	fun sleep() = (*OS.IO.poll(nil,SOME delay)  *)
                OS.Process.sleep delay

	fun loop acc = case (sleep(); Info.getInfo pid_s)
			 of SOME minfo => loop (new acc minfo)
			  | NONE => acc
	val {count,size,rss,data,stk,exe} = loop {count=0,size=0,rss=0,data=0,stk=0,exe=0}
      in
	(* Wait for the dead child *)
	if #1 (Posix.Process.wait()) = pid then
	  let val {cstime, cutime,...} = Posix.ProcEnv.times()
	  in {count=count,size=size,rss=rss,
	      data=data,stk=stk,exe=exe,
	      sys=Time.-(cstime,cstime0),
	      user=Time.-(cutime,cutime0),
	      real=Timer.checkRealTimer realtimer} handle X => err X "loop_and_monitor_child"
	  end
	else raise Fail "loop_and_monitor_child: wrong pid"
      end

    fun memUsage {cmd, args, out_file, eout_file} : report =
        let
          val {cstime=cstime0, cutime=cutime0,...} = Posix.ProcEnv.times() handle X => err X "ProcEnv.times"
      in
	case Posix.Process.fork ()
	  of SOME pid =>                          (* We're in the parent process *)
	     (((loop_and_monitor_child pid {cstime0=cstime0,cutime0=cutime0,
					    realtimer=Timer.startRealTimer()}) handle OS.SysErr(s,_) => raise Fail s)
                  handle X => err X "in parent")
	   | NONE =>                              (* We're in the child process *)
	    let val fd = Posix.FileSys.creat(out_file, Posix.FileSys.S.irwxu)
	                 handle OS.SysErr(s,e) => raise Fail ("Dealing with: " ^ out_file ^ " " ^ s)
                              | _ => raise Fail "memUsage.child.openf failed"
                val efd = case eout_file of
                              SOME eout_file => (Posix.FileSys.creat(eout_file, Posix.FileSys.S.irwxu)
	                                         handle OS.SysErr(s,e) => raise Fail ("Dealing with: " ^ eout_file ^ " " ^ s)
                                                      | _ => raise Fail "memUsage.child.openf failed")
                            | NONE => fd
	    in (* convert stdout, etc to file out_file *)
	     (Posix.IO.close Posix.FileSys.stdout;
	      Posix.IO.dup fd;
	      Posix.IO.close Posix.FileSys.stderr;
	      Posix.IO.dup efd;
	      Posix.IO.close Posix.FileSys.stdin;
	      let val res = Posix.Process.exec (cmd, cmd::args)
              in Posix.IO.close fd;
                 Posix.IO.close efd;
                 res
              end) handle OS.SysErr (s,_) => raise Fail s
	    end handle X => err X "in child"

      end
  end
