signature MEM_USAGE =
  sig
    (* memUsage {cmd, out_file} - Spawn a child process
     * executing cmd and monitor its memory usage.  Output from
     * executing the command is redirected to out_file. *)

    type report = {count:int, rss:int, size: int, 
		   data: int, stk: int, exe: int,
		   sys: Time.time, user: Time.time, real: Time.time}
    val pp_report : report -> string
              
    val memUsage: {cmd: string, args: string list, out_file: string} -> report
  end

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
	val pid_s = (Int.toString o Word32.toInt o Posix.Process.pidToWord) pid
	  
	val delay = Time.fromMilliseconds 50
	fun sleep() = (* OS.IO.poll(nil,SOME delay)  *)
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
	      real=Timer.checkRealTimer realtimer}
	  end
	else raise Fail "loop_and_monitor_child: wrong pid"
      end

    fun memUsage {cmd, args, out_file} : report =
      let val {cstime=cstime0, cutime=cutime0,...} = Posix.ProcEnv.times()
      in
	case Posix.Process.fork () 
	  of SOME pid =>                          (* We're in the parent process *)
	    ((loop_and_monitor_child pid {cstime0=cstime0,cutime0=cutime0,
					realtimer=Timer.startRealTimer()}) handle OS.SysErr(s,_) => raise Fail s)
	   | NONE =>                              (* We're in the child process *)
	    let val fd = Posix.FileSys.creat(out_file, Posix.FileSys.S.irwxu)
	      handle OS.SysErr(s,e) => raise Fail ("Dealing with: "^ out_file ^ " " ^ s)
             | _ => raise Fail "memUsage.child.openf failed"
	    in (* convert stdout, etc to file out_file *)
	      (Posix.IO.close Posix.FileSys.stdout;
	      Posix.IO.dup fd;
	      Posix.IO.close Posix.FileSys.stderr;
	      Posix.IO.dup fd;
	      Posix.IO.close Posix.FileSys.stdin;
	      Posix.Process.exec (cmd, cmd::args)) handle OS.SysErr (s,_) => raise Fail s
	    end
      end
  end

