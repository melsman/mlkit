signature MEM_USAGE =
  sig
    type time = Time.time
    val second: time
    val tenthSecond: time
    val quarterSecond: time
      
    (* memUsage {cmd, out_file, delay} - Spawn a child process
     * executing cmd and monitor its memory usage.  Output from
     * executing the command is redirected to out_file. The monitor
     * waits, roughly, delay between each report. *)

    type report = {count:int, rss:int, size: int, 
		   stime: Time.time, utime: Time.time}
              
    val memUsage: {cmd: string, args: string list, out_file: string, delay: time} -> report
  end

structure MemUsage : MEM_USAGE =
  struct

    type report0 = {count:int, rss:int, size: int}
    type report = {count:int, rss:int, size: int, 
		   stime: Time.time, utime: Time.time}

    type time = Time.time
    val second = Time.fromMilliseconds 1000
    val longTime = Time.fromMilliseconds 10000
    val tenthSecond = Time.fromMilliseconds 100 
    val quarterSecond = Time.fromMilliseconds 250

    fun max i i' = if i > i' then i else i'

    fun new {count, size, rss} {size=size',rss=rss'} =
      {count=count+1, size=max size size', rss=max rss rss'}

    fun loop_and_monitor_child (delay: time) (pid:Posix.Process.pid) {cutime0,cstime0} =
      let 
	val pid_s = (Int.toString o Word32.toInt o Posix.Process.pidToWord) pid
	fun loop acc = case (Posix.Process.sleep delay; Info.getInfo pid_s)
			 of SOME minfo => loop (new acc minfo)
			  | NONE => acc
	val {count,size,rss} = loop {count=0,size=0,rss=0}
      in 
	(* Wait for the dead child *)
	if #1 (Posix.Process.waitpid (Posix.Process.W_CHILD pid, nil)) = pid then 
	  let val {cstime, cutime,...} = Posix.ProcEnv.times()
	  in {count=count,size=size,rss=rss,
	      stime=Time.-(cstime,cstime0),
	      utime=Time.-(cutime,cutime0)}
	  end
	else raise Fail "loop_and_monitor_child: wrong pid"
      end

    fun memUsage {cmd, args, out_file, delay} : report =
      let val {cstime=cstime0, cutime=cutime0,...} = Posix.ProcEnv.times()
      in
	case Posix.Process.fork () 
	  of SOME pid =>                          (* We're in the parent process *)
	    loop_and_monitor_child delay pid {cstime0=cstime0,cutime0=cutime0}
	   | NONE =>                              (* We're in the child process *)
	    let val fd = Posix.FileSys.creat(out_file, Posix.FileSys.S.irwxu)
	      handle _ => raise Fail "memUsage.child.openf failed"
	    in (* convert stdout, etc to file out_file *)
	      Posix.IO.close Posix.FileSys.stdout;
	      Posix.IO.dup fd;
	      Posix.IO.close Posix.FileSys.stderr;
	      Posix.IO.dup fd;
	      Posix.IO.close Posix.FileSys.stdin;
	      Posix.Process.exec (cmd, cmd::args)
	    end
      end
  end

