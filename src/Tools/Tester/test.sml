use "INFO.sml";
use "LinuxInfo.sml";

    type report = {count:int, rss:int, size: int}

    type time = Time.time
    val second = Time.fromMilliseconds 1000
    val longTime = Time.fromMilliseconds 10000
    val tenthSecond = Time.fromMilliseconds 100 
    val quarterSecond = Time.fromMilliseconds 250

    fun max i i' = if i > i' then i else i'

    fun new {count, size, rss} {size=size',rss=rss'} =
      {count=count+1, size=max size size', rss=max rss rss'}

    fun loop_and_monitor_child (delay: time) (pid:Posix.Process.pid) (acc: report) =
      let val _ = Posix.Process.sleep delay
	val pid_s = (Int.toString o Word32.toInt o Posix.Process.pidToWord) pid
      in case Info.getInfo pid_s
	   of SOME minfo => loop_and_monitor_child delay pid (new acc minfo)
	    | NONE => 
	     let val (pid', _) = Posix.Process.waitpid (Posix.Process.W_CHILD pid, nil)
	     in if pid <> pid' then raise Fail "loop_and_monitor_child: wrong pid"
		else acc
	     end
      end 

    fun memUsage {cmd, out_file, delay} : report =
      case Posix.Process.fork () 
	of SOME pid =>                          (* We're in the parent process *)
	  loop_and_monitor_child delay pid {count=0,rss=0,size=0}
	 | NONE =>                              (* We're in the child process *)
	  let val fd = Posix.FileSys.creat(out_file, Posix.FileSys.S.irwxu)
	    handle _ => raise Fail "memUsage.child.openf failed"
	  in (* convert stdout, etc to file out_file *)
	    Posix.IO.close Posix.FileSys.stdout;
	    Posix.IO.dup fd;
	    Posix.IO.close Posix.FileSys.stderr;
	    Posix.IO.dup fd;
	    
	    Posix.IO.close Posix.FileSys.stdin;

	    Posix.Process.exec (cmd,[cmd])
	  end;

fun test s = memUsage {cmd=s, delay=second, out_file="/tmp/test"} 

