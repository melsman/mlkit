signature MEM_TIME =
  sig
    (* memTime {cmd, args, out_file} -
     * Execute a cmd with args and report its memory and time usage.
     * Output from the command is redirected to out_file.
     * It is assumed that cmd does not write on stderr. *)

    type report = {count:int, rss:int, size: int,
		   data: int, stk: int, exe: int,
		   sys: Time.time, user: Time.time, real: Time.time}
    val pp_report : report -> string
    val add_report : report * report -> report
    val div_time : Time.time * int -> Time.time
    val div_report : report * int -> report
    val zero_report : report

    val memTime: {cmd: string, args: string list,
                  out_file: string, eout_file: string option} -> report
  end

structure SysUtil = struct

fun quot s = "'" ^ s ^ "'"

fun error s = (print s; raise Fail s)

fun vchat0 (verbose:unit -> bool) s =
    if verbose() then print (" ++ " ^ s ^ "\n")
    else ()

fun system_v verbose cmd : unit =
    (vchat0 verbose ("Executing command: " ^ cmd);
     let val status = OS.Process.system cmd
		      handle _ => error ("Command failed: " ^ quot cmd)
     in if OS.Process.isSuccess status then ()
        else error ("Command failed: " ^ quot cmd)
     end)

fun system cmd : unit = system_v (fn () => false) cmd
end


structure MemTime : MEM_TIME =
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
	   sys=Time.+(sys,sys'), user=Time.+(user,user),real=Time.+(real,real')}
    fun div_time (t,n) = Time.fromReal(Time.toReal t / real n)
    fun div_report ({count, rss, size, data, stk, exe, sys, user, real=r} : report, n:int)
        : report =
          {count=count div n, rss=rss div n, size=size div n,
	   data=data div n, stk=stk div n, exe=exe div n,
	   sys=div_time(sys,n), user=div_time(user,n),real=div_time(r,n)}
    val zero_report = {count=0, rss=0, size=0, data=0, stk=0, exe=0,
		       sys=Time.zeroTime, user=Time.zeroTime,real=Time.zeroTime}

    fun readAll f =
        let val is = TextIO.openIn f
            val s = TextIO.inputAll is handle X => (TextIO.closeIn is; raise X)
            val () = TextIO.closeIn is
        in s
        end

(* macos format of '/usr/bin/time -lp':
real         0.22
user         0.18
sys          0.02
  18812928  maximum resident set size
         0  average shared memory size
         0  average unshared data size
         0  average unshared stack size
     13338  page reclaims
         0  page faults
         0  swaps
         0  block input operations
         0  block output operations
         0  messages sent
         0  messages received
         0  signals received
         0  voluntary context switches
        62  involuntary context switches
*)
    fun splitLines s = String.tokens (fn c => c = #"\n") s

    fun lookR lines s =
        case List.find (String.isSuffix s) lines of
            SOME l => (case Real.fromString l of
                           SOME v => v
                         | NONE => raise Fail ("lookR: cannot read real in line '" ^ l ^ "'"))
          | NONE => raise Fail ("lookR: cannot find line with string '" ^ s ^ "'")
    fun lookL lines s =
        case List.find (String.isPrefix s) lines of
            SOME l =>
            let val k = String.extract(l,size s,NONE)
            in (case Real.fromString k of
                    SOME v => v
                  | NONE => raise Fail ("lookL: cannot read real in line '" ^ l ^ "'"))
            end
          | NONE => raise Fail ("lookL: cannot find line with string '" ^ s ^ "'")

    fun memTime_macos {cmd, args, out_file, eout_file} : report =
        let val timeout = case eout_file of
                              SOME f => f
                            | NONE => "time.out"
            val args = String.concatWith " " args
            val command = cmd ^ " " ^ args ^ " > " ^ out_file
            val command2 = "/usr/bin/time -lp " ^ command ^ " 2> " ^ timeout
            val () = OS.FileSys.remove timeout handle _ => ()
            val () = SysUtil.system command2 handle _ => SysUtil.error "Failed to execute command"
            val s = readAll timeout handle _ => SysUtil.error "Failed to read output file"
            val lines = splitLines s
            val rss = Real.floor (lookR lines "maximum resident set size")
            val rss = rss div 1000
            val size = 0
            val data = 0
            val stk = 0
            val exe = 0
            val sys = Time.fromReal (lookL lines "sys")
            val real = Time.fromReal (lookL lines "real")
            val user = Time.fromReal (lookL lines "user")
        in {count=1,size=size,rss=rss,
	    data=data,stk=stk,exe=exe,
	    sys=sys,
	    user=user,
	    real=real}
        end

(* linux format of '/usr/bin/time -v':
	Command being timed: "./lexgen"
	User time (seconds): 0.74
	System time (seconds): 0.04
	Percent of CPU this job got: 96%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.81
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 18808
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 19005
	Voluntary context switches: 39
	Involuntary context switches: 1
	Swaps: 0
	File system inputs: 0
	File system outputs: 8960
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
*)

    fun memTime_linux {cmd, args, out_file, eout_file} : report = raise Fail "memTime_linux not implemented"

    val memTime = if true then memTime_macos
                  else memTime_linux
  end
