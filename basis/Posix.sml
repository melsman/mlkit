structure Posix :> POSIX =
    struct

    fun mkerrno_ (i : int) : OS.syserror =               prim("id", i)
    fun errno_ () : OS.syserror =                        prim("sml_errno", ())
    fun formatErr mlOp (SOME operand) reason =
	mlOp ^ " failed on `" ^ operand ^ "': " ^ reason
      | formatErr mlOp NONE reason =
	mlOp ^ " failed: " ^ reason

    (* Raise SysErr from ML function *)
    fun raiseSysML mlOp operand reason =
	raise OS.SysErr (formatErr mlOp operand reason, NONE)

    (* Raise SysErr with OS specific explanation if errno <> 0 *)
    fun raiseSys mlOp operand reason =
	let val errno = errno_ ()
	in
	    if errno = 0 then raiseSysML mlOp operand reason
	    else raise OS.SysErr
		(formatErr mlOp operand (OS.errorMsg errno),
		 SOME (mkerrno_ errno))
	end

    structure Signal : POSIX_SIGNAL =
	struct
	    type signal = int
		
	    fun toWord (s : signal) : SysWord.word = SysWord.fromInt s
	    fun fromWord (w : SysWord.word) : signal = SysWord.toInt w
      open Initial.Posix_Values.Signal
	end
    structure Error : POSIX_ERROR = 
      struct
        type syserror = OS.syserror
        open Initial.Posix_Values.Err
        fun errorMsg s = OS.errorMsg s
        fun errorName s = OS.errorName s
        fun syserror s = OS.syserror s
        fun fromWord w = SysWord.toInt w
        fun toWord w = SysWord.fromInt w
      end


    structure Process : POSIX_PROCESS=
	struct
	    type pid = int
	    type signal = Signal.signal
		
      datatype waitpid_arg
        = W_ANY_CHILD
        | W_CHILD of pid
        | W_SAME_GROUP
        | W_GROUP of pid

	   datatype exit_status = 
          W_EXITED
	      | W_EXITSTATUS of Word8.word
	      | W_SIGNALED of signal
	      | W_STOPPED of signal

     datatype killpid_arg
      = K_PROC of pid
      | K_SAME_GROUP
      | K_GROUP of pid
		
	    datatype waitpid_arg
		= W_ANY_CHILD | W_CHILD of pid | W_GROUP of pid | W_SAME_GROUP
		
	    structure W =
		struct
		    type flags = SysWord.word
        val untraced = 0wx1
        val nohang = 0wx2

		    fun toWord x = x
		    val fromWord = toWord
		    val flags = List.foldl SysWord.orb 0wx0
		    val all = flags [untraced]
		    val intersect = List.foldl SysWord.andb 0wx1
        val union = List.foldl SysWord.orb 0wx0
		    fun clear (f1,f2) = SysWord.andb (SysWord.notb f1,f2)
		    fun allSet (f1,f2) = SysWord.andb (f1,f2) = f1
		    fun anySet (f1,f2) = SysWord.andb (f1,f2) <> 0wx0
		end
	    
	    val pidToWord = SysWord.fromInt
	    val wordToPid = SysWord.toInt
		
      fun fork() : pid option =
           let
             val ret : int = prim("@fork",())
           in
             if ret < 0 then raiseSys ("Posix.FileSys.fork") NONE ""
             else if ret = 0 then NONE
             else SOME ret
           end
	    
	    fun exit (w:Word8.word) : 'a = 
		let val w = Word8.toInt w
		    val res : unit = prim("@exit",w)
		in raise Fail "never raised"
		end

	    fun waitpid_arg_to_int a : int =
		case a of
		    W_ANY_CHILD => ~1
		  | W_CHILD pid => pid
		  | W_GROUP pid => ~pid
		  | W_SAME_GROUP => 0
		    
	    fun WIFEXITED(status:int) : bool = 
		prim("sml_WIFEXITED", status)

	    fun WIFSIGNALED(status:int) : bool = 
		prim("sml_WIFSIGNALED", status)

	    fun WIFSTOPPED(status:int) : bool = 
		prim("sml_WIFSTOPPED", status)

	    fun WEXITSTATUS(status:int) : Word8.word =
		let val r: int = prim("sml_WEXITSTATUS",status)
		in Word8.fromInt r
		end

	    fun WTERMSIG(status:int) : signal =
		        prim("sml_WTERMSIG",status) : int

	    fun WSTOPSIG(status:int) : signal =
		        prim("sml_WSTOPSIG",status) : int

	    fun flags_to_int l = SysWord.toInt(W.toWord (W.union l))

	    fun waitpid' (wpa: waitpid_arg,flags: W.flags list) : (pid * exit_status) option =
           let
             val (pid:int,status) = 
                  prim("sml_waitpid",(waitpid_arg_to_int wpa,flags_to_int flags))
             val _ = if pid = ~1 then raiseSys "Posix.Process.waitpid" NONE "" else ()
           in
             if (pid <> 0) 
             then SOME(pid,
               if WIFSIGNALED(status)
               then W_SIGNALED(WTERMSIG(status))
               else 
               if WIFEXITED(status) then 
                 let
                   val e = WEXITSTATUS(status)
                 in if e = 0w0 then W_EXITED else W_EXITSTATUS e
                 end
               else if WIFSTOPPED(status)
               then W_STOPPED(WSTOPSIG(status))
               else raise Fail "waitpid.error2")
             else NONE
           end
	    
      fun waitpid (wpa, flags) = Option.valOf(waitpid'(wpa,flags))
	    fun wait () = Option.valOf(waitpid' (W_ANY_CHILD, []))
      fun waitpid_nh (wpa, flags) = waitpid'(wpa, W.nohang :: flags)
		
      fun exec' (s, sl, env, path) = 
           let
             val a = prim("sml_exec", (s : string, sl : string list, env : string list, if path then 1 else 0)) : int
           in
             raiseSys "exec" NONE ""
           end     
      
      fun exec (s : string, sl : string list) = exec'(s,sl,[],true)
      fun exece (s : string, sl : string list, env : string list) = exec'(s,sl,env,true)
      fun execp (s : string, sl : string list) = exec'(s,sl,[],false)

      fun fromStatus 0 = W_EXITED
        | fromStatus _ = W_EXITSTATUS 0wxFF

      fun alarm t = 
           let
             val a = prim("@alarm", Time.toSeconds t)
           in
             if a = ~1
             then raiseSys "Posix.Process.alarm" (SOME (Time.toString t)) ""
             else Time.fromSeconds a
           end

      fun kill (a,s) = 
            let 
              val p = case a of K_PROC pid => pid
                              | K_SAME_GROUP => 0
                              | K_GROUP pid => ~pid

              val r = prim("@kill", (p : int, SysWord.toInt(Signal.toWord s) : int))
            in
              if r = ~1 then raiseSys "Posix.Process.kill" (SOME (Int.toString p)) ""
              else ()
            end

      fun pause () = prim("@pause", ()) : unit

      fun sleep t = 
            let
              val s = Int.fromLarge(Time.toSeconds t)
              val m = Int.fromLarge(Time.toMicroseconds(Time.-(t, Time.fromSeconds (Int.toLarge s))))
              val (r,s,m) = prim("sml_microsleep", (s : int, m : int)) : (int * int * int)
              val _ = if r = ~1 then raiseSys "Posix.Process.sleep" (SOME (Time.toString t)) "" else ()
            in
              Time.+(Time.fromSeconds (Int.toLarge s),Time.fromMicroseconds (Int.toLarge m))
            end

	end
	
    structure ProcEnv : POSIX_PROCENV 
      where type pid = Process.pid = 
	struct 
	    type uid = int
	    type gid = int
	    type pid = Process.pid
	    type file_desc = int

      fun wordToUid w = SysWord.toInt w
      fun uidToWord g = SysWord.fromInt g
      fun wordToGid w = SysWord.toInt w
      fun gidToWord g = SysWord.fromInt g

	    fun sysconf (s : string) =
		let
		    fun rsys i = (prim ("sml_sysconf", i : int) : int)
			handle Overflow => raise OS.SysErr ("Not supported", NONE)
		in
		    SysWord.fromInt 
		    (case s of 
			 "ARG_MAX" => rsys 1
		       | "CHILD_MAX" => rsys 2
		       | "CLK_TCK" => rsys 3
		       | "NGROUPS_MAX" => rsys 4
		       | "OPEN_MAX" => rsys 5
		       | "STREAM_MAX" => rsys 6
		       | "TZNAME_MAX" => rsys 7
		       | "JOB_CONTROL" => rsys 8
		       | "SAVED_IDS" => rsys 9
		       | "VERSION" => rsys 10
           | "_SC_GETGR_R_SIZE_MAX" => rsys 11
           | "_SC_GETPW_R_SIZE_MAX" => rsys 12
		       | _ => raise OS.SysErr ("Not supported", NONE))
		end
	    
	    fun times () =
		let
		    val (elapsed : int,
			 utime : int,
			 stime : int,
			 cutime : int,
			 cstime : int) = (prim ("sml_times", ())) 
			handle Overflow => 
			    raiseSys "Posix.ProcEnv.times" NONE ""
		    val cps = SysWord.toInt(sysconf "CLK_TCK")
		    fun split t = (t div cps, Real.floor(1000000.0 * Real.fromInt(t mod cps)) div cps) 
		    val toTime = (fn (s,n) => Time.+(Time.fromSeconds (Int.toLarge s),
						     Time.fromMicroseconds (Int.toLarge n))) o 
			split
		in
		    {elapsed = toTime elapsed,
		     utime = toTime utime, 
		     stime = toTime stime, 
		     cutime = toTime cutime, 
		     cstime = toTime cstime}
		end

    fun isatty d = prim("@isatty", d : int) : bool

    fun isNull s = prim("__is_null",s : string) : bool

    fun ctermid () =
          let 
            val r = prim("sml_ctermid", ()) : string
          in
            if isNull r then raiseSys "Posix.ProcEnv.ctermid" NONE "" else r
          end

    fun environ () = 
          let
            val r = prim("sml_environ", ()) : string list
          in
            r
          end
    fun getegid () = prim("@getegid", ()) : int
    fun getgid () = prim("@getgid", ()) : int
    fun geteuid () = prim("@geteuid", ()) : int
    fun getuid () = prim("@getuid", ()) : int

    fun getenv a = OS.Process.getEnv a

    fun getgroups () = 
          let
            val e = OS.SysErr ("Posix.ProcEnv.getgroups", NONE)
            val (r,l) = prim("sml_getgroups", e : exn) : (int * int list)
          in
            if r = ~1 then raiseSys "Posix.ProcEnv.getgroups" NONE "" else l
          end

    fun getlogin () = 
          let
            val s = prim("sml_getlogin", ()) : string
          in
            if isNull s then raiseSys "Posix.ProcEnv.getlogin" NONE "" else s
          end

    fun getpgrp () =
           let
             val r = prim("@getpgrp", ()) : int
           in
             if r = ~1 then raiseSys "Posix.ProcEnv.getpgrp" NONE "" else r
           end

    fun getpid () = 
           let
             val r = prim("@getpid", ()) : int
           in
             if r = ~1 then raiseSys "Posix.ProcEnv.getpid" NONE "" else r
           end

    fun getppid () = 
           let
             val r = prim("@getppid", ()) : int
           in
             if r = ~1 then raiseSys "Posix.ProcEnv.getppid" NONE "" else r
           end

    fun setgid g = 
           let
             val r = prim("@setgid", g : int) : int
           in
             if r = ~1 then raiseSys "Posix.ProcEnv.setpid" NONE "" else ()
           end

    fun setsid g = 
           let
             val r = prim("@setsid", ()) : int
           in
             if r = ~1 then raiseSys "Posix.ProcEnv.setsid" NONE "" else r
           end

    fun time () = 
         let
           val (l,s,r) = prim("sml_gettime", ()) : (int * int * int)
         in
           if r = ~1 then raiseSys "Posix.ProcEnv.time" NONE ""
           else Time.+(Time.fromSeconds(LargeInt.*(LargeInt.fromInt 1000000000, (LargeInt.fromInt s))),
                       Time.fromSeconds(LargeInt.fromInt r))
         end

    fun ttyname fd = 
          let 
            val (r,s) = prim("sml_ttyname", fd : int) : (int * string)
          in
            if isNull s then raise OS.SysErr(let val err = Error.fromWord(SysWord.fromInt r) in (Error.errorMsg err, SOME err) end)
            else s
          end

    fun setuid g = 
           let
             val r = prim("@setuid", ()) : int
           in
             if r = ~1 then raiseSys "Posix.ProcEnv.setuid" NONE "" else ()
           end

    fun setpgid {pid : pid option, pgid : pid option} = 
          let
            val id = case pid of NONE => 0 | SOME p => p
            val gid = case pgid of NONE => 0 | SOME p => p
            val r = prim("@setpgid", (id,gid)) : int
          in
            if r = ~1 then raiseSys "Posix.ProcEnv.setpgid" NONE "" else ()
          end
     
    fun uname () = 
          let
            val l = prim("sml_uname", ()) : (string * string) list
          in
            if l = [] then raiseSys "Posix.ProcEnv.uname" NONE "" else l
          end
    
	end
    
    structure FileSys : POSIX_FILE_SYS
      where type file_desc = ProcEnv.file_desc
      where type uid = ProcEnv.uid 
      where type gid = ProcEnv.gid = 
	struct 
	    type uid = ProcEnv.uid
	    type gid = ProcEnv.gid
	    type file_desc = int
		
	    val stdin = Initial.Posix_File_Sys.stdin
	    val stdout = Initial.Posix_File_Sys.stdout
	    val stderr = Initial.Posix_File_Sys.stderr
		
	    structure S = 
		struct 
		    type mode = SysWord.word
		    type flags = mode
			
		    open Initial.Posix_File_Sys.S 
			
		    fun toWord x = x
		    val fromWord = toWord
		    val flags = List.foldl SysWord.orb 0wx0
		    val all = flags [irwxu,irusr,iwusr,ixusr,irwxg,irgrp,iwgrp,
				     ixgrp,irwxo,iroth,iwoth,ixoth,isuid,isgid]
		    val intersect = List.foldl SysWord.andb 0wx3FFF
		    fun clear (f1,f2) = SysWord.andb (SysWord.notb f1,f2)
		    fun allSet (f1,f2) = SysWord.andb (f1,f2) = f1
		    fun anySet (f1,f2) = SysWord.andb (f1,f2) <> 0wx0
		end
	    structure O = 
		struct
		    type flags = SysWord.word
			
		    open Initial.Posix_File_Sys.O
			
		    fun toWord x = x
		    val fromWord = toWord
		    val flags = List.foldl SysWord.orb 0wx0
		    val all = flags [append,excl,noctty,nonblock,sync,trunc]
		    val intersect = List.foldl SysWord.andb 0wx3F
		    fun clear (f1,f2) = SysWord.andb (SysWord.notb f1,f2)
		    fun allSet (f1,f2) = SysWord.andb (f1,f2) = f1
		    fun anySet (f1,f2) = SysWord.andb (f1,f2) <> 0wx0
		end
	    
	    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
	    datatype access_mode = A_READ | A_WRITE | A_EXEC

      fun iodToFD x = SOME x
      fun wordToFD x = SysWord.toInt x
      fun fdToIOD x = x
      fun fdToWord x = SysWord.fromInt x
		
	    fun lower s (name,omode,flags,mo,i,kind) = 
		let val a = prim("@sml_lower", (name : string,
						case omode of 
						    O_RDONLY => 0
						  | O_WRONLY => 1
						  | O_RDWR => 2,
							SysWord.toInt(O.toWord flags) : int,
							SysWord.toInt(S.toWord mo) : int,
							i : int,
							kind : int)) : int
		in 
		    if a = ~1 then raiseSys ("Posix.FileSys." ^ s) NONE "" else a
		end
	    
	    
	    fun createf (n,m,f,m2) = lower "createf" (n,m,f,m2, 0, 1)
	    fun openf (n,m,f) = lower "openf" (n,m,f,S.irwxo, 0, 2)
	    fun creat (name,mode) = lower "creat" (name,O_WRONLY, O.trunc, mode, 0, 1)
	    fun umask m = S.fromWord(SysWord.fromInt(lower "umask" ("", O_WRONLY, O.trunc, m, 0, 3)))
		
	    fun link {old : string, new : string} =
		let val a = prim("@link", (old,new)) : int
		in if a = ~1 then raiseSys "Posix.FileSys.link" NONE "" else ()
		end

	    fun rename {old : string, new : string} =
		let val a = prim("@rename", (old,new)) : int
		in if a = ~1 then raiseSys "Posix.FileSys.rename" NONE "" else ()
		end

	    fun symlink {old : string, new : string} =
		let val a = prim("@symlink", (old,new)) : int
		in if a = ~1 then raiseSys "Posix.FileSys.symlink" NONE "" else ()
		end

	    fun unlink (path : string) =
		let val a = prim("@unlink", path) : int
		in if a = ~1 then raiseSys "Posix.FileSys.unlink" NONE "" else ()
		end
	    
	    fun rmdir (path : string) =
		let val a = prim("@rmdir", path) : int
		in if a = ~1 then raiseSys "Posix.FileSys.rmdir" NONE "" else ()
		end

	    fun mkdir (n,s) = (lower "mkdir" (n,O_WRONLY, O.trunc, s, 0, 4); ())
	    fun mkfifo (n,s) = (lower "mkfifo" (n,O_WRONLY, O.trunc, s, 0, 5); ())

	    val readlink = OS.FileSys.readLink
		
	    fun chmod (name : string, m) = (lower "chmod" (name, O_WRONLY, O.trunc, m, 0, 6);())
	    fun fchmod (f,m) = (lower "fchmod" ("",O_WRONLY, O.trunc, m, f, 7);())

	    fun chown (name : string, uid : uid, gid : gid) = 
		let val a = prim("@chown", (name,uid,gid)) : int
		in if a = ~1 then raiseSys "Posix.FileSys.chown" NONE "" else ()
		end
	    
	    fun fchown (f : file_desc, uid : uid, gid : gid) = 
		let val a = prim("@fchown", (f,uid,gid)) : int
		in if a = ~1 then raiseSys "Posix.FileSys.fchown" NONE "" else ()
		end
	end

    structure IO : POSIX_IO
      where type pid = Process.pid 
      where type file_desc = ProcEnv.file_desc
      where type open_mode = FileSys.open_mode = 
	struct
	    type pid = int
	    type file_desc = FileSys.file_desc
      datatype open_mode = datatype FileSys.open_mode
		
	    fun pipe () = let val (r,a,b) = prim("sml_pipe",()) : (int * int * int)
			  in if r = ~1 then raiseSys "Posix.IO.pipe" NONE "" else {infd = a, outfd = b}
			  end
		      
	    fun close f = let val a = prim("@close", f : int) : int
			  in if a = ~1 then raiseSys "close" NONE "" else ()
			  end
		      
	    fun dupfd {old,base} = let val a = prim ("@sml_dupfd", (old : int,base : int)) : int
				   in if a = ~1 then raiseSys "dupfd" NONE "" else a
				   end
			       
	    fun dup f = dupfd {old = f, base = 0}

      fun readVec (fd,n) = 
           let
             val _ = if n < 0 then raise Size else ()
             val (s,j) = prim("readVec", (fd : int, n : int)) : string * int
             val _ = if j = ~1 then raiseSys "sml_readVec" NONE "" else ()
             val s' = Byte.stringToBytes s
           in
             if j = n
             then s'
             else Word8VectorSlice.vector(Word8VectorSlice.slice(s', 0, SOME j))
           end
     (* fun writeVec (fd, vs) =
           let val r = prim ("@sml_writeVec", (fd : int, vs : Word8VectorSlice.slice,
                                               Word8VectorSlice.length vs : int)) : int
           in if r = ~1 then raiseSys "sml_writeVec" NONE "" else r
           end *)
	end
    
    structure SysDB : POSIX_SYS_DB =
      struct
        type uid = ProcEnv.uid
        type gid = ProcEnv.gid

        structure Passwd =
          struct
            type passwd = {n : string, u : uid, g : gid, h : string, s : string}
            fun name ({n,...} : passwd) = n
            fun uid ({u,...} : passwd) = u
            fun gid ({g,...} : passwd) = g
            fun home ({h,...} : passwd) = h
            fun shell ({s,...} : passwd) = s
          end
        structure Group =
          struct
            type group = {n : string, g : gid, m : string list}
            fun name ({n,...} : group) = n
            fun gid ({g,...} : group) = g
            fun members ({m,...} : group) = m
          end
        fun getgrgid g = 
             let
               val s = SysWord.toInt(ProcEnv.sysconf "_SC_GETGR_R_SIZE_MAX")
               val e = OS.SysErr ("getgrgid: no group with gid = " ^ (Int.toString g) ^ " found", NONE)
               val (n,m,res) = prim("sml_getgrgid", (g : int, s : int, e : exn)) : (string * string list * int)
               val res' = Error.fromWord(SysWord.fromInt res)
             in
               if res = 0 then {n = n, m = m, g = g} else raise OS.SysErr (Error.errorName res' ^ ": "^ (Error.errorMsg res'),SOME res)
             end
        fun getgrnam n = 
             let
               val s = SysWord.toInt(ProcEnv.sysconf "_SC_GETGR_R_SIZE_MAX")
               val e = OS.SysErr ("getgrnam: no group with groupname = " ^ n ^ " found", NONE)
               val (g,m,res) = prim("sml_getgrnam", (n : string, s : int, e : exn)) : (int * string list * int)
               val res' = Error.fromWord(SysWord.fromInt res)
             in
               if res = 0 then {n = n, m = m, g = g} else raise OS.SysErr (Error.errorName res' ^ ": "^ (Error.errorMsg res'),SOME res)
             end
        fun getpwnam n = 
             let
               val s = SysWord.toInt(ProcEnv.sysconf "_SC_GETPW_R_SIZE_MAX")
               val e = OS.SysErr ("getpwnam: no user with username = " ^ n ^ " found", NONE)
               val (u,g,h,s,res) = prim("sml_getpwnam", (n : string, s : int, e : exn)) : (int * int * string * string * int)
               val res' = Error.fromWord(SysWord.fromInt res)
             in
               if res = 0 then {n = n, u = u, g = g, s = s, h = h}
               else raise OS.SysErr (Error.errorName res' ^ ": "^ (Error.errorMsg res'),SOME res)
             end
        fun getpwuid u = 
             let
               val s = SysWord.toInt(ProcEnv.sysconf "_SC_GETPW_R_SIZE_MAX")
               val e = OS.SysErr ("getpwuid: no user with uid = " ^ (Int.toString u) ^ " found", NONE)
               val (n,g,h,s,res) = prim("sml_getpwuid", (u : int, s : int, e : exn)) : (string * int * string * string * int)
               val res' = Error.fromWord(SysWord.fromInt res)
             in
               if res = 0 then {n = n, u = u, g = g, s = s, h = h}
               else raise OS.SysErr (Error.errorName res' ^ ": "^ (Error.errorMsg res'),SOME res)
             end
      end
	
    end
