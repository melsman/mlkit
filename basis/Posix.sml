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

    structure Process =
	struct
	    type pid = int
	    type signal = SysWord.word
		
	    datatype exit_status = 
		W_EXITED
	      | W_EXITSTATUS of Word8.word
	      | W_SIGNALED of signal
	      | W_STOPPED of signal
		
	    datatype waitpid_arg
		= W_ANY_CHILD | W_CHILD of pid | W_GROUP of pid | W_SAME_GROUP
		
	    structure W =
		struct
		    type flags = unit
		end
	    
	    val pidToWord = SysWord.fromInt
		
	    fun fork() : pid option =
		let val ret : int = prim("@fork",())
		in if ret < 0 then raise Fail "Posix.fork"
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
		let val i:int = prim("sml_WTERMSIG",status)
		in SysWord.fromInt i
		end

	    fun WSTOPSIG(status:int) : signal =
		let val i:int = prim("sml_WSTOPSIG",status)
		in SysWord.fromInt i
		end

	    fun flags_to_int _ = 0

	    fun waitpid (wpa: waitpid_arg,flags: W.flags list) : pid * exit_status =
		let val (pid:int,status) = 
		    prim("sml_waitpid",(waitpid_arg_to_int wpa,flags_to_int flags))
		in if pid = 0 then raise Fail "waitpid error"
		   else 
		       let 
			   val es = 
			       if WIFSIGNALED(status) then W_SIGNALED(WTERMSIG(status))
			       else 
				   if WIFEXITED(status) then 
				       let val e = WEXITSTATUS(status)
				       in if e = 0w0 then W_EXITED
					  else W_EXITSTATUS e
				       end
				   else if WIFSTOPPED(status) then W_STOPPED(WSTOPSIG(status))
					else raise Fail "waitpid.error2"
					    
		       in (pid,es)
		       end	    
		end
	    
	    fun wait () = waitpid (W_ANY_CHILD, [])
		
	    fun exec (s : string, sl : string list) = 
		let val a = prim("sml_exec", (s,sl)) : int
		in if a = 0 then 
		    raiseSysML "exec" NONE ""
		   else raiseSys "exec" NONE ""
		end	    
	end
	
    structure ProcEnv : POSIX_PROCENV 
      where type pid = Process.pid = 
	struct 
	    type uid = int
	    type gid = int
	    type pid = Process.pid
	    type file_desc = int
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
    
    structure Signal : POSIX_SIGNAL =
	struct
	    type signal = SysWord.word
		
	    val toWord   : signal -> SysWord.word = fn x => x
	    val fromWord : SysWord.word -> signal = fn x => x
	end
	
    end
