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
		type signal = int

		datatype exit_status
	          = W_EXITED
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
		    prim("sml_WTERMSIG",status)

		fun WSTOPSIG(status:int) : signal =
		    prim("sml_WSTOPSIG",status)

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

     fun exec (s : string, sl : string list) = let val a = prim("sml_exec", (s,sl)) : int
                                               in if a = 0 
                                                  then raiseSysML "exec" NONE ""
                                                  else raiseSys "exec" NONE ""
                                               end

	    end
    
    structure ProcEnv : POSIX_PROCENV = 
      struct 
        type uid = int
        type gid = int
        type pid = int
        type file_desc = int
        fun sysconf (s : string) =
            let
              fun rsys i = (prim ("sml_sysconf", i : int) : int)
                  handle Overflow => raise OS.SysErr ("Not supported", NONE)
            in
              SysWord.fromInt (
                case s 
                of "ARG_MAX" => rsys 1
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
              fun split t = (t div cps, (1000000000 div cps) * (t mod cps))
              val toTime = (fn (s,n) => Time.+(Time.fromSeconds s,Time.fromMicroseconds n)) o 
                                        split
            in
              {elapsed = toTime elapsed,
               utime = toTime utime, 
               stime = toTime stime, 
               cutime = toTime cutime, 
               cstime = toTime cstime}
            end
      end

    structure FileSys : POSIX_FILE_SYS = 
      struct 
        type uid = ProcEnv.uid
        type gid = ProcEnv.gid
        type file_desc = int

        val stdin = Initial.stdIn_stream
        val stdout = Initial.stdOut_stream
        val stderr = Initial.stdErr_stream

        structure S = 
          struct 
            type mode = SysWord.word
            type flags = mode

            open Posix_File_Sys.S 

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
             
            open Posix_File_Sys.O

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

        fun createf (name,omode,flags,mo) = 
            let val a = prim("@sml_open", (name : string,
                                  case omode of O_RDONLY => 0
                                              | O_WRONLY => 1
                                              | O_RDWR => 2,
                                  SysWord.toInt(O.toWord flags),
                                  SysWord.toInt(S.toWord mo))) : int
            in 
              if a = ~1 then raiseSys "createf" NONE "" else a
            end
                                                                  
        fun creat (name,mode) = createf(name,O_WRONLY, O.trunc, mode)
      end

    structure IO : POSIX_IO = 
      struct
        type pid = int
        type file_desc = FileSys.file_desc
        type open_mode = FileSys.open_mode

        fun close f = let val a = prim("@close", f : int) : int
                      in if a = ~1 then raiseSys "close" NONE "" else ()
                      end

        fun dupfd {old,base} = let val a = prim ("@sml_dupfd", (old : int,base : int)) : int
                               in if a = ~1 then raiseSys "dupfd" NONE "" else a
                               end

        fun dup f = dupfd {old = f, base = 0}
      end
	
    end
