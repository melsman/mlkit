structure Posix :> POSIX =
    struct
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
	    end
	
    end