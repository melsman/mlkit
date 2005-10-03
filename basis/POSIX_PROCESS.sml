signature POSIX_PROCESS =
    sig
	eqtype pid
	type signal

	datatype exit_status
	  = W_EXITED
	  | W_EXITSTATUS of Word8.word
	  | W_SIGNALED of signal
	  | W_STOPPED of signal

	datatype waitpid_arg
	  = W_ANY_CHILD | W_CHILD of pid | W_GROUP of pid | W_SAME_GROUP

	structure W :
	    sig
		type flags
	    end
	
	val fork : unit -> pid option
	val exit : Word8.word -> 'a	    
	val waitpid : waitpid_arg * W.flags list -> pid * exit_status

    end