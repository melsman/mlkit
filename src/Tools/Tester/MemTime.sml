signature MEM_TIME =
  sig 

    exception Crash of string
    type timings = {max_mem_size: string,
		    max_res_size: string,
		    real : string,
		    user : string,
		    sys  : string}
    val bin_directory : string ref
    val memtime : {msg : string -> unit,
		   program: string,
		   args:string list,
		   outputfile: string} -> timings
  end

structure MemTime : MEM_TIME =
  struct

    exception Crash of string
    fun die s = raise Crash ("MemTime: " ^ s)

    val bin_directory = ref "uninitialised"

    fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())

    fun execute_shell_command command =
      let val status = OS.Process.system command
      in if status = OS.Process.success then ()
	 else die ("Error no. " ^ Int.toString status
				 ^ " when executing shell command " ^ command ^ ".")
      end handle OS.SysErr(s,_) =>
	            die ("Error in executing shell command " ^ command ^ ".\n"
				 ^ "Reported error: " ^ s)
               | _ => die ("Unknown exception when executing shell command "
				   ^ command ^ ".")

    (* time_command times the command.     *)
    (* The time information is read from a *)
    (* temporary file.                     *)
    type timings = {max_mem_size: string,
		    max_res_size: string,
		    real : string,
		    user : string,
		    sys  : string}

    fun max i1 (i2:int) = if i1 > i2 then i1 else i2

    fun parseLine (s: string) : int * int =
      let val cs = explode s
	fun getc nil = NONE
	  | getc (c::cs) = SOME(c,cs)
      in case Int.scan StringCvt.DEC getc cs
	   of SOME (i1, cs) =>
	     (case Int.scan StringCvt.DEC getc cs
		of SOME (i2, _) => (i1,i2)
		 | NONE => die "parseLine2")
	    | NONE => die "parseLine1"
      end

    fun report i = (* i is in kilobytes *)
      if i > 10000 then Int.toString (i div 1000) ^ "M"
      else Int.toString i ^ "K"
    
    (* This function uses shell script memtime located in the Target
     * directory. In the future, it should be replaced by ML code. *)

    fun memtime {msg:string->unit,program:string, args:string list,outputfile:string} : timings= 
      let
	val msg = fn s => msg("    " ^ s)
	fun std () = 	      
	  let
	      val _ = msg ("executing target program: " ^ program)
(*	      val cpu_timer = Timer.startCPUTimer() *)
	      val real_timer = Timer.startRealTimer()
	      val {count,rss,size} = MemUsage.memUsage {cmd=program, args=args, delay=MemUsage.quarterSecond, 
							out_file=outputfile}
	      val max_mem_size = report size
	      val max_res_size = report rss

(*            val {usr, sys, gc} = Timer.checkCPUTimer cpu_timer *)
              val real = Timer.checkRealTimer real_timer
	  in {max_mem_size = max_mem_size, max_res_size = max_res_size, 
	      real = Time.toString real, user = "--", sys = "--"}
	    (* cpu_timer does not measure time spent in child processes, hence it
	     does not work for system calls, which are forked. *)
	  end

      in case arch_os()
	   of ("HPPA", "HPUX") =>
	     let (* We use shell script for HPUX. *)
	       val tempfile = "timex.temp"
	       val memtime_exe = OS.Path.joinDirFile{dir= !bin_directory, file="memtime_hpux"}
	       val _ = (msg "executing target program: ";
			msg ("memtime_hpux -f " ^ outputfile ^ " -o " ^ tempfile ^ " " ^ program))
	       val shell_command = memtime_exe ^ " -f " ^ outputfile ^ " -o " ^ tempfile ^ " " ^ program

	       val _ = execute_shell_command shell_command

	       fun scanToNL getc src =
		 let fun loop (acc,src) =
		       case getc src
			 of SOME (#"\n",src) => SOME(implode(rev acc),src)
			  | SOME (c,src) => loop(c::acc,src)
			  | NONE => NONE
		 in loop ([],src)
		 end
	       
	       fun scanEntry l getc src =
		 let (* first read chars *)
		   fun loop ([],src) = SOME src
		     | loop (c::cs,src) = case getc src
					    of NONE => NONE
					     | SOME(c',src) => if c=c' then loop(cs,src)
							       else NONE
		   val src = StringCvt.skipWS getc src
		 in case loop(l,src)
		      of NONE => NONE
		       | SOME src => scanToNL getc src
		 end
	       
	       val input_stream = TextIO.openIn tempfile
		 
	       fun input_value name_of_value =
		 case TextIO.scanStream (scanEntry (explode name_of_value)) input_stream
		   of SOME res => res
		    | NONE => raise Crash ("I could not read " ^ name_of_value ^ " entry from temp file.")
		     
	       val max_mem_size = input_value "MAX MEM SIZE:"
	       val max_res_size = input_value "MAX MEM RES:"
	       val real = input_value "Real time:"
	       val user = input_value "User time:"
	       val sys  = input_value "System time:"
	       val _ = TextIO.closeIn input_stream
	     in
	       {max_mem_size = max_mem_size, max_res_size = max_res_size, 
		real = real, user = user, sys = sys}
	     end
	    | ("SUN", "OS4") => std()
	    | ("SPARC", "Solaris") => std()
	    | ("X86", "Linux") => std()
	    | (arch,os) => raise Crash ("Error in memtime, " ^ arch ^ "-" ^ os ^ " not supported.")
      end  handle IO.Io {name=s,...} => raise Crash ("Error in memtime: " ^ s)
  end

