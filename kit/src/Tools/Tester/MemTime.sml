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
		   outputfile: string} -> timings
  end

structure MemTime : MEM_TIME =
  struct

    exception Crash of string

    val bin_directory = ref "uninitialised"

    fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())

    fun execute_shell_command command =
      let val status = OS.Process.system command
      in if status = OS.Process.success then ()
	 else raise Crash ("Error no. " ^ Int.toString status
				 ^ " when executing shell command " ^ command ^ ".")
      end handle OS.SysErr(s,_) =>
	            raise Crash ("Error in executing shell command " ^ command ^ ".\n"
				 ^ "Reported error: " ^ s)
               | _ => raise Crash ("Unknown exception when executing shell command "
				   ^ command ^ ".")

    (* time_command times the command.     *)
    (* The time information is read from a *)
    (* temporary file.                     *)
    type timings = {max_mem_size: string,
		    max_res_size: string,
		    real : string,
		    user : string,
		    sys  : string}

    (* This function uses shell script memtime located in the *)
    (* Target directory. In future, it should be replaced by  *)
    (* ML code.                                               *)
    fun memtime {msg:string->unit,program:string, outputfile:string} : timings= 
      let
	val msg = fn s => msg("    " ^ s)
	val tempfile = "timex.temp"
	val shell_command =
	  case arch_os()
	    of ("HPPA", "HPUX") =>
	      let (* We use shell script for HPUX. *)
		val memtime_exe = OS.Path.joinDirFile{dir= !bin_directory, file="memtime_hpux"}
		val _ = (msg "executing target program: ";
			 msg ("memtime_hpux -f " ^ outputfile ^ " -o " ^ tempfile ^ " " ^ program))
	      in
		memtime_exe ^ " -f " ^ outputfile ^ " -o " ^ tempfile ^ " " ^ program
	      end
	     | ("SUN", "OS4") =>
	      let (* We use shell script for SUN_OS4. *)
		val memtime_exe = OS.Path.joinDirFile{dir= !bin_directory, file="memtime_sun_os4"}
		val temp = "time.temp"
		val _ = (msg "executing target program: ";
			 msg ("memtime_sun_os4 -f " ^ outputfile ^ " -o " ^ tempfile ^ " -t " ^  temp ^ " " ^ program))
	      in 
		memtime_exe ^ " -f " ^ outputfile ^ " -o " ^ tempfile ^ " -t " ^ temp ^ " " ^ program
	      end
	     | ("SPARC", "Solaris") =>
	      let (* We use shell script for SUN_OS4. *)
		val memtime_exe = OS.Path.joinDirFile{dir= !bin_directory, file="memtime_sun_os4"}
		val temp = "time.temp"
		val _ = (msg "executing target program: ";
			 msg ("memtime_sun_os4 -f " ^ outputfile ^ " -o " ^ tempfile ^ " -t " ^  temp ^ " " ^ program))
	      in 
		memtime_exe ^ " -f " ^ outputfile ^ " -o " ^ tempfile ^ " -t " ^ temp ^ " " ^ program
	      end
	     | (arch,os) => raise Crash ("Error in memtime, " ^ arch ^ "-" ^ os ^ " not supported.")

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
    handle IO.Io {name=s,...} => raise Crash ("Error in memtime: " ^ s)
  end