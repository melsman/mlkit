signature EXEC_PROFILE =
  sig  
    val main : string * string list -> OS.Process.status
  end

structure ExecProfile : EXEC_PROFILE =
  struct
    val _ = SMLofNJ.Internals.GC.messages false;

    fun process_args [exec,outfile] = SOME (exec,outfile)
      | process_args _ = NONE

    fun print_usage progname = print("\nUsage: progname exec outfile\n\
				     \  exec: path to executable\n\
				     \  outfile: output from executing prog_name is redirected to outfile.\n")
				     
    fun main (progname, args) =
      case process_args args of 
        SOME (exec,outfile) =>
	  (print (MemUsage.pp_report (MemUsage.memUsage {cmd=exec,args=nil,out_file=outfile}));
	   OS.Process.success)
      | NONE => (print_usage progname; OS.Process.failure)

    fun install() =
      let val _ = print "\n ** Installing ExecProfile, a tool for simple memory and time profiling **\n\n"

	  val kit_src_tools_tester_path = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/Tester directory *)
	  val kit_bin_path = OS.Path.mkCanonical (OS.Path.concat(kit_src_tools_tester_path, "../../../bin"))
	  val kit_bin_execprofile_path = OS.Path.joinDirFile{dir=kit_bin_path, file="execprofile"}

          fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
	  fun execprofile_image() =
	    case arch_os()
	      of ("X86", "Linux") => "execprofile.x86-linux"
	       | ("HPPA", "HPUX") => "execprofile.hppa-hpux"
	       | ("SPARC", "Solaris") => "execprofile.sparc-solaris"
	       | ("SUN", "OS4") => "unknown"
	       | _ => "unknown"
	  val kit_bin_execprofile_image_path = OS.Path.joinDirFile{dir=kit_bin_path, file=execprofile_image()}
	  val os = TextIO.openOut kit_bin_execprofile_path
	  val _ = (TextIO.output(os, "sml @SMLload=" ^ kit_bin_execprofile_image_path ^ " $*"); TextIO.closeOut os)
	  val _ = OS.Process.system("chmod a+x " ^ kit_bin_execprofile_path)
	    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ kit_bin_execprofile_path 
			       ^ "' failed***\n");
			 OS.Process.failure)
      in SMLofNJ.exportFn(kit_bin_execprofile_path,main)
      end

    val _ = install()

  end
