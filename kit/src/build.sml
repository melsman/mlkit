val _ = SMLofNJ.Internals.GC.messages false;

local val srcdir = OS.FileSys.getDir()
in val cd = OS.FileSys.chDir
   fun cdsrc() = cd srcdir; 
   val pwd = OS.FileSys.getDir
   fun mk() = (cdsrc(); CM.make())
end;

val _ =
let

  fun error s = (print "\n ** Error : "; print s; print " **\n")

  exception Die
  fun die s = (error s;
	       cdsrc();
	       print " ** Installation procedure terminated ** \n";
	       raise Die)

  (* Read characters until a newline is found. *)
  fun read_string s =
    let
      val ch = TextIO.inputN(TextIO.stdIn, 1)
    in
      if ch = "\n" then
        s
      else
        read_string (s^ch)
    end;

  (* If we can't open the .config file then assume it is non existing. *)
  (* Assume we are in the source directory.                            *)
  fun load_config_file(file_name) =
    let
      val file_strm = TextIO.openIn(file_name)
      val arch_info = TextIO.inputLine(file_strm)
      val os_info = TextIO.inputLine(file_strm)
      val _ = TextIO.closeIn(file_strm)
    in
      (* Remove the newlines *)
      (String.extract(arch_info, 0, SOME (String.size arch_info-1)), 
       String.extract(os_info, 0, SOME (String.size os_info-1)))
    end
  handle _ => ("unknown", "unknown")

  (* Save the .config file *)
  (* Assume we are in the source directory. *)
  fun save_config_file() =
    let
      val file_strm = TextIO.openOut(".config")
      val _ = TextIO.output(file_strm, SMLofNJ.SysInfo.getHostArch()^"\n")
      val _ = TextIO.output(file_strm, SMLofNJ.SysInfo.getOSName())
      val _ = TextIO.closeOut(file_strm)
    in
      ()
    end
  handle _ => die "save_config_file: can't open or write to file."

  val upper = implode o map Char.toUpper o explode
    
  fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
  
  fun available_with_C_backend() =
    case arch_os()
      of ("X86", "Linux") => true
       | ("HPPA", "HPUX") => true
       | ("SPARC", "Solaris") => true
       | _ => false

  fun warn_cross (s:string) : string option =
    (print "Do you really want to generate code for a platform\n\
            \different from the platform you are now working\n\
            \on? (type yes or no) ";
     (case (upper (read_string ""))
	of "YES" => SOME s
         | _ => NONE))

  fun check_cross s =
    case s
      of "C" => if available_with_C_backend() then SOME s
		else warn_cross s
       | "X86" => if #1(arch_os()) = "X86" then SOME s else warn_cross s
       | "HPPA" => if #1(arch_os()) = "HPPA" then SOME s else warn_cross s
       | "PAML" => warn_cross s 
       | "DUMMY" => warn_cross s
       | _ => NONE

  fun resolve_backend () =
    let val _ = print "\nWhich platform do you want to use as target? (type\n\
                       \HPPA, X86, C, or PaML):"
    in 
      case check_cross (upper (read_string ""))
	of SOME "C" => (* set a symbol for the compilation manager CM *)
	  CM.SymVal.define("KIT_TARGET_C", 1)  
         | SOME "HPPA" => CM.SymVal.define("KIT_TARGET_HPPA", 1)
	 | SOME "X86" => CM.SymVal.define("KIT_TARGET_X86", 1)
	 | SOME "PAML" => CM.SymVal.define("KIT_TARGET_PAML", 1)
	 | SOME "DUMMY" => CM.SymVal.define("KIT_TARGET_DUMMY", 1)
	 | _ => (error "try again..."; resolve_backend())
    end

  fun build_runtime(runtime_path) =
      (print "\n ** Building runtime system **\n\n";
       cd runtime_path;
       if (load_config_file("../.config") <> arch_os()) then
	 (if OS.Process.system ("gmake clean") = OS.Process.success then ()
	  else die "build_runtime: gmake clean failed";
	  if OS.Process.system ("gmake depend") = OS.Process.success then ()
	  else die "build_runtime: gmake depend failed")
       else ();
       if OS.Process.system "gmake" = OS.Process.success then ()
       else die "build_runtime: gmake failed";
       cdsrc())

  fun build_rp2ps() =
    (print "\n ** Building profiling tool rp2ps **\n\n";
     cd "Tools/Rp2ps";
     if (load_config_file("../../.config") <> arch_os()) then
       (if OS.Process.system ("gmake clean") = OS.Process.success then ()
	else die "build_rp2ps: gmake clean failed";
	if OS.Process.system ("gmake depend") = OS.Process.success then ()
	else die "build_rp2ps: gmake depend failed")
     else ();
     if OS.Process.system ("gmake") = OS.Process.success then () 
     else die "build_rp2ps: gmake failed";
     cdsrc();
     if OS.Process.system "cp Tools/Rp2ps/rp2ps ../bin/rp2ps" = OS.Process.success then ()
     else die "build_rp2ps: failed to install rp2ps")

  fun build_kittester() =
    (print "\n ** Building kittester, a program for testing the Kit **\n\n";
     cd "Tools/Tester";
     if OS.Process.system ("echo 'CM.make();' | sml") = OS.Process.success then () 
     else die "build_kittester: ``echo 'CM.make();' | sml'' failed";
     cdsrc())
      
  fun build_kit() = 
    (print "\n ** Building the ML Kit compiler **\n\n";
     CM.make())
in
  print "\n\n ** ML Kit with Regions installation ** \n\n";  
  resolve_backend();
  build_runtime("Runtime");
  build_runtime("RuntimeWithGC");
  build_rp2ps();
  build_kittester();
  save_config_file();
  build_kit()
end ;

(* This is only temporary; 09/02/1999, Niels *)
val _ = 
  let
    fun enable s = K.Flags.lookup_flag_entry s := true
    fun disable s =  K.Flags.lookup_flag_entry s := false
    fun error s = (print "\n ** Error : "; print s; print " **\n")
    val upper = implode o map Char.toUpper o explode
    (* Read characters until a newline is found. *)
    fun read_string s =
      let
	val ch = TextIO.inputN(TextIO.stdIn, 1)
      in
	if ch = "\n" then
	  s
	else
	  read_string (s^ch)
      end
    fun choose_backend() =
      let 
	fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
	val (arch,os) = arch_os()

	val _ = print("\nDo you want to use the new backend?\n\
	              \If you say yes then your previous selection is overruled.\n\
                      \Target system is " ^ arch ^ "-" ^ os ^ " (type yes or no): ")
	val profflags = ["region_profiling", "print_program_points",
			 "print_call_explicit_expression", "log_to_file"]
      in 
	case explode (upper (read_string ""))
	  of #"Y" :: #"E" :: #"S" :: _ => 
	    (enable "enable_lambda_backend";
	     disable "garbage_collection";
 	     disable "delete_target_files";
	     K.build_basislib();
	     K.install())
	   | #"N" :: #"O" :: _ => 
	    (disable "enable_lambda_backend";
	     disable "garbage_collection";
	     K.build_basislib();
	     app enable profflags;
	     K.build_basislib();
	     app disable profflags;
	     K.install())
	   | _ => (error "you must type yes or no"; 
		   choose_backend())
      end
  in
    choose_backend()
  end;