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

  fun resolve_backend () =
    let val _ = print "\nDo you want to use the C backend or the native backend?\n\
	               \The native backend is only available under the HPPA-HPUX\n\
                       \system (type C or native): "
    in case explode (upper (read_string ""))
	 of #"C" :: _ => 
	   if available_with_C_backend() then CM.SymVal.define("KIT_TARGET_C", 1)  (* set a symbol for the compilation manager *)
	   else die "the Kit does not work with your system"
	  | #"N" :: #"A" :: #"T" :: #"I" :: #"V" :: #"E" :: _ =>  (* check that the architecture is HP PA-RISC and that
								   * the operating system is HPUX. *)
	     (case arch_os()
		of ("HPPA", "HPUX") => (CM.SymVal.define("KIT_TARGET_HPPA", 1))
		 | _ => (error "the native backend is only available for the HPPA-HPUX system"; 
			 resolve_backend()))
 	  | _ => (error "you must type C or native"; 
		  resolve_backend())
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
    fun maybe_enable_lambda_backend() =
      let 
	val _ = print "\nDo you want to use the new backend?\n\
	              \If you say yes then your previous selection is overruled.\n\
                      \Target system is HP PA-RISC (type yes or no): "
      in 
	case explode (upper (read_string ""))
	  of #"Y" :: #"E" :: #"S" :: _ => 
	    (K.Flags.lookup_flag_entry "enable_lambda_backend" := true;
	     K.Flags.lookup_flag_entry "garbage_collection" := false;
 	     K.Flags.lookup_flag_entry "delete_target_files" := false)
	   | #"N" :: #"O" :: _ => 
	    (K.Flags.lookup_flag_entry "enable_lambda_backend" := false;
	     K.Flags.lookup_flag_entry "garbage_collection" := false)
	   | _ => (error "you must type yes or no"; 
		   maybe_enable_lambda_backend())
      end
  in
    maybe_enable_lambda_backend()
  end;

val _ = 
  let fun enable s = K.Flags.lookup_flag_entry s := true
      fun disable s =  K.Flags.lookup_flag_entry s := false
      val profflags = ["region_profiling", "print_program_points",
		       "print_call_explicit_expression", "log_to_file"]
  in
    K.build_basislib();
(*
    if !(K.Flags.lookup_flag_entry "enable_lambda_backend") then  (* This is only temporary; 09/02/1999, Niels *)
      (enable "garbage_collection";
       K.build_basislib();
       disable "garbage_collection")
    else
      ();
*)
    if not (!(K.Flags.lookup_flag_entry "enable_lambda_backend")) then  (* This is only temporary; 09/02/1999, Niels *)
      (app enable profflags;
       K.build_basislib();
       app disable profflags)
    else
      ()
  end

val _ = K.install();


