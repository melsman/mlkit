val _ = SMLofNJ.Internals.GC.messages false;

local val srcdir = OS.FileSys.getDir()
in fun cdsrc() = OS.FileSys.chDir srcdir; 
   val cd = OS.FileSys.chDir
   val pwd = OS.FileSys.getDir
end;

val _ =
let

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

  val upper = implode o map Char.toUpper o explode


  fun error s = (print "\n ** Error : "; print s; print " **\n")

  exception Die
  fun die s = (error s;
	       cdsrc();
	       print " ** Installation procedure terminated ** \n";
	       raise Die)
    
  fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
  
  fun available_with_C_backend() =
    case arch_os()
      of ("X86", "Linux") => false
       | ("HPPA", "HPUX") => true
       | ("SUN", "OS4") => false
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
		of ("HPPA", "HPUX") => ()
		 | _ => (error "the native backend is only available for the HPPA-HPUX system"; 
			 resolve_backend()))
 	  | _ => (error "you must type C or native"; 
		  resolve_backend())
    end

  fun build_runtime() =
    let val _ = print "\n ** Building runtime system **\n\n"
    in cd "Runtime/Version17";
      if OS.Process.system "gmake" = OS.Process.success then cdsrc()
      else die "build_runtime: gmake failed"
    end


  fun build_rp2ps() =
    let val _ = print "\n ** Building profiling tool rp2ps **\n\n"
        val ext = case arch_os()
		    of ("HPPA", "HPUX") => "HPUX"
		     | _ => die "build_rp2ps"
    in cd "Runtime/Version17/Rp2ps";
      if OS.Process.system ("gmake rp2ps" ^ ext) = OS.Process.success then cdsrc()
      else die "build_rp2ps: gmake failed"
    end
      

  fun build_kit() = 
    (print "\n ** Building the ML Kit compiler **\n\n";
     CM.make())

in
  print "\n\n ** ML Kit with Regions installation ** \n\n";  
  resolve_backend();
  build_runtime();
  build_rp2ps();
  build_kit()
end ;

(*
val _ = K.Flags.lookup_flag_entry "delete_target_files" := false;
*)

(*
val _ = K.build_basislib()

val _ = K.install();
*)

