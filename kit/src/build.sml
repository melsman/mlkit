val _ = SMLofNJ.Internals.GC.messages false;

local val srcdir = OS.FileSys.getDir()
in val cd = OS.FileSys.chDir
   fun cdsrc() = cd srcdir; 
   val pwd = OS.FileSys.getDir
   fun mk() = (cdsrc(); CM.make())
end;

local

  exception Die
  fun die s = (print " **  DIE: Installation procedure terminated.  ** \n";
	       cdsrc(); raise Die)

  fun build_runtime(runtime_path) =
      (print "\n **  Building runtime system  **\n\n";
       cd runtime_path;
       if OS.Process.system "gmake" = OS.Process.success then ()
       else die "build_runtime: gmake failed";
       cdsrc())

  fun build_rp2ps() =
    (print "\n ** Building profiling tool rp2ps **\n\n";
     cd "Tools/Rp2ps";
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
  val _ = print "\n\n ** ML Kit with Regions installation ** \n\n";  
  val _ = 
	((* build_runtime("RuntimePaML");
	 build_runtime("Runtime"); *)
	 build_runtime("RuntimeWithGC");
(*
	 build_rp2ps();
	 build_kittester();
*)
	 build_kit())
end ;


(* This is only temporary; 09/02/1999, Niels *)
val _ = 
  let
    fun enable s = KitX86.Flags.lookup_flag_entry s := true
    fun disable s =  KitX86.Flags.lookup_flag_entry s := false
  in 
      disable "garbage_collection";
      disable "delete_target_files";
      KitX86.build_basislib()
(*      ; KitX86.install() *)
  end;