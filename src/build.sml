val _ = SMLofNJ.Internals.GC.messages false;

local val srcdir = OS.FileSys.getDir()
in val cd = OS.FileSys.chDir
   fun cdsrc() = cd srcdir; 
   val pwd = OS.FileSys.getDir
   fun mk() = (cdsrc(); CM.make())
   fun get_src_dir() = srcdir
end;

local

(*
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

  fun build_kitgen_opcodes() =
    (print "\n ** Building kitgen_opcodes, a program for generating source files for opcodes and \n";
     print " ** built in C primitives **\n\n";
     cd "Tools/GenOpcodes";
     if OS.Process.system ("echo 'CM.make();' | sml") = OS.Process.success then () 
     else die "build_kitgen_opcodes: ``echo 'CM.make();' | sml'' failed";
     cdsrc())

  fun gen_opcodes() =
    (print "\n ** Building source files for opcodes and built in C primitives ** \n\n";
     cd "../bin";
     if OS.Process.system ("kitgen_opcodes " ^ (get_src_dir())) = OS.Process.success then 
       ()
     else 
       die "gen_opcodes failed";
     cdsrc())
*)
      
  fun build_kit() = 
    (print "\n ** Building the ML Kit compiler **\n\n";
     CM.make())
in
  val _ = print "\n\n ** ML Kit with Regions installation ** \n\n";  
  val _ = 
	((* build_runtime("RuntimePaML");
	 build_runtime("Runtime");
	 build_kitgen_opcodes();
	 gen_opcodes();
	 build_runtime("RuntimeWithGC");
	 build_rp2ps();
	 build_kittester();
*)
	 build_kit())
end ;


(* This is only temporary; 09/02/1999, Niels *)
fun build_x86 () = 
  let
    fun enable s = KitX86.Flags.lookup_flag_entry s := true
    fun disable s =  KitX86.Flags.lookup_flag_entry s := false
  in 
    disable "garbage_collection";
    disable "delete_target_files";

    disable "optimiser";
    disable "unbox_function_arguments";
    disable "use_flow_variables"; 

    disable "print_call_explicit_expression";
    disable "print_clos_conv_program";
    disable "print_calc_offset_program";
    disable "print_simplified_program";

(*    KitX86.build_basislib();*)
    enable "auto_import_basislib" ;
    KitX86.install() 
  end;


(* This is only temporary; 09/02/1999, Niels *)
fun build_kam () =
  let
    fun enable s = KitKAM.Flags.lookup_flag_entry s := true
    fun disable s =  KitKAM.Flags.lookup_flag_entry s := false
    val opts = ["statistics_after_optimisation","minimize_fixs","fix_conversion",
		(*"contract",*) "specialize_recursive_functions", "eliminate_explicit_records",
		"unbox_function_arguments"]
    val test_files = ["f1.sml", "f2.sml", "fib.sml", "foldl.sml", 
		      "hanoi.sml", "hello.sml", "if.sml", "immedString.sml", 
		      "l1.sml", "list_nh.sml"(*, "listpair.sml"*), "ref.sml", "string1.sml"]
  in 
    disable "garbage_collection";
    disable "delete_target_files";
    disable "unbox_function_arguments";
    disable "link_time_dead_code_elimination";
(*      List.app disable opts;*)
    enable "delay_assembly";
    enable "smlserver";
(*      enable "chat"; *)
    KitKAM.Flags.target_file_extension := ".uo";

  (*   ; List.app (fn tf => KitKAM.comp ("../test_dev/"^tf)) test_files *)

(*      ; disable "auto_import_basislib"*)

(*      ; disable "jump_tables" *)

    KitKAM.Flags.lookup_string_entry "kam_uolistfile" := "basislib.ul";
    KitKAM.build_basislib() ;
    KitKAM.Flags.lookup_string_entry "kam_uolistfile" := "";

(*    enable "print_kam_program" ;*)
(*    enable "print_lift_conv_program";*)

(*    KitKAM.comp "../test_dev/real_problem.sml"*)

    KitKAM.install() 
  end;

(*val _ = build_kam() *)
val _ = build_x86()
