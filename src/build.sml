
(*build.sml*)

(* This file builds an ML Kit from a consult file and exports an image into
 the /bin directory.  A scriptfile (kit.script) matching the backend and the
 architecture is put in the same directory.  Symbolic links to the runtime
 system are also generated.  The version of the runtime system to use is
 "hard" coded in this build file, see runtime_directory below.  The build
 file does _not_ compile the runtime system.  To do that go into the runtime
 directory and type:

   make runtimeHPUX

 and an object file runtimeHPUX.o is generated.  See the makefile in the
 runtime directory to see how to compile the other versions of the runtime
 system.  The runtime system must be compiled before build.sml is used.
 There are four possible runtime systems files, two for each architecture
 (one with profiling and one without).*)

(* use of kit: kitversion [-script name] [-tempfile name] [-dir name]*)

open Make;

val release = "2.0"
val header_with_release = 
         "\n\n\n\n               The ML Kit with Regions, release: " 
       ^ release ^ "\n\n\n"

val (export_file, kit_name, path_to_kit_script, kit_version) =

(*Unfortunately, export_file, kit_name, path_to_kit_script, and
 kit_version have to be declared at top level, as they are needed
 after the kit has been exported.*)

let
  (* Functions to generate script file. *)
  fun init_script_file out_stream =
    (output (out_stream, "(* ------------------------------------------------------------------------\n");
     output (out_stream, " * ML Kit script file.                                                     \n");
     output (out_stream, " *                                                                         \n");
     output (out_stream, " *                                                                         \n");
     output (out_stream, " * syntax:                                                                 \n");
     output (out_stream, " *                                                                         \n");
     output (out_stream, " *   DEC::= val ID : TYPE  = CONST REST                                    \n");
     output (out_stream, " *   REST::= ;                                                             \n");
     output (out_stream, " *       |  DEC                                                            \n");
     output (out_stream, " *   ID  ::= sequence_of_letters_underbars_and_primes                      \n");
     output (out_stream, " *   TYPE::= int | string | bool                                           \n");
     output (out_stream, " *   CONST::= ml_integer_constant | ml_string_constant | ml_bool_constant  \n");
     output (out_stream, " *                                                                         \n");
     output (out_stream, " *   blanks, tabs and newlines are separators;                             \n");
     output (out_stream, " *   comments are enclosed in (* and *) and can be nested.                 \n");
     output (out_stream, " * ------------------------------------------------------------------------- *)\n\n\n"))

  fun end_script_file out_stream = output(out_stream, "\n\n;\n\n")
    
  fun insert_comment_script_file out_stream comment =
    output (out_stream, ("\n(* " ^ comment ^ " *)\n"))

  fun insert_string_flag out_stream name_of_flag value =
    output (out_stream, ("val " ^ name_of_flag ^ " : string = \"" ^ value ^ "\"\n"))

  fun insert_bool_flag out_stream name_of_flag true  = output (out_stream, ("val " ^ name_of_flag ^ " : bool = true\n"))
    | insert_bool_flag out_stream name_of_flag false = output (out_stream, ("val " ^ name_of_flag ^ " : bool = false\n"))

  fun insert_int_flag out_stream name_of_flag value =
    output (out_stream, ("val " ^ name_of_flag ^ " : int = " ^ Int.string value ^ "\n"))

  (* Read characters until a newline is found. *)
  fun read_string s =
    let
      val ch = input(std_in, 1)
    in
      if ch = "\n" then
        s
      else
        read_string (s^ch)
    end;

  val upper = implode o map String.upper o explode
  fun user_types_y () = upper (read_string "") = "Y"
  fun wait_for_return () = (read_string ""; ())
    
  (* Create directory d, with specified acces rights. *)
  val access_rights = 505
  fun create_dir d = SML_NJ.Unsafe.SysIO.mkdir (d,access_rights)
    
  (* Create a symbolic link: dest --> src *)
  fun mk_sym_link src dest =
    SML_NJ.Unsafe.SysIO.symlink (src,dest)
    handle _ =>
      (print ("\nCould not create the symbolic link\n\n\
               \   \"" ^ dest ^ "\".\n\n\
	       \(maybe it already exists?).  Press return to continue ");
       wait_for_return ())
       

  (* Return true if file or directory d exists. *)
  fun exists_file d = SML_NJ.Unsafe.SysIO.access (d,[])

  (* Find a temp file to use when building the ML Kit. *)
  fun gen_tmp_file name =
    if exists_file (name) then
      gen_tmp_file (name^"%")
    else
      name

  fun exit () = (print "exiting...\n";
		 SML_NJ.Unsafe.CInterface.exit 0)

  (*-------------------------------------------*)
  (* Initialize version, name and script file. *)
  (*-------------------------------------------*)

  (* Where to put the ML Kit. *)
  val _ = print "\n\nYou have now installed the source files for the ML Kit."
  val _ = print "\nTo build the ML Kit you have to type in the path to the"
  val _ = print "\ndirectory where the file \"roadmap\" is located"
  val _ = print "\n(e.g. /usr/local/topps/MLKit/):" 
  val ml_kit_dir = read_string ""
(*KILL 30/03/1997 15:09. tho.:
  val _ = print "\n\nIs The ML Kit installed in directory:"
  val _ = print ("\n    " ^ ml_kit_dir ^ "? (y/n):")
  val _ =
    if read_string "" <> "y" then
      exit ()
    else
      ()
*)

  (* Which consult file shall we use. *)
  val _ = print "\n\nA new version of The ML Kit will be generated."
  val _ = print "\nDo you want the Kit to generate ANSI C or HP PA-RISC code?"
  val _ = print "\n(type C or HPPA): "
  val consult_file_version = upper (read_string "")

  (* Do we know the backend. *)
  val backend = 
    case explode consult_file_version
      of "C" :: rest => "C"
       | "H"::"P"::"P"::"A"::rest => "HPPA"
       | _ => (print ("I do not know backend " ^ consult_file_version ^ "\n");
	       exit ())

  (* Which architecture to use. *)  
  val _ = print "\nDo you want the Kit to run on SUN_OS4 or HPUX? "
  val kit_architecture = upper (read_string "")

  (* Do we know the architecture. *)  
  val (c_compiler,c_libs) = 
    case kit_architecture
      of "HPUX" => ("cc -Aa", "-lm")
       | "SUN_OS4" => ("gcc -ansi", "-lm")
       | _ => (print ("I do not know architecture " ^ kit_architecture ^ "\n");
	       exit ())

  (*----------------------------------*)
  (* Generation of script file flags. *)
  (*----------------------------------*)
  val kit_version = "ML_to_"^consult_file_version^"_on_"^kit_architecture
  val kit_source_directory = ml_kit_dir^"src/"

  (* We assume version 17 of the runtime system. *)
  val runtime_directory =  kit_source_directory ^ "Runtime/Version17/"

  val src_directory = ml_kit_dir ^ "kitdemo/"
  val test_env_directory = ml_kit_dir^"TestEnv/"
  val target_directory = ml_kit_dir^"bin/"^kit_version^"/" 
  val log_directory = ml_kit_dir^"bin/"^kit_version^"/"
  val path_to_kit_script = ml_kit_dir^"bin/"^kit_version^"/kit.script"
  val path_to_runtime = ml_kit_dir^"bin/"^kit_version^"/runtime.o"
  val path_to_runtime_prof = ml_kit_dir^"bin/"^kit_version^"/runtime_prof.o"
  val export_dir = ml_kit_dir^"bin/"^kit_version^"/"
  val kit_name = "kit"
  val export_file = export_dir ^ kit_name
  val consult_file = ("ML_CONSULT_" ^ consult_file_version);
  val path_to_consult_file = kit_source_directory ^ consult_file
  val temp_file = gen_tmp_file ("/tmp/%Make"^kit_version);

  fun gen_script () =
    let
      val out_stream = open_out (path_to_kit_script)
    in
      init_script_file out_stream;

      insert_comment_script_file out_stream "Printing of intermediate forms";

      insert_bool_flag out_stream "print_attop_atbot_expression" false;
      insert_bool_flag out_stream "print_cfg_code_before_register_allocation" false;
      insert_bool_flag out_stream "print_cfg_code_after_register_allocation" false;
      insert_bool_flag out_stream "statistics_after_optimisation" false;
      insert_bool_flag out_stream "print_drop_regions_expression" false;
      insert_bool_flag out_stream "print_physical_size_inference_expression" false;
      insert_bool_flag out_stream "print_call_explicit_expression" false;
      

      insert_comment_script_file out_stream "Layout";

      insert_bool_flag out_stream "print_types" false;
      insert_bool_flag out_stream "print_effects" false;
      insert_bool_flag out_stream "print_regions" true;
      insert_bool_flag out_stream "print_K_normal_forms" false;
      insert_bool_flag out_stream "raggedRight" true;
      insert_int_flag out_stream "colwidth" 80;
      insert_bool_flag out_stream "print_rho_levels" false;
      insert_bool_flag out_stream "print_rho_types" false;
      insert_bool_flag out_stream "print_program_points" false;
      insert_bool_flag out_stream "comments_in_kam_code" false;


      insert_comment_script_file out_stream "Control";

      insert_bool_flag out_stream "chat" false;
      insert_bool_flag out_stream "optimiser" true;
      insert_bool_flag out_stream "minimize_fixs" true;
      insert_bool_flag out_stream "fix_conversion" true;
      insert_bool_flag out_stream "contract" true;
      insert_bool_flag out_stream "specialize_recursive_functions" true;
      insert_bool_flag out_stream "elim_explicit_records" true;
      insert_bool_flag out_stream "all_multiplicities_infinite" false;
      insert_bool_flag out_stream "disable_atbot_analysis" false;
      insert_bool_flag out_stream "show_compiler_timings" false;
      insert_bool_flag out_stream "cfg_copy_propagation" true;
      insert_bool_flag out_stream "cfg_dead_code_elimination" true;
      insert_bool_flag out_stream "cfg_register_allocation" true;
      insert_bool_flag out_stream "report_file_sig" false;

      
      insert_comment_script_file out_stream "File";

      insert_comment_script_file out_stream "remember to end directories with a /";
      insert_string_flag out_stream "source_directory" src_directory;
      insert_string_flag out_stream "target_directory" target_directory;
      insert_bool_flag out_stream "log_to_file" true;
      insert_string_flag out_stream "log_directory" log_directory;
      insert_string_flag out_stream "path_to_kit_script" path_to_kit_script;
      insert_string_flag out_stream "path_to_runtime" path_to_runtime;
      insert_string_flag out_stream "path_to_runtime_prof" path_to_runtime_prof;


      insert_comment_script_file out_stream "Profiling";

      insert_bool_flag out_stream "region_profiling" false;
      insert_bool_flag out_stream "generate_lambda_code_with_program_points" false;
      insert_bool_flag out_stream "generate_vcg_graph" false;
      insert_bool_flag out_stream "print_all_program_points" true;


      insert_comment_script_file out_stream "The following eight are used in TestEnv only.";

      insert_string_flag out_stream "test_log" "std_out";
      insert_bool_flag out_stream "size_of_ml_kit_test" false;
      insert_bool_flag out_stream "acceptance_test" true;
      insert_bool_flag out_stream "quicker_acceptance_test" true;
      insert_bool_flag out_stream "performance_test" false;

      insert_string_flag out_stream "kit_source_directory" kit_source_directory;
      insert_string_flag out_stream "test_env_directory" test_env_directory;
      insert_string_flag out_stream "kit_version" kit_version;
      insert_string_flag out_stream "path_to_consult_file" path_to_consult_file;


      insert_comment_script_file out_stream "Debug Kit";

      (*29/03/1997 22:32. tho.: some debugging flags are still hard-wired in
       FLAGS (instead of being dynamic flags).  I wont change that;
       they will probably never be used (as the code works)*)

      insert_bool_flag out_stream "warn_on_escaping_puts" true;
      insert_bool_flag out_stream "debug_which_at" false; 
      insert_bool_flag out_stream "DEBUG_COMPILER" false;
      insert_bool_flag out_stream "debug_cfg_register_allocation" false;
      insert_bool_flag out_stream "debug_KAM_variables" false;
      insert_bool_flag out_stream "debug_lambda_compiler" false;
      insert_bool_flag out_stream "debug_live_set" false;
      insert_bool_flag out_stream "debug_man_enrich" false;
(*
      insert_bool_flag out_stream "debug_expbases" false;
*)

      insert_comment_script_file out_stream "Do not change the following flags:";

      insert_bool_flag out_stream "type_check_lambda" true;
      insert_bool_flag out_stream "eliminate_polymorphic_equality" true;
      insert_bool_flag out_stream "unbox_datatypes" false;
      insert_bool_flag out_stream "tag_integers" false;
      insert_bool_flag out_stream "tag_values" false;
      insert_bool_flag out_stream "enhanced_atbot_analysis" false; 

      if backend = "C" then
	(insert_comment_script_file out_stream "Script entries for C backend";
	 insert_string_flag out_stream "target_file_extension" ".c";
	 insert_bool_flag out_stream "debug_c_code" false;
	 insert_bool_flag out_stream "gen_c_code" true)
      else (* HPPA *)
	(insert_comment_script_file out_stream "Script entries for HPPA backend";
	 insert_string_flag out_stream "target_file_extension" ".s");

      if kit_architecture = "HPUX" then
	(insert_comment_script_file out_stream "Script entries for HPUX architecture";
	 insert_string_flag out_stream "kit_architecture" kit_architecture;
	 insert_string_flag out_stream "c_compiler" "cc -Aa";
	 insert_string_flag out_stream "c_libs" "-lm")
      else (* SUN_OS4 *)
	(insert_comment_script_file out_stream "Script entries for SUN_OS4 architecture";
	 insert_string_flag out_stream "kit_architecture" kit_architecture;
	 insert_string_flag out_stream "c_compiler" "gcc -ansi";
	 insert_string_flag out_stream "c_libs" "-lm");

      insert_string_flag out_stream "link_filename" "link";

      end_script_file out_stream;
      close_out out_stream
    end
in
  print ("\n\n    Name of ML Kit........: " ^ kit_version);
  print ("\n    Example SML files.....: " ^ src_directory);
  print ("\n    Test environment......: " ^ test_env_directory);
  print ("\n    Target directory......: " ^ target_directory);
  print ("\n    Log directory.........: " ^ log_directory);
  print ("\n    Default script........: " ^ path_to_kit_script);
  print ("\n    Runtime system........: " ^ path_to_runtime);
  print ("\n      linked to...........: " ^ runtime_directory ^ "runtime" ^ kit_architecture ^ ".o");
  print ("\n    Runtime system (prof).: " ^ path_to_runtime_prof);
  print ("\n      linked to...........: " ^ runtime_directory ^ "runtime" ^ kit_architecture ^ "Prof.o");

  print ("\n\n    Exporting to..........: " ^ export_file);
  print ("\n    Using consult file....: " ^ consult_file);
  print ("\n    Architecture..........: " ^ kit_architecture);
  print ("\n    Using tempfile........: " ^ temp_file);
   
  ((if exists_file (export_dir) then () else create_dir export_dir) 
   handle X => (print ("Cannot create directory " ^ export_dir);
		raise X));

(*KILL 30/03/1997 00:58. tho.:  scary to a user
     print ("\n\n Directory: " ^ export_dir ^ " does not exists.");
     print ("\n Create directory (y/n): ");
     if (read_string "") <> "y" then
       exit ()
     else
*)
   
  print "\n\nIs this allright? (y/n): ";
  if not (user_types_y ()) then exit () else ();

  (* Generate the script file for the architecture and the backend. *)
  print "\n\nGenerating kit.script...\n\n";
  gen_script();

(*KILL 30/03/1997 18:57. tho.:  the prelude should be with the
 source files, not with the executables.  Also, it is always the
 same, regardless of kit_version.  Furthermore, it should not reside in
 src/, as that contains the sources for the kit, not the sources to
 input to the kit
  (* Link the prelude from src/prelude.sml to the bin directory. *)
  print "\n\nLinking the standard prelude...\n\n";
  mk_sym_link (kit_source_directory ^ "prelude.sml") (ml_kit_dir^"bin/"^kit_version^"/prelude.sml");
*)

  (* Link the runtime system from /src/Runtime/Version17/runtimeARCH.o to the bin directory. *)
  print "\n\nMaking symbolic link to the runtime systems.\n\n";
  mk_sym_link (runtime_directory ^ "runtime" ^ kit_architecture ^ ".o") path_to_runtime;
  mk_sym_link (runtime_directory ^ "runtime" ^ kit_architecture ^ "Prof.o") path_to_runtime_prof;
  mk_sym_link (runtime_directory ^ "MlConvert.h") (ml_kit_dir ^ "kitdemo/MlConvert.h");

  (* Link the prelude from src/prelude.sml to the bin directory. *)
  print "\n\nMaking symbolic link to rp2ps.\n\n";
  mk_sym_link (runtime_directory ^ "Rp2ps/rp2ps" ^ kit_architecture)
      (ml_kit_dir^"bin/"^kit_version^"/rp2ps");

  print "\n\nIt may take a while to build the kit.\nPress return to continue  ";
  wait_for_return ();
  print "\n\n\n\n\n";

  setTempfile temp_file;
  loadFrom consult_file;
  SML_NJ.Print.signatures := 0;
  SML_NJ.Control.Runtime.softmax := 70000000; 
  (export_file, kit_name, path_to_kit_script, kit_version)
end;

(* Building the ML Kit. *)
make "K";

print "\n\nFinished building kit; now exporting it.\n\n";

fun how_to header = header ^ kit_name ^ " [-script s] [-dir d]\n\n\
         \where s is the name of your script file\n\
         \  and d is the directory you want to use as working directory\n\n"

fun process_args ("-script" :: s :: rest) = 
                  (Flags.lookup_string_entry "path_to_kit_script" := s; 
                   process_args rest)
(*       | process_args ("-tempfile" :: s :: rest) = 
                  (setTempfile s; process_args rest)*)
       | process_args ("-dir" :: s :: rest) = (System.cd s; process_args rest)
       | process_args [] = print (how_to "\nalso allowed: ")
       | process_args (bad::other) = 
                  print (how_to("\nArgument '"^bad^ 
                                "' not recognized; try "))

fun fn_to_export (argv, environ) =
      (print (header_with_release) ;
       Flags.lookup_string_entry "path_to_kit_script" := path_to_kit_script ;
       (case argv of
	    _ (*the name of the executable*) :: args => process_args args
	| _ => print (how_to "use: "));
       print ("Script file is " ^ 
              Flags.get_string_entry "path_to_kit_script" ^ "\n\
        	\Reading script file.\n") ;
       read_script () ;
       print ("\n\nVersion " 
              ^ Flags.get_string_entry "kit_version" ^ ".\n\n") ;
       interact ()) ;
      
print ("\n\nAfterwards, the new kit can be started by typing\n\n\
 \     ../bin/" ^ kit_version ^ "/kit\n\n") ;

fun exportWithSML_NJ() =
if exportML export_file then 
  (print header_with_release;
   Flags.lookup_string_entry "path_to_kit_script" := path_to_kit_script ;
   (case SML_NJ.argv () of
	_ :: args => process_args args
      | _ => print (how_to "\nalso allowed: ")) ;
   print ("Script file is " ^ Flags.get_string_entry "path_to_kit_script" ^ "\n\
          \Reading script file.\n") ;
   read_script () ; (* moved to after processing args - Martin *)
   print ("Version " ^ Flags.get_string_entry "kit_version" ^ ".\n") ;
   interact ())

   (* It would be nicer for a kit-developer if path_to_kit_script was
    * a global ref and not (re-)generated by Flags everytime one
    * recompiles *)

else 
  print ("\n\n\nFinished exporting.  Type ctrl-D to exit sml/nj.\n\
   \Start the new kit by typing \"../bin/" ^ kit_version ^ "/kit\".\n");

(*
exportFn (export_file ^ "_standalone", fn_to_export) : unit;
*)

(******************************************************************
* The line below may be used instead of the line above if one instead 
*   wishes to export  SML/NJ with the entire Kit inside it (for Kit systems
*   programming). :
*
******************************************************************)
exportWithSML_NJ();



