
structure ExecutionX86: EXECUTION =
  struct
    structure TopdecGrammar = PostElabTopdecGrammar
    structure Labels = AddressLabels
    structure PP = PrettyPrint

    structure BackendInfo = 
      BackendInfo(val down_growing_stack : bool = true)          (* true for x86 code generation *)

    structure NativeCompile = NativeCompile(structure BackendInfo = BackendInfo
					    structure RegisterInfo = InstsX86.RI)

    structure CompileBasis = CompileBasis(structure ClosExp = NativeCompile.ClosExp)

    structure JumpTables = JumpTables(BackendInfo)

    structure CodeGen = CodeGenX86(structure BackendInfo = BackendInfo
				   structure JumpTables = JumpTables
				   structure CallConv = NativeCompile.CallConv
				   structure LineStmt = NativeCompile.LineStmt
				   structure SubstAndSimplify = NativeCompile.SubstAndSimplify)

    fun die s = Crash.impossible("ExecutionX86." ^ s)

    val be_rigid = false

    local
	fun convertList option s =
	    let val l = String.tokens(fn c => c = #",")s
	    in map (fn s => option ^ s) l
	    end
    in
	fun libConvertList s = concat(convertList " -l" s)
	fun libdirsConvertList s = concat(convertList " -L" s)
    end

    local val default = "m,c,dl"
    in
	val _ = Flags.add_string_entry 
	    {long="libs", short=NONE, item=ref default,
	     menu=["Control", "foreign libraries (archives)"],
	     desc="For accessing a foreign function residing in\n\
	      \an archive named libNAME.a from Standard ML code\n\
	      \(using prim), you need to add 'NAME' to this\n\
	      \comma-separated list. Notice that an object file\n\
	      \(with extension '.o') is an archive if it is\n\
	      \renamed to have extension '.a'. You may need to\n\
	      \use the -libdirs option for specifying\n\
	      \directories for which ld should look for library\n\
	      \archives. The libraries are passed to 'ld' using\n\
	      \the -l option."}
    end

    val _ = Flags.add_string_entry 
      {long="libdirs", short=NONE, item=ref "",
       menu=["Control", "library directories (paths to archives)"],
       desc="This option controls where ld looks for\n\
	\archives. The format is a comma-separated list\n\
	\of directories; see the -libs entry. The default\n\
	\is the empty list; thus 'ld' will look for\n\
	\libraries in only the system specific default\n\
	\directores. The directories are passed to 'ld'\n\
	\using the -L option."}

    val _ = Flags.add_string_entry 
      {long="c_compiler", short=SOME "cc", item=ref "gcc",
       menu=["Control", "C compiler (used for linking)"],
       desc="This option specifies which C compiler is\n\
	\used for linking. When linking with c++\n\
        \libraries, 'g++' is the linker you want."}

    val strip_p = ref false
    val _ = Flags.add_bool_entry 
       {long="strip", short=NONE, neg=false, item=strip_p,
	menu=["Control", "strip executable"],
	desc="If enabled, the Kit strips the generated executable."}

    val _ = Flags.add_bool_entry
       {long="delete_target_files", short=NONE, neg=true, item=ref true,
	menu=["Debug", "delete target files"], 
	desc="Delete assembler files produced by the compiler. If you\n\
	 \disable this flag, you can inspect the assembler code\n\
	 \produced by the compiler."}

    val _ = Flags.add_bool_entry 
	{long="gdb_support", short=SOME "g", neg=false, 
	 menu=["Debug","gdb support"], item=ref false, 
	 desc="When enabled, the compiler passes the option --gstabs\n\
	  \to `as' (The GNU Assembler) and preserves the generated\n\
	  \assembler files (.s files). Passing the --gstabs\n\
	  \option to `as' makes it possible to step through\n\
	  \the generated program using gdb (The GNU Debugger)."}
	 
    val dangle_stat_p = ref false
    val _ = Flags.add_bool_entry 
	{long="dangling_pointers_statistics", short=NONE, neg=false, 
	 menu=["Debug","dangling pointers statistics"], item=dangle_stat_p, 
	 desc="When enabled, the compiler prints statistics about\n\
          \the number of times strengthening of the region typing\n\ 
	  \rules (to avoid dangling pointers during evaluation)\n\
	  \effects the target program. This flag is useful only\n\
	  \when the flag -gc or -no_dangle is enabled."}

    fun report_dangle_stat() =
	 if !dangle_stat_p then
	   let val n = !Flags.Statistics.no_dangling_pointers_changes
               val total = !Flags.Statistics.no_dangling_pointers_changes_total
	   in
	       print ("Dangling pointers statistics: \n\
		      \ * Number of changes due to strengthening of typing \n\
		      \   rules to avoid dangling pointers: " ^ Int.toString n ^
		      "\n * Total number of changes: " ^ Int.toString total ^ "\n")
	   end
	 else ()
 
    val backend_name = "X86"

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type Env = CompilerEnv.ElabEnv
    type strdec = TopdecGrammar.strdec
    type strexp = TopdecGrammar.strexp
    type funid = TopdecGrammar.funid
    type strid = TopdecGrammar.strid
    type target = CodeGen.AsmPrg
    type lab = NativeCompile.label

    val pr_lab = Labels.pr_label               

    type linkinfo = {code_label:lab, imports: lab list * lab list, 
		     exports : lab list * lab list, unsafe:bool}
    fun code_label_of_linkinfo (li:linkinfo) = #code_label li
    fun exports_of_linkinfo (li:linkinfo) = #exports li
    fun imports_of_linkinfo (li:linkinfo) = #imports li
    fun unsafe_linkinfo (li:linkinfo) = #unsafe li
    fun mk_linkinfo a : linkinfo = a

    (* Hook to be run before any compilation *)
    val preHook : unit -> unit = Compile.preHook
	
    (* Hook to be run after all compilations (for one compilation unit) *)
    val postHook : {unitname:string} -> unit = Compile.postHook

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    fun compile fe (ce, CB, strdecs, vcg_file) =
      let val (cb,closenv) = CompileBasis.de_CompileBasis CB
      in
	case Compile.compile fe (ce, cb, strdecs)
	  of Compile.CEnvOnlyRes ce => CEnvOnlyRes ce
	   | Compile.CodeRes(ce,cb,target,safe) => 
	    let 
	      val (closenv, target_new) = NativeCompile.compile(closenv,target,safe,vcg_file)
	      val {main_lab, code, imports, exports, safe} = target_new
	      val asm_prg = Timing.timing "CG" CodeGen.CG target_new
	      val linkinfo = mk_linkinfo {code_label=main_lab,
					  imports=imports, (* (MLFunLab, DatLab) *)
					  exports=exports, (* (MLFunLab, DatLab) *)
					  unsafe=not(safe)}
	      val CB = CompileBasis.mk_CompileBasis(cb,closenv)
	    in 
	      CodeRes(ce,CB,asm_prg,linkinfo)
	    end
      end
    val generate_link_code = SOME (fn (labs,exports) => CodeGen.generate_link_code (labs,exports))
	
    fun delete_file f = OS.FileSys.remove f handle _ => ()
    fun execute_command command : unit =
      (OS.Process.system command; ())
(*      handle OS.SysErr(s,_) => die ("\nCommand " ^ command ^ "\nfailed (" ^ s ^ ");") *)

    val gdb_support = Flags.is_on0 "gdb_support"
    val delete_target_files = Flags.is_on0 "delete_target_files"
    val libs = Flags.lookup_string_entry "libs"

    fun gas() = if gdb_support() then "as --gstabs"
		else "as"
    fun assemble (file_s, file_o) =
      (execute_command (gas() ^ " --32 -o " ^ file_o ^ " " ^ file_s);
       if delete_target_files() andalso not(gdb_support()) then delete_file file_s 
       else ())

	  (*e.g., "cc -Aa -c -o link.o link.s"

	   man cc:
	   -c          Suppress the link edit phase of the compilation, and
		       force an object (.o) file to be produced for each .c
		       file even if only one program is compiled.  Object
		       files produced from C programs must be linked before
		       being executed.

	   -ooutfile   Name the output file from the linker outfile.  The
		       default name is a.out.*)


    fun emit {target, filename:string} : string =
      let val filename_o = filename ^ ".o"
	  val filename_s = filename ^ ".s"
      in CodeGen.emit (target, filename_s);
	assemble(filename_s, filename_o);
	filename_o
      end

    fun strip run =
      if !strip_p then (execute_command ("strip " ^ run)
	                handle _ => ())
      else ()

    fun link_files_with_runtime_system0 path_to_runtime files run =
      let val files = map (fn s => s ^ " ") files
	  val libdirs = 
	      case !(Flags.lookup_string_entry "libdirs") of
		  "" => ""
		| libdirs => " " ^ libdirsConvertList libdirs
	  val shell_cmd = !(Flags.lookup_string_entry "c_compiler") ^ " -m32 -o " ^ run ^ " " ^ 
	    concat files ^ path_to_runtime() ^ libdirs ^ libConvertList(!libs)
	  val debug_linking = Flags.lookup_flag_entry "debug_linking"
	  fun pr_debug_linking s = if !debug_linking then print s else ()
      in 
	pr_debug_linking ("[using link command: " ^ shell_cmd ^ "]\n");
	execute_command shell_cmd;
	strip run;
	print("[wrote executable file:\t" ^ run ^ "]\n");
	report_dangle_stat()
      end 

    val op ## = OS.Path.concat infix ##
                                               
    local
	  val region_profiling = Flags.lookup_flag_entry "region_profiling"
	  val tag_values = Flags.is_on0 "tag_values"
	  val tag_pairs_p = Flags.is_on0 "tag_pairs"
          val gc_p = Flags.is_on0 "garbage_collection"
          val gengc_p = Flags.is_on0 "generational_garbage_collection"

	  fun path_to_runtime () = 
	    let fun file () = 
	      if !region_profiling andalso gc_p() andalso tag_pairs_p() then "runtimeSystemGCTPProf.a"  else
	      if !region_profiling andalso gc_p() andalso gengc_p()     then "runtimeSystemGenGCProf.a" else
	      if !region_profiling andalso gc_p()                       then "runtimeSystemGCProf.a"    else
	      if !region_profiling                                      then "runtimeSystemProf.a"      else
              if                           gc_p() andalso tag_pairs_p() then "runtimeSystemGCTP.a"      else
              if                           gc_p() andalso gengc_p()     then "runtimeSystemGenGC.a"     else
              if                           gc_p()                       then "runtimeSystemGC.a"        else
              if tag_values()                     andalso tag_pairs_p() then 
		  die "no runtime system supports tagging of values with tagging of pairs"             else
              if tag_values()                                           then "runtimeSystemTag.a"      else
		                                                             "runtimeSystem.a"
	    in !Flags.install_dir ## "lib" ## file()
	    end
    in
       val link_files_with_runtime_system = link_files_with_runtime_system0 path_to_runtime
    end


    local
      val region_profiling = Flags.is_on0 "region_profiling"
      val recompile_basislib = Flags.is_on0 "recompile_basislib"
      val tag_pairs_p = Flags.is_on0 "tag_pairs"
      val gc_p = Flags.is_on0 "garbage_collection"
      val gengc_p = Flags.is_on0 "generational_garbage_collection"
    in 
	(* Remember also to update RepositoryFinMap in Common/Elaboration.sml *)
      fun mlbdir() = 
	  let val subdir =
	      if recompile_basislib() then "Scratch"   (* avoid overwriting other files *)
	      else 
		  case (gengc_p(),gc_p(), region_profiling(), tag_pairs_p()) of 
		      (false,     true,   true,               false) => "RI_GC_PROF"
		    | (false,     true,   false,              false) => "RI_GC"
		    | (false,     true,   true,               true)  => "RI_GC_TP_PROF"
		    | (false,     true,   false,              true)  => "RI_GC_TP"
		    | (true,      true,   true,               false) => "RI_GEN_GC_PROF"
		    | (true,      true,   false,              false) => "RI_GEN_GC"
		    | (true,      _,      _,                  _)     => die "Illegal combination of generational garbage collection and tagged pairs"
		    | (false,     false,  true,               _)     => "RI_PROF"
		    | (false,     false,  false,              _)     => "RI"
	  in "MLB" ## subdir
	  end
    end

    val pu_linkinfo =
	let val pu_labels = Pickle.listGen Labels.pu
	    val pu_pair = Pickle.pairGen(pu_labels,pu_labels)
	in Pickle.convert (fn (c,i,e,u) => {code_label=c,imports=i,exports=e,unsafe=u},
			   fn {code_label=c,imports=i,exports=e,unsafe=u} => (c,i,e,u))
	    (Pickle.tup4Gen(Labels.pu,pu_pair,pu_pair,Pickle.bool))
	end
  end
