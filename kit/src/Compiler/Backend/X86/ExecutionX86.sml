
functor ExecutionX86 (BuildCompile : BUILD_COMPILE) : EXECUTION =
  struct
    structure ExecutionArgs = BuildCompile.ExecutionArgs
    open ExecutionArgs

    structure Basics = Elaboration.Basics
    structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
    structure Tools = Basics.Tools
    structure AllInfo = Basics.AllInfo
    structure PP = Tools.PrettyPrint
    structure Name = Basics.Name
    structure IntFinMap = Tools.IntFinMap
    structure Flags = Tools.Flags
    structure Report = Tools.Report
    structure Crash = Tools.Crash

    structure InstsX86 = InstsX86(structure Labels = Labels
				  structure Lvars = Lvars
				  structure Lvarset = Lvarset
				  structure Crash = Crash
				  structure PP = PP)

    structure BackendInfo = 
      BackendInfo(structure Labels = Labels
		  structure PP = PP
		  structure Flags = Flags
		  structure Report = Report
		  structure Crash = Crash
		  structure RegConst = BuildCompile.RegConst
		  val down_growing_stack : bool = true)          (* true for x86 code generation *)

    structure NativeCompile = NativeCompile(open ExecutionArgs
					    open BuildCompile
					    structure BackendInfo = BackendInfo
					    structure RegisterInfo = InstsX86.RI)

    structure CompileBasis = CompileBasis(structure CompBasis = BuildCompile.CompBasis
					  structure ClosExp = NativeCompile.ClosExp
					  structure PP = PP
					  structure Flags = Flags)

    structure JumpTables = JumpTables(structure BI = BackendInfo
				      structure Crash = Crash)

    structure CodeGen = CodeGenX86(structure BackendInfo = BackendInfo
				   structure InstsX86 = InstsX86
				   structure Con = Con
				   structure Excon = Excon
				   structure Lvars = Lvars
				   structure Lvarset = Lvarset
				   structure Labels = Labels
				   structure JumpTables = JumpTables
				   structure CallConv = NativeCompile.CallConv
				   structure LineStmt = NativeCompile.LineStmt
				   structure SubstAndSimplify = NativeCompile.SubstAndSimplify
				   structure PP = PP
				   structure Flags = Tools.Flags
				   structure Report = Tools.Report
				   structure Crash = Tools.Crash
				   structure Effect = BuildCompile.Effect)

    structure Compile = BuildCompile.Compile
    structure CompilerEnv = BuildCompile.CompilerEnv

    val _ = Flags.add_string_entry 
      {long="clibs", short=NONE, item=ref "-lm -lc",
       menu=["Control", "c libraries (archives)"],
       desc="If you have added your own object files to a project,\n\
	\you might also need to link with libraries other\n\
	\than libm.a and libc.a (\"-lm -lc\")."}

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
    type CEnv = BuildCompile.CompilerEnv.CEnv
    type Env = BuildCompile.CompilerEnv.ElabEnv
    type strdec = TopdecGrammar.strdec
    type strexp = TopdecGrammar.strexp
    type funid = TopdecGrammar.funid
    type strid = TopdecGrammar.strid
    type target = CodeGen.AsmPrg
    type label = NativeCompile.label

    type linkinfo = {code_label:label, imports: label list * label list, 
		     exports : label list * label list, unsafe:bool}
    fun code_label_of_linkinfo (li:linkinfo) = #code_label li
    fun exports_of_linkinfo (li:linkinfo) = #exports li
    fun imports_of_linkinfo (li:linkinfo) = #imports li
    fun unsafe_linkinfo (li:linkinfo) = #unsafe li
    fun mk_linkinfo a : linkinfo = a

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
	      val asm_prg = Tools.Timing.timing "CG" CodeGen.CG target_new
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
    val clibs = Flags.lookup_string_entry "clibs"

    fun gas() = if gdb_support() then "as --gstabs"
		else "as"
    fun assemble (file_s, file_o) =
      (execute_command (gas() ^ " -o " ^ file_o ^ " " ^ file_s);
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

    fun link_files_with_runtime_system path_to_runtime files run =
      let val files = map (fn s => s ^ " ") files
	  val shell_cmd = !(Flags.lookup_string_entry "c_compiler") ^ " -o " ^ run ^ " " ^ 
	    concat files ^ path_to_runtime() ^ " " ^ !clibs
	  val debug_linking = Flags.lookup_flag_entry "debug_linking"
	  fun pr_debug_linking s = if !debug_linking then print s else ()
      in 
	pr_debug_linking ("[using link command: " ^ shell_cmd ^ "]\n");
	execute_command shell_cmd;
	strip run;
	print("[wrote executable file:\t" ^ run ^ "]\n");
	report_dangle_stat()
      end 

  end;

