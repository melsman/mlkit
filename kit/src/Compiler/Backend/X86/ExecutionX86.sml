
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
		  val down_growing_stack : bool = true          (* true for x86 code generation *)
		  val double_alignment_required : bool = false  (* false for x86 code generation *)
		  val extra_prims = nil)                        (* for KAM *)

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
				   structure Crash = Tools.Crash)

    structure Compile = BuildCompile.Compile
    structure CompilerEnv = BuildCompile.CompilerEnv

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = BuildCompile.CompilerEnv.CEnv
    type strdec = TopdecGrammar.strdec
    type target = CodeGen.AsmPrg
    type label = NativeCompile.label

    type linkinfo = {code_label:label, imports: label list, exports : label list, unsafe:bool}
    fun code_label_of_linkinfo (li:linkinfo) = #code_label li
    fun exports_of_linkinfo (li:linkinfo) = #exports li
    fun imports_of_linkinfo (li:linkinfo) = #imports li
    fun unsafe_linkinfo (li:linkinfo) = #unsafe li
    fun mk_linkinfo a : linkinfo = a

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    fun compile (ce, CB, strdecs, vcg_file) =
      let val (cb,closenv) = CompileBasis.de_CompileBasis CB
      in
	case Compile.compile (ce, cb, strdecs, vcg_file)
	  of Compile.CEnvOnlyRes ce => CEnvOnlyRes ce
	   | Compile.CodeRes(ce,cb,target,safe) => 
	    let 
	      val (closenv, target_new) = NativeCompile.compile(closenv,target,safe)
	      val {main_lab, code, imports, exports, safe} = target_new
	      val asm_prg = Tools.Timing.timing "CG" CodeGen.CG target_new
	      val linkinfo = mk_linkinfo {code_label=main_lab,
					  imports=(#1 imports) @ (#2 imports), (* Merge MLFunLab and DatLab *)
					  exports=(#1 exports) @ (#2 exports), (* Merge MLFunLab and DatLab *)
					  unsafe=not(safe)}
	      val CB = CompileBasis.mk_CompileBasis(cb,closenv)
	    in 
	      CodeRes(ce,CB,asm_prg,linkinfo)
	    end
      end
    val generate_link_code = SOME (fn labs => CodeGen.generate_link_code labs)

    fun emit {target, filename:string} : unit =
      CodeGen.emit (target, filename)


    fun delete_file f = OS.FileSys.remove f handle _ => ()
    fun execute_command command : unit =
      (OS.Process.system command; ())
(*      handle OS.SysErr(s,_) => die ("\nCommand " ^ command ^ "\nfailed (" ^ s ^ ");") *)
  
    val c_libs = Flags.lookup_string_entry "c_libs"
    fun assemble (file_s, file_o) =
      (execute_command (!(Flags.lookup_string_entry "c_compiler") ^ " -c -o " ^ file_o ^ " " ^ file_s);
       if !(Flags.lookup_flag_entry "delete_target_files")
	 then  delete_file file_s 
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

    fun link_files_with_runtime_system path_to_runtime files run =
      let val files = map (fn s => s ^ " ") files
	  val shell_cmd = !(Flags.lookup_string_entry "c_compiler") ^ " -o " ^ run ^ " " ^ 
	    concat files ^ path_to_runtime() ^ " " ^ !(Flags.lookup_string_entry "c_libs")
      in execute_command shell_cmd;
	TextIO.output (TextIO.stdOut, "[wrote executable file:\t" ^ run ^ "]\n")
      end 

  end;
