
functor ExecutionKAM(BuildCompile : BUILD_COMPILE) : EXECUTION =
  struct
    open BuildCompile
    open ExecutionArgs

    structure Basics = Elaboration.Basics
    structure TyName = Basics.TyName
    structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
    structure Tools = Basics.Tools
    structure AllInfo = Basics.AllInfo
    structure PP = Tools.PrettyPrint
    structure Name = Basics.Name
    structure IntStringFinMap = Tools.IntStringFinMap
    structure Flags = Tools.Flags
    structure Report = Tools.Report
    structure Crash = Tools.Crash

    structure BackendInfo = 
      BackendInfo(structure Labels = Labels
		  structure PP = PP
		  structure Flags = Flags
		  structure Report = Report
		  structure Crash = Crash
		  structure RegConst = BuildCompile.RegConst
		  val down_growing_stack : bool = false         (* false for KAM *)
		  val double_alignment_required : bool = true)   (* true for KAM?? *)

    structure Kam = Kam (structure Labels = Labels
			 structure PP = PP
			 structure Crash = Crash)

    structure ClosConvEnv = ClosConvEnv(structure Lvars = Lvars
					structure Con = Con
					structure Excon = Excon
					structure Effect = Effect
					structure MulExp = MulExp
					structure RegvarFinMap = EffVarEnv
					structure PhysSizeInf = PhysSizeInf
					structure Labels = Labels
					structure BI = BackendInfo
					structure PP = PP
					structure Crash = Crash)

    structure CallConv = CallConv(structure Lvars = Lvars
				  structure BI = BackendInfo
				  structure PP = PP
				  structure Flags = Flags
				  structure Report = Report
				  structure Crash = Crash)

    structure RegionFlowGraphProfiling =
      RegionFlowGraphProfiling(structure Effect = Effect
			       structure AtInf = AtInf
			       structure PhySizeInf = PhysSizeInf
			       structure PP = PP
			       structure Flags = Flags
			       structure Crash = Crash
			       structure Report = Report)


    structure ClosExp = ClosExp(structure Con = Con
				structure Excon = Excon
				structure Lvars = Lvars
				structure TyName = TyName
				structure Effect = Effect
				structure RType = RType
				structure MulExp = MulExp
				structure Mul = Mul
				structure AtInf = AtInf
				structure PhysSizeInf = PhysSizeInf
				structure RegionFlowGraphProfiling = RegionFlowGraphProfiling
				structure Labels = Labels
				structure ClosConvEnv = ClosConvEnv
				structure BI = BackendInfo
				structure CallConv = CallConv
				structure PP = PP
				structure Flags = Flags
				structure Report = Report
				structure Crash = Crash)

    structure JumpTables = JumpTables(structure BI = BackendInfo
				      structure Crash = Crash)

    structure BuiltInCFunctions = BuiltInCFunctionsKAM()

    structure CodeGen = CodeGenKAM(structure PhysSizeInf = PhysSizeInf
				   structure Con = Con
				   structure Excon = Excon
				   structure Lvars = Lvars
				   structure Effect = Effect
				   structure Labels = Labels
				   structure RegvarFinMap = EffVarEnv
				   structure CallConv = CallConv
				   structure ClosExp = ClosExp
				   structure BI = BackendInfo
				   structure JumpTables = JumpTables
				   structure Lvarset = Lvarset
				   structure Kam = Kam
				   structure BuiltInCFunctions = BuiltInCFunctions
				   structure PP = PP
				   structure Report = Report
				   structure Flags = Flags
				   structure Crash = Crash)

    structure Opcodes = OpcodesKAM()
    structure BuffCode = BuffCode()
    structure ResolveLocalLabels = ResolveLocalLabels(structure BC = BuffCode
						      structure IntStringFinMap = IntStringFinMap
						      structure Labels = Labels
						      structure Crash = Crash)

    structure EmitCode = EmitCode(structure Labels = Labels
				  structure CG = CodeGen
				  structure Opcodes = Opcodes
				  structure BC = BuffCode
				  structure RLL = ResolveLocalLabels
				  structure Kam = Kam
				  structure BI = BackendInfo
				  structure Flags = Flags
				  structure Crash = Crash)

    structure CompileBasis = CompileBasis(structure CompBasis = CompBasis
					  structure ClosExp = ClosExp
					  structure PP = PP
					  structure Flags = Flags)

    structure Compile = BuildCompile.Compile
    structure CompilerEnv = BuildCompile.CompilerEnv

    val backend_name = "KAM"

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = BuildCompile.CompilerEnv.CEnv
    type Env = BuildCompile.CompilerEnv.ElabEnv
    type strdec = TopdecGrammar.strdec
    type strexp = TopdecGrammar.strexp
    type funid = TopdecGrammar.funid
    type strid = TopdecGrammar.strid
    type target = CodeGen.AsmPrg
    type label = Labels.label

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
	      val {main_lab, code, imports, exports, env} = ClosExp.lift(closenv,target)
(* 	      val _ = print "Returning from lift...\n" *)
	      val asm_prg = 
		Tools.Timing.timing "CG" CodeGen.CG 
		{main_lab_opt= (* if safe then NONE else*) SOME main_lab, 
		 code=code, 
		 imports=imports, 
		 exports=exports}

	      val linkinfo = mk_linkinfo {code_label=main_lab,
					  imports=imports, (* (MLFunLab,DatLab) *)
					  exports=exports, (* (MLFunLab,DatLab) *)
					  unsafe=not(safe)}
	      val CB = CompileBasis.mk_CompileBasis(cb,env)
	    in 
	      CodeRes(ce,CB,asm_prg,linkinfo)
	    end
      end

    val generate_link_code = NONE

    fun emit (arg as {target, filename:string}) : string = 
      let val filename = filename ^ ".uo"
      in EmitCode.emit {target=target, filename=filename};
	filename
      end
	      
    fun link_files_with_runtime_system _ files run = 
      if !Flags.SMLserver then ()
      else 
	let val os = TextIO.openOut run	    
	in (* print ("[Creating file " ^ run ^ " begin ...]\n"); *)
	  TextIO.output(os, "#!/bin/sh\n" ^ !Flags.install_dir ^ "/bin/kam ");
	  app (fn f => TextIO.output(os, f ^ " ")) files;
	  TextIO.output(os, "--args $0 $*");
	  TextIO.closeOut os;
	  OS.Process.system "chmod a+x run";
	  print("[Created file " ^ run ^ "]\n")
	  (* ; app (print o (fn s => "   " ^ s ^ "\n")) files  *)
	end

    val pu_linkinfo =
	let val pu_labels = Pickle.listGen Labels.pu
	    val pu_pair = Pickle.pairGen(pu_labels,pu_labels)
	in Pickle.convert (fn (c,i,e,u) => {code_label=c,imports=i,exports=e,unsafe=u},
			   fn {code_label=c,imports=i,exports=e,unsafe=u} => (c,i,e,u))
	    (Pickle.tup4Gen0(Labels.pu,pu_pair,pu_pair,Pickle.bool))
	end    
  end