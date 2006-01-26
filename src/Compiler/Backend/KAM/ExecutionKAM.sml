
structure ExecutionKAM : EXECUTION =
  struct
    structure TopdecGrammar = PostElabTopdecGrammar
    structure Labels = AddressLabels
    structure PP = PrettyPrint

    structure BackendInfo = 
      BackendInfo(val down_growing_stack : bool = false)         (* false for KAM *)

    structure ClosConvEnv = ClosConvEnv(BackendInfo)

    structure CallConv = CallConv(BackendInfo)

    structure ClosExp = ClosExp(structure ClosConvEnv = ClosConvEnv
				structure BI = BackendInfo
				structure CallConv = CallConv)

    structure JumpTables = JumpTables(BackendInfo)

    structure CodeGen = CodeGenKAM(structure CallConv = CallConv
				   structure ClosExp = ClosExp
				   structure BI = BackendInfo
				   structure JumpTables = JumpTables)

    structure EmitCode = EmitCode(structure CG = CodeGen
				  structure BI = BackendInfo)

    structure CompileBasis = CompileBasis(structure CompBasis = CompBasis
					  structure ClosExp = ClosExp)

    val backend_name = "KAM"

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type Env = CompilerEnv.ElabEnv
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
	      val {main_lab, code, imports, exports, env} = ClosExp.lift(closenv,target)
(* 	      val _ = print "Returning from lift...\n" *)
	      val asm_prg = 
	       Timing.timing "CG" CodeGen.CG 
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
	let 
	    (* It would be preferable to truly link together the files
	     * and the runtime system "kam", so as to produce a movable
	     * executable. mael 2005-04-18 *)
	    val files = 
		map (fn f => OS.Path.mkAbsolute{relativeTo=OS.FileSys.getDir(),path=f}) files
	    val os = TextIO.openOut run
	in (* print ("[Creating file " ^ run ^ " begin ...]\n"); *)
	  TextIO.output(os, "#!/bin/sh\n" ^ !Flags.install_dir ^ "/lib/kam ");
	  app (fn f => TextIO.output(os, f ^ " ")) files;
	  TextIO.output(os, "--args $0 $*");
	  TextIO.closeOut os;
	  OS.Process.system ("chmod a+x " ^ run);
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
