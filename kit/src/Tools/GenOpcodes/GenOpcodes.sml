signature GEN_OPCODES = 
  sig 
    val main : string * string list -> OS.Process.status
  end

structure GenOpcodes : GEN_OPCODES =
  struct

    fun copy_if_different source target =
      let fun all f = 
	    let val is = TextIO.openIn f
	    in TextIO.inputAll is before TextIO.closeIn is
	    end handle _ => ""
      in 
	if all source = all target then 
	  (OS.FileSys.remove source) handle _ => 
	    print ("\n*** Error removing file " ^ source ^ "\n")
	else if OS.Process.system ("mv " ^ source ^ " " ^ target) = OS.Process.success then ()
	     else print ("\n*** Error renaming " ^ source ^ " to " ^ target ^ "\n")
      end 

    fun gen_spec_insts (spec_file: string) : string list =
      String.tokens (fn #" " => true | #"\n" => true | _ => false) (TextIO.inputAll(TextIO.openIn(spec_file)))

    fun cur_date () = Date.toString(Date.fromTimeLocal(Time.now()))

    fun write_functor spec_file functor_file =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = gen_spec_insts spec_file
	val out_stream = TextIO.openOut(tmp_file)
	val _ = TextIO.output(out_stream, "(* This file is auto-generated with Tools/GenOpcodes *)\n")
(*	val _ = TextIO.output(out_stream, "(* " ^ (cur_date()) ^ " *)\n")*)
	val _ = TextIO.output(out_stream, "functor OpcodesKAM () : OPCODES_KAM = \n")
	val _ = TextIO.output(out_stream, "  struct\n");
	fun write_opcode(opcode,n) = (TextIO.output(out_stream, "    val " ^ opcode ^ " = " ^ (Int.toString n) ^ "\n");
				  n+1)
	val _ = List.foldl write_opcode 0 spec_insts
	val _ = TextIO.output(out_stream, "  end\n");
      in
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file functor_file
      end

    fun write_signature spec_file signature_file =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = gen_spec_insts spec_file
	val out_stream = TextIO.openOut(tmp_file)
	val _ = TextIO.output(out_stream, "(* This file is auto-generated with Tools/GenOpcodes on *)\n")
(*	val _ = TextIO.output(out_stream, "(* " ^ (cur_date()) ^ " *)\n")*)
	val _ = TextIO.output(out_stream, "signature OPCODES_KAM = \n")
	val _ = TextIO.output(out_stream, "  sig\n");
	fun write_opcode(opcode,n) = (TextIO.output(out_stream, "    val " ^ opcode ^ " : int \n");
				      n+1)
	val _ = List.foldl write_opcode 0 spec_insts
	val _ = TextIO.output(out_stream, "  end\n");
      in
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file signature_file
      end

    fun write_kam_insts_C spec_file kam_insts_C_file =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = gen_spec_insts spec_file
	val out_stream = TextIO.openOut(tmp_file)
	val _ = TextIO.output(out_stream, "/* This file is auto-generated with Tools/GenOpcodes on */\n")
(*	val _ = TextIO.output(out_stream, "/* " ^ (cur_date()) ^ " */\n")*)
	val _ = TextIO.output(out_stream, "enum instructions {\n")
	fun write_opcode([]) = TextIO.output(out_stream, "  };\n")
	  | write_opcode([opcode]) = TextIO.output(out_stream, "  " ^ opcode ^ "\n};\n")
	  | write_opcode(opcode::rest) = (TextIO.output(out_stream, "  " ^ opcode ^ ",\n");
					  write_opcode rest)
	val _ = write_opcode spec_insts
      in
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file kam_insts_C_file
      end

    fun write_functor_cfuncs spec_file spec_file_nssml functor_file =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = gen_spec_insts spec_file
	val spec_insts_nssml = gen_spec_insts spec_file_nssml
	val out_stream = TextIO.openOut(tmp_file)
	fun out s = TextIO.output(out_stream, s)

	fun out_fun funname spec_insts =
	  let 
	    fun write_first_opcode opcode = 
	      out ("        of \"" ^ opcode ^ "\" => 0\n")
	    fun write_opcode(opcode,n) = 
	      (out ("      | \"" ^ opcode ^ "\" => " ^ Int.toString n ^ "\n");
		n+1)
	  in
	     out ("    fun " ^ funname ^ " name =\n");
	     out "      case name\n";
	    
	     (case spec_insts 
		of [] => out "      of \"\" => 0\n"
		 | (x::xs) => (write_first_opcode x;
			       List.foldl write_opcode 1 xs; ()));
	     out "      | _ => ~1\n"
	  end
      in
	out "(* Do *NOT* edit this file - it is auto-generated with Tools/GenOpcodes *)\n\n";
	out "signature BUILT_IN_C_FUNCTIONS_KAM = \n";
	out "  sig\n";
	out "    val name_to_built_in_C_function_index : string -> int\n";
	out "    val name_to_built_in_C_function_index_nssml : string -> int\n";
	out "  end\n\n";

	out "functor BuiltInCFunctionsKAM () : BUILT_IN_C_FUNCTIONS_KAM = \n";
	out "  struct\n";
	out_fun "name_to_built_in_C_function_index" spec_insts;
	out_fun "name_to_built_in_C_function_index_nssml" (spec_insts @ spec_insts_nssml);
	out "  end\n";
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file functor_file
      end

    fun write_built_in_c_funcs_C (spec_files:string list) outfile =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = List.concat (map gen_spec_insts spec_files)
	val out_stream = TextIO.openOut(tmp_file)
	fun out s = TextIO.output(out_stream, s)
	fun write_opcode([]) = out "  };\n"
	  | write_opcode([opcode]) = out ("  " ^ opcode ^ "\n};\n")
	  | write_opcode(opcode::rest) = (out ("  " ^ opcode ^ ",\n");
					  write_opcode rest)
      in
	out "/* Do *NOT* edit this file - it is auto-generated with Tools/GenOpcodes */\n\n";
	out "#include \"Prims.h\"\n\n";
	List.app (fn prim => out ("extern int " ^ prim ^ "();\n")) spec_insts;
	out "\nc_primitive cprim[] = {\n";
	write_opcode spec_insts;
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file outfile
      end

    fun process_args [src_dir] = SOME src_dir
      | process_args _ = NONE

    fun print_usage progname = print("\nusage: " ^ progname ^ " src_dir\n")

    fun main (progname, args) =
      case process_args args
	of SOME src_dir =>
	 (let
	    fun mk_path rel_path = OS.Path.mkCanonical(OS.Path.concat(src_dir, rel_path))
	    (* Opcodes *)
	    val spec_file = mk_path "Compiler/Backend/KAM/KamInsts.spec"
	    val functor_file = mk_path "Compiler/Backend/KAM/OpcodesKAM.sml"
	    val signature_file = mk_path "Compiler/Backend/KAM/OPCODES_KAM.sml"
	    val kam_insts_C_file = mk_path "RuntimeWithGC/KamInsts.h"

	    val _ = (write_functor spec_file functor_file;
		     write_signature spec_file signature_file;
		     write_kam_insts_C spec_file kam_insts_C_file)

            (* Built In C-functions in the runtime system *)
	    val spec_file_cfuncs = mk_path "Compiler/Backend/KAM/BuiltInCFunctions.spec"
	    val spec_file_cfuncs_nssml = mk_path "Compiler/Backend/KAM/BuiltInCFunctionsNsSml.spec"
	    val functor_file_cfuncs = mk_path "Compiler/Backend/KAM/BuiltInCFunctionsKAM.sml"
	    val C_file_cfuncs = mk_path "RuntimeWithGC/Prims.c"
	    val C_file_cfuncs_nssml = mk_path "RuntimeWithGC/PrimsNsSml.c"
	    val _ = (write_functor_cfuncs spec_file_cfuncs spec_file_cfuncs_nssml functor_file_cfuncs;
		     write_built_in_c_funcs_C [spec_file_cfuncs] C_file_cfuncs;
		     
		     (* version of the C file that support nssml functionality used by
		      * SMLserver *)
		     write_built_in_c_funcs_C [spec_file_cfuncs, spec_file_cfuncs_nssml] C_file_cfuncs_nssml)
	  in
	     OS.Process.success
	  end handle _ => OS.Process.failure)
      | NONE => (print_usage progname; OS.Process.failure)

    fun install() =
      let 
	val _ = print "\n ** Installing Gen Opcodes, a tool for building SML and C files"
	val _ = print "\n ** with opcodes for KAM instructions and built in primitives. \n"

	val kit_src_tools_gen_opcodes_path = OS.FileSys.getDir()   (* assumes we are in kit/src/Tools/GenOpcodes directory *)
	val kit_bin_path = OS.Path.mkCanonical (OS.Path.concat(kit_src_tools_gen_opcodes_path, "../../../bin"))
	val kit_bin_kitgen_opcodes_path = OS.Path.joinDirFile{dir=kit_bin_path, file="kitgen_opcodes"}

	fun arch_os() = (SMLofNJ.SysInfo.getHostArch(), SMLofNJ.SysInfo.getOSName())
	fun kitgen_opcodes_image() =
	  case arch_os()
	    of ("X86", "Linux") => "kitgen_opcodes.x86-linux"
	  | ("HPPA", "HPUX") => "kitgen_opcodes.hppa-hpux"
	  | _ => "unknown"
	val kit_bin_kitgen_opcodes_image_path = OS.Path.joinDirFile{dir=kit_bin_path, file=kitgen_opcodes_image()}
	val os = TextIO.openOut kit_bin_kitgen_opcodes_path
	val _ = (TextIO.output(os, "sml @SMLload=" ^ kit_bin_kitgen_opcodes_image_path ^ " $*"); TextIO.closeOut os)
	val _ = OS.Process.system("chmod a+x " ^ kit_bin_kitgen_opcodes_path)
	  handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ kit_bin_kitgen_opcodes_path ^ "' failed***\n");
		       OS.Process.failure)
      in 
	SMLofNJ.exportFn(kit_bin_kitgen_opcodes_path,main)
      end

    val _ = install()
  end