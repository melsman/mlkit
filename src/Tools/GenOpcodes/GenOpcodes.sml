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

    fun gen_spec_insts spec_file =
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

    fun write_functor_cfuncs spec_file functor_file =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = gen_spec_insts spec_file
	val out_stream = TextIO.openOut(tmp_file)
	val _ = TextIO.output(out_stream, "(* This file is auto-generated with Tools/GenOpcodes on *)\n")
(*	val _ = TextIO.output(out_stream, "(* " ^ (cur_date()) ^ " *)\n")*)

	val _ = TextIO.output(out_stream, "signature BUILT_IN_C_FUNCTIONS_KAM = \n")
	val _ = TextIO.output(out_stream, "  sig\n");
	val _ = TextIO.output(out_stream, "    val name_to_built_in_C_function_index : string -> int\n");
	val _ = TextIO.output(out_stream, "  end\n\n");

	val _ = TextIO.output(out_stream, "functor BuiltInCFunctionsKAM () : BUILT_IN_C_FUNCTIONS_KAM = \n")
	val _ = TextIO.output(out_stream, "  struct\n");
	val _ = TextIO.output(out_stream, "    fun name_to_built_in_C_function_index name =\n")
	val _ = TextIO.output(out_stream, "      case name\n")
	fun write_first_opcode opcode = TextIO.output(out_stream, "        of \"" ^ opcode ^ "\" => 0\n");
	fun write_opcode(opcode,n) = (TextIO.output(out_stream, "      | \"" ^ opcode ^ "\" => " ^ (Int.toString n) ^ "\n");
				      n+1)
	val _ = 
	  case spec_insts of
	    [] => TextIO.output(out_stream, "      of \"\" => 0\n")
	  | (x::xs) => (write_first_opcode x;
			List.foldl write_opcode 1 xs;
			())
	val _ = TextIO.output(out_stream, "      | _ => ~1\n  end\n");
      in
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file functor_file
      end

    fun write_built_in_c_funcs_C spec_file outfile =
      let
	val tmp_file = OS.FileSys.tmpName ()
	val spec_insts = gen_spec_insts spec_file
	val out_stream = TextIO.openOut(tmp_file)
	val _ = TextIO.output(out_stream, "/* This file is auto-generated with Tools/GenOpcodes on */\n")
(*	val _ = TextIO.output(out_stream, "/* " ^ (cur_date()) ^ " */\n")*)
	val _ = TextIO.output(out_stream, "#include \"Prims.h\"\n\n")

	val _ = List.app (fn prim => TextIO.output(out_stream, "extern int " ^ prim ^ "();\n")) spec_insts
	val _ = TextIO.output(out_stream, "\nc_primitive cprim[] = {\n")
	fun write_opcode([]) = TextIO.output(out_stream, "  };\n")
	  | write_opcode([opcode]) = TextIO.output(out_stream, "  " ^ opcode ^ "\n};\n")
	  | write_opcode(opcode::rest) = (TextIO.output(out_stream, "  " ^ opcode ^ ",\n");
					  write_opcode rest)
	val _ = write_opcode spec_insts
      in
	TextIO.closeOut(out_stream);
	copy_if_different tmp_file outfile
      end

    fun process_args [src_dir] = SOME src_dir
      | process_args _ = NONE

    fun print_usage progname = print("\nusage: " ^ progname ^ " src_dir\n")

    fun main (progname, args) =
      case process_args args
	of SOME src_dir =>
	  let
	    fun mk_path rel_path = OS.Path.mkCanonical(OS.Path.concat(src_dir, rel_path))
	    (* Opcodes *)
	    val spec_file = mk_path "Compiler/Backend/KAM/KamInsts.spec"
	    val functor_file = mk_path "Compiler/Backend/KAM/OpcodesKAM.sml"
	    val signature_file = mk_path "Compiler/Backend/KAM/OPCODES_KAM.sml"
	    val kam_insts_C_file = mk_path "RuntimeWithGC/KamInsts.h"
            (* Built In C-functions in the runtime system *)
	    val spec_file_cfuncs = mk_path "Compiler/Backend/KAM/BuiltInCFunctions.spec"
	    val functor_file_cfuncs = mk_path "Compiler/Backend/KAM/BuiltInCFunctionsKAM.sml"
	    val C_file_cfuncs = mk_path "RuntimeWithGC/Prims.c"
	  in
	    (write_functor spec_file functor_file;
	     write_signature spec_file signature_file;
	     write_kam_insts_C spec_file kam_insts_C_file;
	     
	     write_functor_cfuncs spec_file_cfuncs functor_file_cfuncs;
	     write_built_in_c_funcs_C spec_file_cfuncs C_file_cfuncs;
	     OS.Process.success)
	    handle _ => OS.Process.failure
	  end
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