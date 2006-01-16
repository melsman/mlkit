functor KitMainSMLNJ (Kit : KIT_COMPILER) =
    struct
	fun arch_os() = 
	    case SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName() of 
		"X86-Linux" => "x86-linux"
	      | "HPPA-HPUX" => "hppa-hpux"
	      | "X86-BSD" => "x86-bsd"
	      | s => s

	(* As default root directory we use the absolute path that corresponds to the kit-directory 
	 * in which the Kit is built. When the Kit is properly installed this directory can be changed
	 * by passing another directory than this to the Kit executable; we assume we are in the 
	 * kit/src directory... *)
	val default_root_dir = OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(), ".."))
	    
	fun install() =
	    let 
		val _ = print "\n ** Exporting compiler executable **\n\n"			
		val kitbinkitimage_path = OS.Path.concat(default_root_dir, "bin/kit." ^ arch_os())
		val kitbinkit_path = OS.Path.concat(default_root_dir, "bin/kit")
		val os = TextIO.openOut kitbinkit_path
		val _ = (TextIO.output(os, "sml @SMLload=" ^ kitbinkitimage_path ^ " " ^ 
				       default_root_dir ^ " $*"); 
			 TextIO.closeOut os)
		val _ = OS.Process.system("chmod a+x " ^ kitbinkit_path)
		    handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
				       kitbinkit_path ^ "' failed***\n");
				 OS.Process.failure)
	    in SMLofNJ.exportFn(kitbinkit_path, Kit.kitexe)
	    end
	fun enable s = Flags.turn_on s
	fun disable s = Flags.turn_off s
    end
