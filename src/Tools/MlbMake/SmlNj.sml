structure DoIt =
  struct
      local structure M = MlbStandAlone()
      in
	  fun install() =
	      let 
		  fun arch_os() = 
		      case SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName() of
			  "X86-Linux" => "x86-linux"
			| "HPPA-HPUX" => "hppa-hpux"
			| "X86-BSD" => "x86-bsd"
			| s => s
		  val _ = print ("\n ** Exporting " ^ M.cmdName() ^ " executable **\n\n")
		  val kitbin_path = OS.Path.mkCanonical (OS.Path.concat(OS.FileSys.getDir(), "../../../bin"))
		  val bin_path = OS.Path.joinDirFile{dir=kitbin_path, file=M.cmdName()}		
		  val binimage_path = OS.Path.joinDirFile{dir=kitbin_path, file=M.cmdName() ^ "." ^ arch_os()}
		  val os = TextIO.openOut bin_path
		  val _ = (TextIO.output(os, "sml @SMLload=" ^ binimage_path ^ " " ^ " $*"); 
			   TextIO.closeOut os)
		  val _ = OS.Process.system("chmod a+x " ^ bin_path)
		      handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
					 bin_path ^ "' failed***\n");
				   OS.Process.failure)
	      in SMLofNJ.exportFn(bin_path, M.mlbmake)
	      end
	  
	  val _ = install()
      end
  end
