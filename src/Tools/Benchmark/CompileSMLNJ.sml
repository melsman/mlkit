structure CompileSMLNJ : COMPILE =
  struct

    fun arch_os() = "x86-linux"
(*
      case SMLofNJ.SysInfo.getHostArch() ^ "-" ^ SMLofNJ.SysInfo.getOSName()
	of "X86-Linux" => "x86-linux"
	 | "HPPA-HPUX" => "hppa-hpux"
	 | "X86-BSD" => "x86-bsd"
	 | s => s
*)
    fun compile kitdir compileflags src opts =
      let 
	  val heap2execDir = kitdir ^ "/src/heap2exec/"
	  val heap2exec = heap2execDir ^ "heap2exec " ^ heap2execDir ^ "run." ^ arch_os()
	  val {base,ext} = OS.Path.splitBaseExt src
      in case ext
	   of SOME "sml" =>
	     let
	       val src_smlnj = base ^ "_smlnj.sml"
	       val target = base ^ "_smlnj.exe"
	       val exportCmd = 
		 ("echo 'use \"" ^ src_smlnj
		  ^ "\"; SMLofNJ.exportFn(\"" ^ target ^ "\", fn _ => (doit();OS.Process.success));' | sml")
	       val heap2execCmd = 
		  (heap2exec ^ " " ^ target ^ "." ^ arch_os() ^ " " ^ target) 
	     in if (OS.Process.isSuccess(OS.Process.system exportCmd) andalso
		    OS.Process.isSuccess(OS.Process.system heap2execCmd) andalso
		    OS.Process.isSuccess(OS.Process.system("chmod a+x " ^ target)))
		  then SOME (src_smlnj,target)
		else NONE
	     end
	    | SOME "pm" => 
  	     let val src_smlnj = base ^ "_smlnj.cm"
	         val target = base ^ "_smlnj.exe"
		 val exportCmd = 
		   ("echo 'CM.make'\"'\"' \"" ^ src_smlnj
		    ^ "\"; SMLofNJ.exportFn(\"" ^ target ^ "\", fn _ => (Main.doit();OS.Process.success));' | sml")
		 val heap2execCmd = 
		    (heap2exec ^ " " ^ target ^ "." ^ arch_os() ^ " " ^ target) 
	     in if (OS.Process.isSuccess(OS.Process.system exportCmd) andalso
		    OS.Process.isSuccess(OS.Process.system heap2execCmd) andalso
		    OS.Process.isSuccess(OS.Process.system("chmod a+x " ^ target)))
		  then SOME (src_smlnj,target)
		else NONE
	     end
	    | SOME _ => NONE
	    | NONE => NONE
      end handle _ => NONE
  end
	