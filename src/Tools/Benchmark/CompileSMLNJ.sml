structure CompileSMLNJ : COMPILE =
  struct
    fun compile src =
      let val {base,ext} = OS.Path.splitBaseExt src
      in case ext
	   of SOME "sml" =>
	     let
	       val src2 = base ^ "_smlnj.sml"
	       val target = base ^ "_smlnj.exe"
	       val cmd = ("echo 'use \"" ^ src2
			  ^ "\"; SMLofNJ.exportFn(\"" ^ target
			  ^ "\", fn _ => (doit();OS.Process.success));' | sml")
	       val os = TextIO.openOut target
	       val _ = TextIO.output(os, "sml @SMLload=" ^ target ^ ".x86-linux $*")
	       val _ = TextIO.closeOut os
	       val _ = OS.Process.system("chmod a+x " ^ target)
			  handle _ => (print("\n***Installation not fully succeeded; `chmod a+x " ^ 
					     target ^ "' failed***\n");
				       OS.Process.failure)
	     in if OS.Process.system cmd = OS.Process.success then SOME target
		else NONE
	     end
	    | SOME _ => NONE
	    | NONE => NONE
      end
  end
	