local
    fun compact s =
      let fun f(nil,acc) = implode(rev acc)
	    | f(#" "::cs,acc) = f (cs,acc)
	    | f(c::cs,acc) = f (cs, c::acc)
      in f (explode s, nil)
      end
    fun compileArgs compflags src =
      let val {base,ext} = OS.Path.splitBaseExt src	  
	  val t = base ^ "_mlkit" ^ compact compflags ^ ".exe"
	  val cmd = "mlkit " ^ compflags ^ " -o " ^ t ^ " " ^ src
	  val cmd = "../bin/mlkit " ^ compflags ^ " -o " ^ t ^ " " ^ src (*Niels*)
      in  print ("Executing: " ^ cmd ^ "\n")
	; if OS.Process.isSuccess(OS.Process.system cmd) then SOME (src,t)
	  else NONE
      end
in

structure CompileMLKIT : COMPILE =
  struct fun compile kitdir compflags src opts = compileArgs compflags src 
  end
end