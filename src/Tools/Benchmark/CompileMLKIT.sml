local
    fun compileArgs compflags src =
      let val {base,ext} = OS.Path.splitBaseExt src
	  val t = base ^ "_mlkit.exe"
	  val cmd = "mlkit " ^ compflags ^ " -o " ^ t ^ " " ^ src
	  val cmd = "../bin/mlkit " ^ compflags ^ " -o " ^ t ^ " " ^ src (*Niels*)
      in  print ("Executing: " ^ cmd ^ "\n")
	; if OS.Process.system cmd = OS.Process.success then SOME (src,t)
	  else NONE
      end
in

structure CompileMLKIT : COMPILE =
  struct fun compile kitdir compflags src opts = compileArgs compflags src 
  end
end