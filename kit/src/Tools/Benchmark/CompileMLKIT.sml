local
    fun compileArgs extension args src =
      let val {base,ext} = OS.Path.splitBaseExt src
	  val t = base ^ extension
	  val cmd = "mlkit " ^ args ^ " -o " ^ t ^ " " ^ src
      in if OS.Process.system cmd = OS.Process.success then SOME (src,t)
	 else NONE
      end
in

structure CompileMLKIT : COMPILE =
  struct fun compile src = compileArgs "_mlkit.exe" "" src 
  end

structure CompileMLKITGC : COMPILE =
  struct fun compile src = compileArgs "_mlkitgc.exe" "-gc" src
  end
end