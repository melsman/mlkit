structure CompileMLKIT : COMPILE =
  struct
    fun compile src =
      let val {base,ext} = OS.Path.splitBaseExt src
	  val t = base ^ "_mlkit.exe"
	  val cmd = "mlkit -o " ^ t ^ " " ^ src
      in if OS.Process.system cmd = OS.Process.success then SOME t
	 else NONE
      end
  end