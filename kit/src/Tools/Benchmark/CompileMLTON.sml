structure CompileMLTON : COMPILE =
  struct 
    fun compile kitdir compflags src opts =
      let val {base,ext} = OS.Path.splitBaseExt src
	  val target = base ^ "_mlton.exe"
	  fun comp src =
	    let val cmd = "/usr/local/bin/mlton -o " ^ target ^ " " ^ compflags ^ " " ^ src
	    in  print ("Executing: " ^ cmd ^ "\n")
	      ; if OS.Process.system cmd = OS.Process.success then SOME (src,target)
		else NONE
	    end
      in case ext
	   of SOME "sml" => 
	     if List.exists (fn a => a = "mlton") opts then comp (base ^ "_mlton.sml") 
	     else comp src (* use the same src as mlkit *)
	    | SOME "pm" => comp (base ^ "_mlton.cm") (* use mlton specific source *)
	    | _ => NONE
      end
  end