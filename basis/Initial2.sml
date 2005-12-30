structure Initial2 =
  struct 
    exception SysErr of string * int Option.option
    val _ = prim ("sml_setFailNumber", (SysErr ("as",NONE) : exn, 2 : int)) : unit
  end


