
structure CommandLine : COMMAND_LINE =
  struct
    fun name () : string = prim("sml_commandline_name", ())
    fun arguments () : string list = prim("sml_commandline_args", ())
  end
