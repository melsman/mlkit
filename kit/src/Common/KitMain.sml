functor KitMain (K : KIT_COMPILER) : sig end =
    struct
	val name = CommandLine.name()
	val args = CommandLine.arguments()
	val _ = OS.Process.exit (K.kitexe(name,args))
    end
	    