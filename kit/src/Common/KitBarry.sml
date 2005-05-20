local
    structure KitBarry = 
	KitCompiler(ExecutionBarry)
in
    structure Main =
	struct
	    val name = CommandLine.name()
	    val args = CommandLine.arguments()
	    val _ = OS.Process.exit
		(KitBarry.kitexe(name,args))
	end
end
