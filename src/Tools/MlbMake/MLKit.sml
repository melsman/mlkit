local structure M = MlbStandAlone()
in
    val _ = OS.Process.exit 
	(M.mlbmake(M.cmdName(),
		      CommandLine.arguments()))
end