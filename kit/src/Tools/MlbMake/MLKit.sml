val _ = OS.Process.exit 
    (Main.mlbmake(Main.cmdName(),
		  CommandLine.arguments()))