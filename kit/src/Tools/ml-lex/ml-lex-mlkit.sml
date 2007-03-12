local
  val args = CommandLine.arguments()
  val name = CommandLine.name()
  fun err msg = TextIO.output(TextIO.stdErr, String.concat msg)
in
  val _ =
      case args of
          nil => (err [name, ": missing filename\n"];
                  OS.Process.exit OS.Process.failure)
        | files => List.app LexGen.lexGen files
end
