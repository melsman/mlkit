(* export-yacc.sml
 *
 * ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi
 *)
local

    fun err msg = TextIO.output (TextIO.stdErr, msg)
    val exit = OS.Process.exit
    val args = CommandLine.arguments()
    val name = CommandLine.name()
in
    val _ =
        case args of
	    [file] => (ParseGen.parseGen file; exit OS.Process.success)
	  | _ => (err("Usage: " ^ name ^ " filename\n");
		  exit OS.Process.failure)
end
