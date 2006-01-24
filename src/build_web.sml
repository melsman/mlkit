
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the MLKit compiler [SMLserver] **\n\n";
      (* Compiler.Profile.setProfMode true; *)
	 CM.make' "Compiler/bytecode.cm");

val _ = (Main.disable "garbage_collection";
	 Main.disable "cross_module_opt";   (* better module reuse *)
	 Main.enable "quotation";           (* support for quotation-antiquotation *)
	 Flags.SMLserver := true;
	 (* Main.enable "formtyping";  *) (* support for form typing *)
	 Main.install() 
	 )

