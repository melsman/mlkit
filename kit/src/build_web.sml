
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [SMLserver] **\n\n";
	 CM.make' "Compiler/bytecode.cm");

val _ = (Main.disable "garbage_collection";
	 Main.disable "cross_module_opt";   (* better module reuse *)
	 Main.enable "quotation";           (* support for quotation-antiquotation *)
	 Flags.SMLserver := true;
         Flags.WEBserver := "AOLServer"; (*"Apache"*)
	 (* Main.enable "formtyping";  *) (* support for form typing *)
	 Main.build_basislib();
	 Main.install() 
	 )
