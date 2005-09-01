
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [SMLserver] **\n\n";
	 CM.make' "Compiler/bytecode.cm");

(*
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [SMLserver] with SML-NJ profiling enabled **\n\n";
         Compiler.Profile.setProfMode true;
	 CM.make());
*)
val _ = (Main.disable "garbage_collection";
	 Main.disable "cross_module_opt";   (* better module reuse *)
	 Main.enable "quotation";           (* support for quotation-antiquotation *)
	 Flags.SMLserver := true;
(*	 Flags.lookup_string_entry "output" := "MLB/ulfile.ul"; *)
(*         Flags.WEBserver := "AOLServer"; (*"Apache"*) *)
	 (* Main.enable "formtyping";  *) (* support for form typing *)
	 Main.build_basislib(); 
	 Main.install() 
	 )
