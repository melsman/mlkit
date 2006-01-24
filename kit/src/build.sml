

val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the MLKit compiler [X86 Backend] **\n\n";
      (* Compiler.Profile.setProfMode true; *)
	 CM.make' "Compiler/native.cm");

val _ = Main.install()
