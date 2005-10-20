
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the MLKit compiler [KAM Backend] **\n\n";
	 CM.make' "Compiler/bytecode.cm");

val _ = (Main.disable "garbage_collection";
	 Main.install() 
	 )
