
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [X86 Backend] **\n\n";
	 CM.make());

local
  structure K = KitX86()
  fun enable s = K.Flags.turn_on s
  fun disable s =  K.Flags.turn_off s
in
  val _ = (enable "unbox_function_arguments";
	   
	   enable "garbage_collection";
	   K.build_basislib();

	   disable "garbage_collection";
	   K.build_basislib();
	   
	   K.install())
end
