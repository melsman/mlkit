
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [KAM Backend] **\n\n";
	 CM.make());

local
  structure K = KitKAM()

  fun enable s = K.Flags.turn_on s
  fun disable s = K.Flags.turn_off s
in
  val _ = (disable "garbage_collection";

	   K.build_basislib() ;
	   
	   K.install() 
	   )
end
