
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [KAM Backend] **\n\n";
	 CM.make());

local
  structure K = KitKAM()

  fun enable s = K.Flags.turn_on s
  fun disable s = K.Flags.turn_off s
in
  val _ = (disable "garbage_collection";

	   K.Flags.lookup_string_entry "kam_uolistfile" := "basislib.ul";
	   K.build_basislib() ;
	   K.Flags.lookup_string_entry "kam_uolistfile" := "";
	   
	   K.install() 
	   )
end
