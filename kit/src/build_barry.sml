
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building Barry, a Standard ML barifier **\n\n";
	 CM.make());

local
  structure K = KitBarry()

  fun enable s = K.Flags.turn_on s
  fun disable s = K.Flags.turn_off s
in
  val _ = (disable "garbage_collection";
	   K.Flags.print_types := false;
	   K.build_basislib() ;
	   
	   K.install() 
	   )
end
