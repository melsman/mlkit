
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [HPPA Backend] **\n\n";
	 CM.make());

local
  structure K = KitHPPA()
  fun enable s = K.Flags.turn_on s
  fun disable s =  K.Flags.turn_off s
in
  val _ = (disable "garbage_collection";
	   disable "auto_import_basislib" ;
	   K.build_basislib();
	   enable "auto_import_basislib" ;
	   K.install())
end

