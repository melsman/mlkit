
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building Barry, a Standard ML barifier **\n\n";
	 CM.make' "Compiler/barry.cm");

local
  structure K = KitBarrySMLNJ

  fun enable s = Flags.turn_on s
  fun disable s = Flags.turn_off s
in
  val _ = (disable "garbage_collection";
	   Flags.print_types := false;
	   K.build_basislib() ;	
	   K.install() 
	   )
end
