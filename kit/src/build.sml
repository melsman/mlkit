
val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the ML Kit compiler [X86 Backend] **\n\n";
	 CM.make());

structure K = KitX86()

local
  fun enable s = K.Flags.turn_on s
  fun disable s =  K.Flags.turn_off s
(*
  val _ = enable "debug_compiler"
  val _ = disable "import_basislib"
*)
in
  val _ = (enable "unbox_function_arguments" ;

	   enable "garbage_collection";
	   K.build_basislib();

	   disable "garbage_collection";
	   K.build_basislib();
	   K.install()
(*
	   enable "print_type_name_stamps";
	   enable "chat"; 
	   disable "opt"; 
	   enable "keep_functor_bodies_in_memory";
	   enable "sig";
	   disable "import_basislib";
	   K.comp "../test_dev/functor2.sml"
*)
)
end
