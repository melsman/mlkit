structure Compiler = KitCompiler(ExecutionKAM)
val _ = 
   Flags.turn_off "garbage_collection";
	 Flags.turn_off "cross_module_opt";   (* better module reuse *)
	 Flags.turn_on "quotation";           (* support for quotation-antiquotation *)
	 Flags.SMLserver := true;

structure K = KitMain(Compiler)
