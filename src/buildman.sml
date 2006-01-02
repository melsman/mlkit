val _ = (SMLofNJ.Internals.GC.messages false;
	 print "\n ** Building the MLKit compiler MANUAL [X86 Backend] **\n\n";
	 CM.make' "native_man.cm");

val default_root_dir = OS.Path.mkCanonical(OS.Path.concat(OS.FileSys.getDir(), ".."))
val kitbinkit_path = OS.Path.concat(default_root_dir, "bin/createman")
val _ = SMLofNJ.exportFn(kitbinkit_path, Man.main)

