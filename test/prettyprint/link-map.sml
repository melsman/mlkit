fun write path text =
  let val out = TextIO.openOut path
  in TextIO.output (out,text); TextIO.closeOut out
  end
val dir = hd (CommandLine.arguments ())
val first = OS.Path.concat (dir,"space 'quote' \\ slash \206\187.o")
val second = OS.Path.concat (dir,"second.o")
val missing = OS.Path.concat (dir,"missing.o")
val () = List.app (fn (path,id) =>
  (write path ""; write (path ^ ".ir") ("MLKIT-IR 7\nidentity\t" ^ id ^ "\n")))
  [(first,"first"),(second,"second")]
val () = List.app (fn (platform,darwin) =>
  (write (OS.Path.concat (dir,platform ^ ".s"))
     (IRLocations.linkMap {darwin = darwin} [first,missing,second]);
   write (OS.Path.concat (dir,platform ^ "-empty.s"))
     (IRLocations.linkMap {darwin = darwin} [missing])))
  [("darwin",true),("elf",false)]
val () = write (OS.Path.concat (dir,"first-path")) (OS.FileSys.fullPath first)
val () = write (OS.Path.concat (dir,"second-path")) (OS.FileSys.fullPath second)
