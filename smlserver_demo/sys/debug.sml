(* This file is used only to find a library file containing an error, which is reported 
   in the log as for instance:
     Warning: script file raised exn

   You can simple place this file in source.pm at various places. *)
val _ = Ns.log(Ns.Notice,"Was here...")
