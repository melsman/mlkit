signature CONTROL = sig
  val printer_set : (string -> unit) -> unit
  val printer_get : unit -> (string -> unit)
end

(* 
 [printer_set p] installs the printer p to be used as the
 underlying printer for the top-level print function and when
 printing to stdout and stderr.

 [printer_get()] returns the current installed underlying printer.  
*)
