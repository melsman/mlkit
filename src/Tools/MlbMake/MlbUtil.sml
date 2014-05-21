structure MlbUtil :>
  sig
    val vchat0 : (unit -> bool) -> string -> unit
    val quot : string -> string
    val warn : string -> unit
    val error : string -> 'a
  end =
    struct
	fun quot s = "'" ^ s ^ "'"

	fun warn (s : string) = print ("\nWarning: " ^ s ^ ".\n\n")
	    
	local
	    fun err s = print ("\nError: " ^ s ^ ".\n\n"); 
	in
	    fun error (s : string) = 
                raise Fail s (* Error is then printed in KitCompiler *)
(*
	    fun error (s : string) = (err s; raise Fail "error")	    
	    fun errors (ss:string list) = 
		(app err ss; raise Fail "error")
*)
	end
    
	fun vchat0 (verbose:unit -> bool) s = 
	    if verbose() then print (" ++ " ^ s ^ "\n") 
	    else ()
		
	fun system verbose cmd : unit = 
	    (vchat0 verbose ("Executing command: " ^ cmd) ;
	     let 
		 val status = OS.Process.system cmd
		     handle _ => error ("Command failed: " ^ quot cmd)
	     in if OS.Process.isSuccess status then ()
                else error ("Command failed: " ^ quot cmd)
	     end
	     )
    end

