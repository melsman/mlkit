structure Apache :> APACHE =
  struct
  
  structure Conn = 
  	struct 

	exception OutOfMemory;

	type rr = int
	fun getReqRec () : int  = prim("__serverGetCtx", ())


	fun echo_rr (r : rr, s: string) : unit = 
	  (*prim("nssml_returnFile", (r, s, OutOfMemory))*)
	  prim("apsml_returnHTML", (r, s, OutOfMemory)) 


   end
	fun echo s = Conn.echo_rr(Conn.getReqRec (), s)
end

