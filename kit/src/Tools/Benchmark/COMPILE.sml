signature COMPILE =
  sig
    (* compile src returns SOME(s,f) on success, where 
     * s is src (perhaps modified) and f is an executable 
     * file; returns NONE on error. *)
    val compile : string -> (string * string) option
  end
  