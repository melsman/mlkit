signature COMPILE =
  sig
    (* compile src returns SOME(f) on 
     * success, where f is an executable 
     * file; returns NONE on error. *)
    val compile : string -> string option
  end
  