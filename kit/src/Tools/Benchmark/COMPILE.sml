signature COMPILE =
  sig
    (* [compile kitdir srcfile options] returns SOME(s,f) on success, where 
     * s is srcfile (perhaps modified) and f is an executable 
     * file; returns NONE on error. kitdir is the directory where the
     * ML Kit is installed, which is used by SML/NJ for bundling an
     * executable with heap2exec. *)
    val compile : string           (*kitdir*)
               -> string           (*compile time flags*)
               -> string           (*src file*)
               -> string list      (*test-file options*)
               -> (string * string) option
  end
  