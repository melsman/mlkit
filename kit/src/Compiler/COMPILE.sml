(*$COMPILE*)

signature COMPILE =
  sig

    (* compiler for compiling structure declarations not containing
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CEnv and CompileBasis and strdec and target and linkinfo

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    val compile : CEnv * CompileBasis * strdec list * string -> res

    val generate_link_code : linkinfo list -> target
    val emit: {target: target, filename:string} -> unit

    val reset : unit -> unit
    val commit : unit -> unit

  end 
