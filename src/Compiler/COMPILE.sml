(*$COMPILE*)

signature COMPILE =
  sig

    (* compiler for compiling structure declarations 
     * not containing functor applications. *)

    type CEnv and CompileBasis and strdec and target and linkinfo

    val compile : CEnv * CompileBasis * strdec list * string ->
      (CEnv * CompileBasis * target * linkinfo) Option

    val generate_link_code : linkinfo list -> target
    val emit: {target: target, filename:string} -> unit

    val reset : unit -> unit
    val commit : unit -> unit

  end 
