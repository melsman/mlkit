signature CalcInterface =
sig

    datatype event =
	DIGIT of int
      | ENTER
      | CLEAR
      | PLUS
      | MINUS
      | MULTIPLY
      | DIVIDE
(*      | DOT
*)
    val getEvent : unit -> event

    val displayLines : string list -> unit
    val displayAccum : string -> unit


    (* Error signaling functions *)
    val beep : unit -> unit
    val internalError : string -> unit

end
