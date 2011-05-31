signature TIMESTAMP =
  sig
    eqtype stamp
    val new: unit -> stamp
    val stamp2int : stamp -> int  (* an injective mapping from stamps to integers *)
    val print: stamp -> string
  end;
