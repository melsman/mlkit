signature EXP_TO_JS = 
sig
  type Js and LambdaPgm

  val toJs : LambdaPgm -> Js

  val toString : Js -> string
  val toFile : string * Js -> unit  (* may raise Fail *)
end
