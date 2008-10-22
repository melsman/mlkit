signature EXP_TO_JS = 
sig
  type Js and LambdaPgm

  structure Env : sig
    type t
    val empty : t
    val initial : t
    val plus : t * t -> t
    val restrict : t * Con.con list -> t
    val enrich : t * t -> bool
    val pu : t Pickle.pu
  end

  val toJs : Env.t * LambdaPgm -> Js * Env.t

  val toString : Js -> string
  val toFile : string * Js -> unit  (* may raise Fail *)

  val exports : LambdaPgm -> string list
  val imports : LambdaPgm -> string list

end
