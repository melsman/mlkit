(*$STATE_MONAD
 *)

signature STATE_MONAD = 
sig
  type state
  val initialState : state
  type 'a S 
  val unitS : 'a -> 'a S
  val bindS : ('a S) * ('a -> 'b S) -> 'b S
  val mapS  : ('a -> 'b) -> 'a S -> 'b S
  val joinS : 'a S S -> 'a S
  val ooS   : ('b -> 'c S) * ('a -> 'b S) -> 'a -> 'c S
  val mapList : ('a -> 'b S) -> 'a list -> 'b list S
  val resetS  : unit S -> unit S
  val getS  : unit S -> state S 
      (* getS is used to get a computation which delivers the current state *)
  val plusS : state S -> unit S
  val setS  : state S -> unit S
  val showS : state -> 'a S -> 'a * state
      (* showS s m gives the value of the computation m and the state;
       * s is supposed to be the current state 
       *)
end;

(*$STATE
 *)
signature STATE =
sig
  type state
  val initialState : state
  val plus : state * state -> state
end;

