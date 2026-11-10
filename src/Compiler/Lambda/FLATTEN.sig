signature FLATTEN =
sig
  type LambdaPgm
  type env
  type repvar
  datatype feature = Reference | Tuple
  datatype decision = Eligible | Disabled | Rejected of string
  type candidate = {outer:repvar, index:int, inner:repvar option, width:int,
                    feature:feature, decision:decision}
  val requested : unit -> bool
  val active : unit -> bool
  (* Analysis returns a private annotated copy; no allocation or access is rewritten. *)
  val analyse : {env:env, pgm:LambdaPgm, references:bool, tuples:bool}
                -> {pgm:LambdaPgm, candidates:candidate list}
  (* Undefined for the disappearing outer field. Inner indices are usable only
     in accesses syntactically composed with that outer selection. *)
  val accessMap : candidate -> repvar * int -> int option
  (* Apply one eligible choice to the annotated program returned by analyse. *)
  val transform : candidate -> LambdaPgm -> LambdaPgm
  val optimise : {enabled:bool, env:env} -> LambdaPgm -> LambdaPgm
end
