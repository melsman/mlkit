signature FLATTEN =
sig
  type LambdaPgm
  type env
  type repvar
  datatype feature = Reference | Tuple | Float
  datatype decision = Eligible | Disabled | Rejected of string
  (* order maps physical indices to spliced logical indices, placing raw fields
     first. The empty list represents the identity permutation. *)
  type candidate = {outer:repvar, index:int, inner:repvar option, width:int, order:int list,
                    feature:feature, decision:decision}
  val requested : unit -> bool
  val active : unit -> bool
  (* Analysis returns a private annotated copy; no allocation or access is rewritten. *)
  val analyse : {env:env, pgm:LambdaPgm, references:bool, tuples:bool}
                -> {pgm:LambdaPgm, candidates:candidate list}
  (* Undefined for the disappearing outer field. Inner indices are usable only
     in accesses syntactically composed with that outer selection. Float fields
     require dedicated unboxing/read/store rules. Defined indices include the
     raw-prefix permutation. *)
  val accessMap : candidate -> repvar * int -> int option
  (* Apply one eligible choice to the annotated program returned by analyse. *)
  val transform : {env:env, candidate:candidate, pgm:LambdaPgm} -> LambdaPgm
  (* Optionally refine candidate-blocking private function type parameters,
     retaining only trials that enable additional allocation removal.
     First rewrite through ordinary polymorphic constructor instances. Specialise
     private ground datatype-use families only for additional payload rewrites;
     preserve residual field parameters and discard unchanged trial families.
     Declaration templates
     and analysis-only family tokens never escape this entry point. *)
  val optimise : {enabled:bool, env:env} -> LambdaPgm -> LambdaPgm
end
