signature FLATTEN_REFINE =
sig
  type tyvar
  type LambdaPgm
  (* Refine only requested function parameters and their use dependencies.
     No cloning or evaluation changes. Unsupported/open requirements fail. *)
  val refine : tyvar list -> LambdaPgm -> LambdaPgm option
end
