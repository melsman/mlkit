signature FLATTEN =
sig
  type LambdaPgm
  (* The caller supplies the global optimiser setting. Data-flattening
     switches are independent of the argument-flattening switches. *)
  val optimise : {enabled:bool} -> LambdaPgm -> LambdaPgm
end
