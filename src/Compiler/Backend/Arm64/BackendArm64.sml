(* Share the instantiated backend between the compiler driver and the
 * instruction-emission regression harness. Flags must be registered once. *)
structure BackendArm64 = struct
  structure NativeCompile = NativeCompile(structure RegisterInfo=InstsArm64.RI)
  structure CodeGen = CodeGenArm64(structure LineStmt=NativeCompile.LineStmt
                                 structure SubstAndSimplify=NativeCompile.SubstAndSimplify)
end
