(* Shared by the compiler driver and direct IR regression tests. *)
structure BackendX64 =
struct
  structure NativeCompile = NativeCompile(structure RegisterInfo = InstsX64.RI)
  structure CodeGen = CodeGenX64(structure LineStmt = NativeCompile.LineStmt
                                structure SubstAndSimplify = NativeCompile.SubstAndSimplify)
end
