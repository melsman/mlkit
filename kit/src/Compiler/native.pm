import ../Edlib/edlib.pm
       ../Pickle/pickle.pm
       ../Common/tools.pm
       ../Common/syntax_objects.pm
       ../Common/special_objects.pm
       compiler_objects.pm
       ../Common/basics.pm
       ../Manager/manager.pm
       compiler.pm
       regions.pm
in
  (* Native Backend *)
  Backend/LINE_STMT.sml
  Backend/REG_ALLOC.sml
  Backend/FETCH_AND_FLUSH.sml
  Backend/CALC_OFFSET.sml
  Backend/SUBST_AND_SIMPLIFY.sml
  Backend/LineStmt.sml
  Backend/RegAlloc.sml
  Backend/FetchAndFlush.sml
  Backend/CalcOffset.sml
  Backend/SubstAndSimplify.sml
  Backend/NativeCompile.sml
 
  (* X86 Backend *)
  Backend/CODE_GEN.sml
  Backend/X86/INSTS_X86.sml
  Backend/X86/InstsX86.sml
  Backend/X86/CodeGenX86.sml
  Backend/X86/ExecutionX86.sml
  ../Common/KitX86.sml
end
