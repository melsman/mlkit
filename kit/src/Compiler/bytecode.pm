import
  ../Edlib/edlib.pm
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
  (* Bytecode Backend *)
  Backend/KAM/KAM.sml
  Backend/KAM/CODE_GEN_KAM.sml
  Backend/KAM/OPCODES_KAM.sml
  Backend/KAM/BUFF_CODE.sml
  Backend/KAM/RESOLVE_LOCAL_LABELS.sml
  Backend/KAM/EMIT_CODE.sml

  Backend/KAM/BuiltInCFunctionsKAM.sml
  Backend/KAM/Kam.sml
  Backend/KAM/OpcodesKAM.sml
  Backend/KAM/BuffCode.sml
  Backend/KAM/ResolveLocalLabels.sml
  Backend/KAM/EmitCode.sml
  Backend/KAM/CodeGenKAM.sml

  Backend/KAM/ExecutionKAM.sml
  ../Common/KitKAM.sml
end
