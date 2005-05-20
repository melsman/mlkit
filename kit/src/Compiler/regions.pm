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
in
  Regions/REG_CONST.sml
  Regions/AT_INF.sml
  Regions/DROP_REGIONS.sml
  Regions/EFFECT.sml
  Regions/LOCALLY_LIVE_VARIABLES.sml
  Regions/MUL.sml
  Regions/REGION_EXP.sml
  Regions/MUL_EXP.sml
  Regions/MUL_INF.sml
  Regions/REGINF.sml
  Regions/REGION_STAT_ENV.sml
  Regions/REG_FLOW.sml
  Regions/RTYPE.sml
  Regions/SPREAD_DATATYPE.sml 
  Regions/SPREAD_EXPRESSION.sml

  Regions/PHYS_SIZE_INF.sml
  Regions/REGION_FLOW_GRAPH_PROFILING.sml

  Regions/RegConst.sml
  Regions/Effect.sml 
  Regions/RType.sml
  Regions/RegionExp.sml 
  Regions/RegionStatEnv.sml
  Regions/EffVarEnv.sml
  ../Common/QUASI_ENV.sml
  ../Common/QuasiEnv.sml
  Regions/Mul.sml
  Regions/MulExp.sml 
  Regions/MulInf.sml
  Regions/LocallyLiveVariables.sml
  Regions/RegFlow.sml
  Regions/RegInf.sml 
  Regions/SpreadDataType.sml
  Regions/SpreadExpression.sml 
  Regions/AtInf.sml
  Regions/DropRegions.sml
  Regions/PhysSizeInf.sml 
  Regions/RegionFlowGraphProfiling.sml
(*     
     Regions/TestSpreadDataType.sml
     Regions/TestSpreadExp.sml
*)
  CompBasis.sml
  Compile.sml

  (* Generic Modules; used both by KAM and X86 backend *)

  Backend/CLOS_CONV_ENV.sml 
  Backend/CLOS_EXP.sml

  Backend/CALL_CONV.sml  
  Backend/BACKEND_INFO.sml 
  Backend/REGISTER_INFO.sml 
  Backend/JUMP_TABLES.sml

  Backend/BackendInfo.sml
  Backend/ClosConvEnv.sml
  Backend/CallConv.sml
  Backend/ClosExp.sml

  Backend/JumpTables.sml
  CompileBasis.sml
end
