import 
  ../Edlib/edlib.pm
  ../Common/common.pm

in

Lambda/CON.sml
Lambda/EXCON.sml
Lambda/LVARS.sml
Lambda/COMPILER_ENV.sml
COMPILE_BASIS.sml
COMP_BASIS.sml
COMPILE.sml
Lambda/LAMBDA_EXP.sml
Lambda/LAMBDA_BASICS.sml
Lambda/OPT_LAMBDA.sml
Lambda/LAMBDA_STAT_SEM.sml
Lambda/ELIMINATE_EQ.sml
Lambda/COMPILE_DEC.sml
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
Regions/ADDRESS_LABELS.sml
Regions/PHYS_SIZE_INF.sml
Regions/REGION_FLOW_GRAPH_PROFILING.sml
Backend/KAM/KAM.sml
Backend/KAM/CODE_GEN_KAM.sml
Backend/KAM/OPCODES_KAM.sml
Backend/KAM/BUFF_CODE.sml
Backend/KAM/RESOLVE_LOCAL_LABELS.sml
Backend/KAM/EMIT_CODE.sml
Backend/CLOS_CONV_ENV.sml
Backend/CALL_CONV.sml
Backend/CLOS_EXP.sml
Backend/LINE_STMT.sml
Backend/BACKEND_INFO.sml 
Backend/REGISTER_INFO.sml 
Backend/REG_ALLOC.sml
Backend/FETCH_AND_FLUSH.sml
Backend/CALC_OFFSET.sml
Backend/SUBST_AND_SIMPLIFY.sml
Backend/CODE_GEN.sml
Backend/JUMP_TABLES.sml
Backend/X86/INSTS_X86.sml
(*Backend/HpPaRisc/HPPA_RESOLVE_JUMPS.sml*)
(*Backend/HpPaRisc/HP_PA_DELAY_SLOT_OPTIMIZATION.sml*)
(*Backend/HpPaRisc/HP_PA_RISC.sml*)

Lambda/Con.sml
Lambda/Excon.sml
Lambda/Lvars.sml
Lambda/CompilerEnv__dummy.sml
CompileBasis__dummy.sml
CompileBasis.sml

CompBasis.sml
Compile.sml

Lambda/LambdaExp.sml
Lambda/LambdaBasics.sml
Lambda/OptLambda.sml
Lambda/LambdaStatSem.sml
Lambda/CompilerEnv.sml
Lambda/EliminateEq.sml
Lambda/CompileDec.sml

Regions/RegConst.sml
Regions/AtInf.sml
Regions/DropRegions.sml
Regions/Effect.sml
Regions/LocallyLiveVariables.sml
Regions/Mul.sml
Regions/MulExp.sml
Regions/MulInf.sml
Regions/RType.sml
Regions/RegFlow.sml
Regions/RegInf.sml
Regions/RegionExp.sml
Regions/RegionStatEnv.sml
Regions/SpreadDataType.sml
Regions/SpreadExpression.sml
Regions/TestSpreadDataType.sml
Regions/TestSpreadExp.sml

(* For all backends *)
Regions/AddressLabels.sml
Regions/PhysSizeInf.sml
Regions/RegionFlowGraphProfiling.sml

(* Bytecode Backend *)
(*
Backend/KAM/BuiltInCFunctionsKAM.sml
Backend/KAM/Kam.sml
Backend/KAM/OpcodesKAM.sml
Backend/KAM/BuffCode.sml
Backend/KAM/ResolveLocalLabels.sml
Backend/KAM/EmitCode.sml
Backend/KAM/CodeGenKAM.sml
 need new libraries 2001-01-27, Niels*)

(* Native Backend *)
Backend/ClosConvEnv.sml
Backend/CallConv.sml
Backend/ClosExp.sml
Backend/LineStmt.sml
Backend/BackendInfo.sml
Backend/RegAlloc.sml
Backend/FetchAndFlush.sml
Backend/CalcOffset.sml
Backend/SubstAndSimplify.sml
Backend/JumpTables.sml

(* X86 Backend *)
Backend/X86/CodeGenX86.sml
Backend/X86/InstsX86.sml

(* HPPA Backend *)
(*Backend/HpPaRisc/HppaResolveJumps.sml*)
(*Backend/HpPaRisc/HpPaDelaySlotOptimization.sml*)
(*Backend/HpPaRisc/CodeGen.sml*)
(*Backend/HpPaRisc/HpPaRisc.sml*)


end

