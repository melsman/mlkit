import ../Edlib/edlib.pm
       ../Common/common.pm
       ../Common/mosmlhacks.pm
in

C_CONST.sml                                 (* first all the signatures *)
Lambda/CON.sml
Lambda/EXCON.sml
Lambda/LVARS.sml
Lambda/LAMBDA_EXP.sml
Lambda/LAMBDA_BASICS.sml
Lambda/OPT_LAMBDA.sml
Lambda/LAMBDA_STAT_SEM.sml
Lambda/COMPILER_ENV.sml
Lambda/ELIMINATE_EQ.sml
Lambda/COMPILE_DEC.sml

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

Kam/ADDRESS_LABELS.sml
Kam/COMP_LAMB.sml
Kam/COMP_LAMB_ENV.sml
Kam/KIT_ABSTRACT_MACHINE.sml
Kam/KAM_VAR.sml
Kam/KAM_BACKEND.sml
Kam/PHYS_SIZE_INF.sml
Kam/REGION_FLOW_GRAPH_PROFILING.sml

Cfg/CFG_INFO.sml
Cfg/CFG_INST.sml
Cfg/TRANSFORM_KBP.sml

C/C_INFO.sml
C/KBP_TO_C.sml
C/C_CODE.sml

COMPILE_BASIS.sml
COMPILE.sml

Backend/CLOS_CONV_ENV.sml
Backend/CALL_CONV.sml
Backend/CLOS_EXP.sml
Backend/LINE_STMT.sml
Backend/BACKEND_INFO.sml
Backend/REG_ALLOC.sml
Backend/FETCH_AND_FLUSH.sml
Backend/CALC_OFFSET.sml
Backend/SUBST_AND_SIMPLIFY.sml
Backend/CODE_GEN.sml
Backend/JUMP_TABLES.sml
(*
Backend/HpPaRisc/HPPA_RESOLVE_JUMPS.sml
Backend/HpPaRisc/HP_PA_DELAY_SLOT_OPTIMIZATION.sml
Backend/HpPaRisc/HP_PA_RISC.sml
*)
EXECUTION.sml

CConst.sml                         (* then all the functors *)

Lambda/Con.sml
Lambda/Excon.sml
Lambda/Lvars.sml
Lambda/LambdaExp.sml
Lambda/LambdaBasics.sml
Lambda/OptLambda.sml
Lambda/LambdaStatSem.sml
Lambda/CompilerEnv.sml

Lambda/EliminateEq.sml
Lambda/CompileDec.sml

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

Kam/AddressLabels.sml
Kam/CompLamb.sml
Kam/CompLambEnv.sml
Kam/KamVar.sml
Kam/PhysSizeInf.sml
Kam/RegionFlowGraphProfiling.sml

Cfg/CfgInst.sml
Cfg/CfgKitAM.sml
Cfg/TransformKbp.sml

(* #if defined(KIT_TARGET_C) *)

C/CInfo.sml
C/KbpToC.sml
C/CCode.sml
C/CKAMBackend.sml

(*
#else  
   
   (* the default is to use the HP backend; to compile the C backend
    * write   CM.SymVal.define("KIT_TARGET_C", 1);
    * before executing   CM.make(); 
    *)

   Hppa/DELAY_SLOT_OPTIMIZATION.sml
   Hppa/DelaySlotOptimization.sml
   Hppa/HPPA_INFO.sml
   Hppa/HPPA_RISC.sml
   Hppa/HpPaInfo.sml
   Hppa/HpPaRisc.sml
   Hppa/HppaKAMBackend.sml
   Hppa/INST_COUNT_PROFILING.sml
   Hppa/InstCountProfiling.sml
   Hppa/KBP_TO_HPPA.sml
   Hppa/KbpToHpPa.sml
   Hppa/RESOLVE_JUMPS.sml
   Hppa/ResolveJumps.sml

#endif
*)

Backend/ClosConvEnv.sml
Backend/CallConv.sml
Backend/ClosExp.sml
Backend/LineStmt.sml
Backend/RegAlloc.sml
Backend/FetchAndFlush.sml
Backend/CalcOffset.sml
Backend/SubstAndSimplify.sml
Backend/JumpTables.sml

(*Backend/HpPaRisc/BackendInfo.sml
Backend/HpPaRisc/HppaResolveJumps.sml
Backend/HpPaRisc/HpPaDelaySlotOptimization.sml
Backend/HpPaRisc/CodeGen.sml
Backend/HpPaRisc/HpPaRisc.sml
*)

   Backend/X86/INSTS_X86.sml
   Backend/X86/BackendInfoX86.sml
   Backend/X86/CodeGenX86.sml
   Backend/X86/InstsX86.sml



CompileBasis.sml
Compile.sml
BuildCompile.sml

end
