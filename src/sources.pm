import 
    Edlib/edlib.pm
    Common/common.pm
    Compiler/compiler.pm
in
    Manager/FREE_IDS.sml
    Common/Elaboration.sml
    Compiler/EXECUTION.sml

    Manager/INT_MODULES.sml
    Manager/OPACITY_ELIM.sml
    Manager/PARSE_ELAB.sml
    Manager/MANAGER_OBJECTS.sml
    Manager/MANAGER.sml

    Manager/FreeIds.sml
    Manager/IntModules.sml
    Manager/OpacityElim.sml
    Manager/ParseElab.sml
    Manager/ManagerObjects.sml
    Manager/MspComp.sml (* new 2001-01-27, Niels *)
    Manager/Manager.sml

    Compiler/BuildCompile.sml
    Compiler/Backend/NativeCompile.sml

    Compiler/Backend/X86/ExecutionX86.sml
    Compiler/Backend/Dummy/Execution__dummy.sml
    (* Compiler/Backend/PaML/ExecutionPaML.sml *)
    Compiler/Backend/HpPaRisc/ExecutionHPPA.sml 
(*    Compiler/Backend/KAM/ExecutionKAM.sml *)
    
    Common/SMLofNJOnKit.sml
    Common/KitCompiler.sml
    Common/KitOnKit.sml
end
