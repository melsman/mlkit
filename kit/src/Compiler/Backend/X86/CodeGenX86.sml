(* Generate Target Code *)

functor CodeGenX86(structure BackendInfo : BACKEND_INFO
		   structure InstsX86 : INSTS_X86
		     sharing type InstsX86.reg = BackendInfo.reg
		     sharing type InstsX86.label = BackendInfo.label
		   structure Con : CON
		   structure Excon : EXCON
		   structure Lvars : LVARS
		     sharing type Lvars.lvar = BackendInfo.lvar
		   structure Lvarset : LVARSET
		     sharing type Lvarset.lvar = Lvars.lvar
		     sharing type Lvarset.lvarset = BackendInfo.lvarset
		   structure Labels : ADDRESS_LABELS
		     sharing type Labels.label = BackendInfo.label
		   structure CallConv: CALL_CONV
		   structure LineStmt: LINE_STMT
 		   sharing type Con.con = LineStmt.con
		   sharing type Excon.excon = LineStmt.excon
		   sharing type Lvars.lvar = LineStmt.lvar = CallConv.lvar
                   sharing type Labels.label = LineStmt.label
		   sharing type CallConv.cc = LineStmt.cc
	           structure SubstAndSimplify: SUBST_AND_SIMPLIFY
		   sharing type SubstAndSimplify.lvar = LineStmt.lvar 
                   sharing type SubstAndSimplify.place = LineStmt.place
                   sharing type SubstAndSimplify.LinePrg = LineStmt.LinePrg
	           structure PP : PRETTYPRINT
		   sharing type PP.StringTree = LineStmt.StringTree
		   structure Flags : FLAGS
	           structure Report : REPORT
		   sharing type Report.Report = Flags.Report
		   structure Crash : CRASH) : CODE_GEN =       
  struct

    structure I = InstsX86

    type label = Labels.label
    type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) LineStmt.LinePrg
    type StoreTypeSS = SubstAndSimplify.StoreTypeCO
    type AtySS = SubstAndSimplify.Aty
    type reg = I.reg
    type offset = int
    type asm_prg = I.asm_prg
      
    val CG : {main_lab:label,code:(StoreTypeSS,offset,AtySS) LinePrg,
	      imports:label list,exports:label list} -> asm_prg = 
      fn _ => {top_decls=[],
	       init_code=[],
	       exit_code=[],
	       static_data=[]}
																  

    fun emit a : unit = I.emit a

  end








