(* Dataflow regressions for the post-SMA must-empty analysis. *)
structure EmptyResetTests =
struct
  open MulExp
  open AtInf
  fun assert message b = if b then () else raise Fail message
  fun fresh () = #1(Effect.freshRhoWithTy(Effect.PAIR_RT,Effect.initCone))
  val r = fresh()
  val q = fresh()
  fun tr e = TR(e,RegionExp.Mus [],[],ref [])
  val unit = tr (UB_RECORD [])
  fun alloc p = tr (RECORD(SOME(ATBOT p),[unit]))
  fun seq a b = tr (LET{k_let=false,pat=[],bind=a,scope=b})
  fun localRegion p t = tr (LETREGION{B=ref [],rhos=ref [(p,Mul.INF)],body=t})
  fun run t =
    let val PGM{expression,...} = eliminate_empty_resets
          (PGM{expression=t,export_datbinds=RegionExp.DATBINDS [],
               import_vars=ref NONE,export_vars=([],[],[]),export_basis=[],export_Psi=[]})
    in expression end
  fun recordTop (TR(RECORD(SOME(ATTOP _),_),_,_,_)) = true | recordTop _ = false
  fun recordBot (TR(RECORD(SOME(ATBOT _),_),_,_,_)) = true | recordBot _ = false
  fun body (TR(LETREGION{body,...},_,_,_)) = body | body _ = raise Fail "letregion"
  fun pair (TR(LET{bind,scope,...},_,_,_)) = (bind,scope) | pair _ = raise Fail "let"
  val (first,second) = pair(body(run(localRegion r (seq (alloc r) (alloc r)))))
  val () = assert "fresh allocation omits reset" (recordTop first)
  val () = assert "occupied region keeps reset" (recordBot second)
  val branch = tr (SWITCH_I{precision=63,switch=SWITCH(unit,[(IntInf.fromInt 0,alloc r)],SOME unit)})
  val (_,afterBranch) = pair(body(run(localRegion r (seq branch (alloc r)))))
  val () = assert "one allocating branch invalidates emptiness" (recordBot afterBranch)
  val pureBranch = tr (SWITCH_I{precision=63,switch=SWITCH(unit,[(IntInf.fromInt 0,unit)],SOME unit)})
  val (_,afterPure) = pair(body(run(localRegion r (seq pureBranch (alloc r)))))
  val () = assert "all empty branches preserve emptiness" (recordTop afterPure)
  val call = tr (APP(NONE,NOT_YET_DETERMINED,unit,unit))
  val (_,afterCall) = pair(body(run(localRegion r (seq call (alloc r)))))
  val () = assert "ML call invalidates captured-region facts" (recordBot afterCall)
  val closure = tr (FN{pat=[],body=alloc r,free=ref NONE,alloc=ATTOP q})
  val TR(FN{body=closureBody,...},_,_,_) = body(run(localRegion r closure))
  val () = assert "closure body cannot inherit entry emptiness" (recordBot closureBody)
  val handled = tr (HANDLE(alloc r,alloc r))
  val TR(HANDLE(a,b),_,_,_) = body(run(localRegion r handled))
  val () = assert "exception paths do not inherit emptiness" (recordBot a andalso recordBot b)
  fun ccall dest args = tr (CCALL({name="test_allocate",mu_result=RType.unitType,
                                 rhos_for_result=[(ATBOT dest,NONE)]},args))
  val c = ccall r []
  val (TR(CCALL({rhos_for_result,...},_),_,_,_),afterC) =
    pair(body(run(localRegion r (seq c (alloc r)))))
  val () = assert "fresh C destination uses attop"
    (case rhos_for_result of [(ATTOP _,NONE)] => true | _ => false)
  val () = assert "C allocation invalidates its destination" (recordBot afterC)
  val (_,afterOtherC) = pair(body(run(localRegion r (seq (ccall q []) (alloc r)))))
  val () = assert "unrelated C destination preserves fact" (recordTop afterOtherC)
  val TR(CCALL({rhos_for_result=afterOperand,...},_),_,_,_) =
    body(run(localRegion r (ccall r [alloc r])))
  val () = assert "C operands precede its allocation"
    (case afterOperand of [(ATBOT _,NONE)] => true | _ => false)
  val exposed = TR(UB_RECORD [],RegionExp.Mus [RType.mkBOX(RType.unitType,r)],[],ref [])
  val (_,afterExposed) = pair(body(run(localRegion r (seq (ccall q [exposed]) (alloc r)))))
  val () = assert "C operands exposing a region invalidate its fact" (recordBot afterExposed)
  val finite = tr (LETREGION{B=ref [],rhos=ref [(r,Mul.NUM 1)],body=alloc r})
  val () = assert "finite-region storage is not assumed empty" (recordBot(body(run finite)))
  val variable = tr (VAR{lvar=Lvars.newLvar(),il=([],[],[]),plain_arreffs=[],
                         fix_bound=true,rhos_actuals=ref [ATBOT r],other=()})
  val TR(VAR{rhos_actuals,...},_,_,_) = body(run(localRegion r variable))
  val () = assert "region argument modes are preserved"
    (case !rhos_actuals of [ATBOT _] => true | _ => false)
  val () = Flags.turn_off "eliminate_empty_region_resets"
  val () = assert "disable switch preserves reset" (recordBot(body(run(localRegion r (alloc r)))))
  val () = Flags.turn_on "eliminate_empty_region_resets"
  val () = print "empty-region reset analysis passed\n"
end
