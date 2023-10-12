signature REPL = sig
  val run : unit -> OS.Process.status
end

structure Repl : REPL = struct

structure PE = ParseElab
structure ME = ModuleEnvironments
structure IB = InfixBasis
structure EB = ME.B

fun repl (stepno, infB, elabB, state) : OS.Process.status =
    let val absprjid = ME.mk_absprjid (Int.toString stepno)
        val () = print (Int.toString stepno  ^ ":\n")
    in case PE.parse_elab_stdin {infB=infB, elabB=elabB,
                                 absprjid=absprjid, state=state} of
           (SOME state', PE.SUCCESS {report,infB=infB',elabB=elabB',topdec}) =>
           ( Report.print report
           ; repl (stepno+1,
                   IB.compose(infB,infB'),
                   EB.plus(elabB,elabB'),
                   state')
           )
         | (_, PE.FAILURE (report,errs)) =>
           ( Report.print report
           ; repl (stepno+1,infB,elabB,
                   PE.begin_stdin())
           )
         | (NONE, PE.SUCCESS _) => raise Fail "Repl.repl.impossible"
    end

fun run () : OS.Process.status =
    repl (0, IB.emptyB, EB.initial(), PE.begin_stdin())

end
