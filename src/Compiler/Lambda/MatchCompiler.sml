(*$MatchCompiler : SCON IDENT LAB DEC_GRAMMAR COMPILER_ENV
        ELAB_INFO LVARS FINMAP BASIC_IO REPORT FLAGS PRETTYPRINT
        CRASH MATCH_COMPILER AndOrTree DecisionList DecisionTree
        PatBindings MatchWarnings*)

(* MatchCompiler refers to the specialised match compiler functors
   explicitly, so that they aren't visible to the rest of the
   compiler at large. I don't generally like such non-local functor
   references, but it's worth it in this case to achieve modularity.
   In any case, it looks rather impressive. *)

functor MatchCompiler(structure SCon: SCON
                      structure Lab: LAB
		      structure Ident: IDENT

                      structure Grammar: DEC_GRAMMAR
                        sharing type Grammar.longid = Ident.longid
                            and type Grammar.scon = SCon.scon
                            and type Grammar.id = Ident.id
                            and type Grammar.lab = Lab.lab

                      structure CompilerEnv: COMPILER_ENV
			sharing type CompilerEnv.id = Ident.id

                      structure ElabInfo : ELAB_INFO
		      sharing type Grammar.info = ElabInfo.ElabInfo
                      structure Lvars: LVARS
                        sharing type CompilerEnv.lvar = Lvars.lvar

                      structure FinMap: FINMAP
                      structure BasicIO: BASIC_IO
                      structure Report: REPORT
                      structure Flags: FLAGS

                      structure PrettyPrint : PRETTYPRINT
                        sharing type FinMap.StringTree 
			             = Grammar.StringTree
                                     = CompilerEnv.StringTree
                                     = ElabInfo.StringTree
                                     = PrettyPrint.StringTree
                            and type PrettyPrint.Report = Report.Report

                      structure Crash: CRASH
                     ): MATCH_COMPILER =
  struct
    structure AndOrTree =
      AndOrTree(structure Ident = Ident
                structure SCon = SCon
                structure Lab = Lab
                structure Grammar = Grammar
                structure ElabInfo = ElabInfo
                structure FinMap = FinMap
                structure PrettyPrint = PrettyPrint
                structure Crash = Crash
               )

    structure DecisionList =
      DecisionList(structure Lab = Lab
		   structure Ident = Ident
                   structure SCon = SCon
                   structure AndOrTree = AndOrTree
                   structure FinMap = FinMap
                   structure PP = PrettyPrint
                   structure Crash = Crash
                  )

    structure DecisionTree =
      DecisionTree(structure Lab = Lab
		   structure Ident = Ident
                   structure SCon = SCon
                   structure Grammar = Grammar
                   structure CompilerEnv = CompilerEnv
                   structure DecisionList = DecisionList

                   structure PatBindings =
                     PatBindings(structure Lab = Lab
				 structure Ident = Ident
                                 structure Grammar = Grammar
                                 structure CompilerEnv = CompilerEnv
                                 structure ElabInfo = ElabInfo
                                 structure Lvars = Lvars
                                 structure PP = PrettyPrint
                                 structure FinMap = FinMap
                                 structure Crash = Crash
                                )

                   structure Lvars = Lvars
                   structure TypeInfo = ElabInfo.TypeInfo
                   structure FinMap = FinMap
                   structure BasicIO = BasicIO
                   structure Report = Report
                   structure Flags = Flags
                   structure PP = PrettyPrint
                   structure Crash = Crash
                  )

    structure MatchWarnings =
      MatchWarnings(structure Ident = Ident
                    structure SCon = SCon
                    structure CompilerEnv = CompilerEnv
                    structure DecTreeDT = DecisionTree
                    structure FinMap = FinMap
                   )

    open DecisionTree           (* Export the DecisionTree datatype, as well as
                                   StringTree and layout declarations. *)

   (* Some type abbreviations to match up to the MATCH_COMPILER signature. *)
    type pat = Grammar.pat
    type lvar = Lvars.lvar
    type scon = SCon.scon
    type lab = Lab.lab
    type longid = Ident.longid
    type (''a, 'b) map = (''a, 'b) FinMap.map
    type RuleNum = int

    val pr = Report.print o PrettyPrint.reportStringTree

    infix //
    val op // = Report.//

   (* reporting of unreachable/inexhaustive matches. I'm not sure if the
      depths of the compiler should have any print statements at all; they
      should probably be passed out symbolically to the outermost level. *)

    fun report(msg, pats, drawArrow) =
      let
        val layoutPat = Grammar.layoutPat

        fun pr(i, pat :: rest) =
                (Report.line((if drawArrow i then "  =--> "
                                             else "       "
                             ) ^ PrettyPrint.flatten (PrettyPrint.format (70, layoutPat pat))
                               ^ " => ..."
                            )
                 // pr(i+1, rest)
                )
          | pr(_, nil) = Report.null
      in
        Report.line msg // pr(1, pats)
      end

    fun issueWarnings(pats, tree, {warnInexhaustive, warnNoBindings}) =
      let
        open MatchWarnings

        fun downfrom' 0 = nil
          | downfrom' n = n :: downfrom'(n-1)

        val downfrom = EqSet.fromList o downfrom'

        val numRules = List.size pats
        val allRules = downfrom numRules
        val unreachables = EqSet.difference allRules (reachable tree)
      in
        case EqSet.isEmpty unreachables
          of true => ()                 (* Nothing to report *)
           | _ => Report.print(report("Some rules are unreachable",
                                      pats, fn i => EqSet.member i unreachables
                                     )
                              );

        if warnNoBindings andalso not(binds tree) then
          Report.print(report("Binding declares no variables",
                              pats, fn _ => false
                             )
                      )
        else ();

        if warnInexhaustive andalso not(exhaustive tree) then
          Report.print(report("Match is not exhaustive", pats, fn _ => false))
        else ()
      end
              

    fun matchCompiler(_, nil, _) = Crash.impossible "matchCompiler(nil)"
      | matchCompiler(root: lvar, pats: pat list,
                      flags: {warnInexhaustive: bool, warnNoBindings: bool}
                     ): DecisionTree =
          let
            val andOrTree = AndOrTree.buildAndOrTree pats

            val _ =
              if !Flags.DEBUG_MATCHCOMPILER then
                pr (PrettyPrint.NODE {start="And/Or tree: ", finish="", indent=3,
				      children=[AndOrTree.layoutAndOrTree andOrTree],
				      childsep=PrettyPrint.NONE})
              else ()

            val decisions = DecisionList.decisions andOrTree

            val _ =
              if !Flags.DEBUG_MATCHCOMPILER then
                pr (PrettyPrint.NODE {start="Decision list: ", finish="", indent=3,
				      children=map DecisionList.layoutDecision decisions,
				      childsep=PrettyPrint.RIGHT ", "})
              else ()

            val decTree =
              DecisionTree.decisionTree
                {pats=pats, root=root, decisions=decisions}
            
            val _ =
              if !Flags.DEBUG_MATCHCOMPILER then
                pr (PrettyPrint.NODE {start="Decision tree(" ^ Lvars.pr_lvar root ^ "): ",
				      finish="", indent=3, childsep=PrettyPrint.NONE,
				      children=[DecisionTree.layoutDecisionTree decTree]})
              else ()
          in
            issueWarnings(pats, decTree, flags);
            decTree
          end
  end;
