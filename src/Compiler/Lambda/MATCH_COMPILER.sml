(*$MATCH_COMPILER: DECTREE_DT*)

(* The match compiler interface; the actual match compiler is built from
   a number of sub-functors, but this top-level interface is the only one
   which the rest of the compiler cares about. Given a series of patterns,
   it returns a DecisionTree (which is essentially an abstract form of the
   final lambda-code), in which all the lvars for identifiers and temporaries
   have been generated. At each leaf of the decision tree, there's a
   single rule number (the rule reached by this series of decisions), and an
   environment from identifiers to lvars, which is used to compile the
   right-hand-side expression for this rule. Nice, huh? *)

signature MATCH_COMPILER =
  sig
    include DECTREE_DT

    type SType and TyVar and LType and tyvar
    val matchCompiler: (TyVar list * SType -> tyvar list * LType) ->
      lvar * pat list * {warnInexhaustive: bool, warnNoBindings: bool}
      -> DecisionTree			(* these flags are set when the
					   warnings are required. *)

    type StringTree
    val layoutDecisionTree: DecisionTree -> StringTree
  end;
