(*$DECTREE_DT*)
(* Just the bare datatype for decision trees. *)

signature DECTREE_DT =
  sig
    type lab
    type lvar
    type id
    type longid
    type scon
    type pat
    type TypeInfo
    eqtype (*pah!*) RuleNum sharing type RuleNum = int
    type (''a, 'b) map
    type CEnv

    datatype DecisionTree =
        LAB_DECOMPOSE of {bind: lvar,
			  parent: lvar,
			  lab: lab,
			  child: DecisionTree,
			  info: TypeInfo
			 }

      | CON_DECOMPOSE of {bind: lvar, parent: lvar, child: DecisionTree,info:TypeInfo}
      | EXCON_DECOMPOSE of {bind: lvar, parent: lvar, child: DecisionTree,info:TypeInfo}

      | CON_SWITCH of {arg: lvar,
		       selections: (longid, (TypeInfo * DecisionTree)) map,
		       wildcard: DecisionTree Option
				(* An `option' because we may notice that all
				   the constructors are present. *)
		      }
      | SCON_SWITCH of {arg: lvar,
			selections: (scon, DecisionTree) map,
			wildcard: DecisionTree
		       }
      | EXCON_SWITCH of {arg: lvar,
			 selections: (longid * DecisionTree) list,
			 wildcard: DecisionTree
			}

      | END of {ruleNum: RuleNum, environment: CEnv}
      | FAIL
  end;

(*$DECISION_TREE: DECTREE_DT*)

signature DECISION_TREE =
  sig
    include DECTREE_DT
    type Decision and TyVar and tyvar and SType and LType

    val decisionTree:
      {compileTypeScheme: TyVar list * SType -> tyvar list * LType,
       pats: pat list, root: lvar, decisions: Decision list} -> DecisionTree

    type StringTree
    val layoutDecisionTree: DecisionTree -> StringTree
  end;
