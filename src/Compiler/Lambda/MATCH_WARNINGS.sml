(*$MATCH_WARNINGS*)
(* Both kinds of match warning (inexhaustive matches and unreachable rules)
   can be detected by inspection from a DecisionTree, but only on the
   assumption that bogus wildcard switches have been taken out of CON_SWITCH
   nodes by the match compiler (otherwise we have no easy way of telling
   that `fn true => 1 | false => 2 | _ => 3' has a redundant clause). *)

signature MATCH_WARNINGS =
  sig
    type DecisionTree
    type RuleNum sharing type RuleNum = int

    val exhaustive: DecisionTree -> bool
    val reachable: DecisionTree -> RuleNum EqSet.Set
    val binds: DecisionTree -> bool
  end;
