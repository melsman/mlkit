(*$AND_OR_TREE: EqSetList*)

signature AND_OR_TREE =
  sig
    eqtype (*pah!*) RuleNum sharing type RuleNum = int

    type pat				(* From abstract syntax: patterns. *)
    type lab				(* Record/tuple labels. *)
    type TypeInfo			(* Hints to enumerate labs/cons. *)
    type id
    type longid
    type scon				(* Special constants (literals). *)
    type (''a, 'b) map			(* Finite maps (from FinMap). *)


    datatype NodeInfo =
      NODEINFO of {rules: RuleNum EqSetList.Set, defaults: RuleNum EqSetList.Set}

   (* Note that EXCON subdivides using a list, not a map. This is because
      we have to test the exceptions in strict top-to-bottom order. The list
      is held in reverse order (last rule to first) since it's easier to
      build it that way. *)

    datatype AndOrTree = TUPLE of {nodeInfo: NodeInfo,
				   children: (lab, TypeInfo * AndOrTree) map
				  }
                       | CON of {nodeInfo: NodeInfo,
				 children: (longid, TypeInfo * AndOrTree) map
				}
		       | SCON of {nodeInfo: NodeInfo,
				  children: (scon, AndOrTree) map
				 }
		       | EXCON of {nodeInfo: NodeInfo,
				   children: (longid * (TypeInfo * AndOrTree)) list
				  }
		       | LEAF of NodeInfo


    val buildAndOrTree: pat list -> AndOrTree
					(* pat list != nil. *)

    type StringTree
    val layoutAndOrTree: AndOrTree -> StringTree
  end;
