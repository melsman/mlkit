(*$DECISION_LIST: EqSetList*)

(* In a lot of cases, we are actually dealing with sets of decisions rather
   lists of them, but that gets unweildy. Equality over decisions gets awkward
   since the datatype is recursive and hence contains sets, and I don't know
   what it would do for the efficiency... Perhaps decision lists should
   actually be maps from labels or something. *)

signature DECISION_LIST =
  sig
    type lab
    type RuleNum sharing type RuleNum = int
    type id
    type scon
    type longid
    type TypeInfo
    type (''a, 'b) map

    datatype Decision =
      DECISION of {path: (lab * TypeInfo) list,
		   select: Select,
		   defaults: RuleNum EqSetList.Set
		  }

    and Select = CON_SELECT of (longid, TypeInfo * SubDecision) map
               | SCON_SELECT of (scon, SubDecision) map
               | EXCON_SELECT of (longid * (TypeInfo * SubDecision)) list
			(* a list (right way round this time) because the EXCONs
			   have to be tested in order. *)

    and SubDecision =
      SUB_DECISION of {rules: RuleNum EqSetList.Set, decisions: Decision list}

    type AndOrTree
    val decisions: AndOrTree -> Decision list

    val deterministic: (int list -> int list) -> Decision list -> int Option

    type StringTree
    val layoutDecision: Decision -> StringTree
  end;
