(*$AndOrTree : SCON IDENT LAB DEC_GRAMMAR ELAB_INFO FINMAP
      PRETTYPRINT CRASH AND_OR_TREE*)

functor AndOrTree(structure Ident: IDENT
		  structure SCon: SCON
		  structure Lab: LAB

		  structure Grammar: DEC_GRAMMAR
		    sharing type Grammar.id = Ident.id
		    sharing type Grammar.longid = Ident.longid
                        and type Grammar.scon = SCon.scon
			and type Grammar.lab = Lab.lab
		  structure ElabInfo : ELAB_INFO
		  sharing type Grammar.info = ElabInfo.ElabInfo
		  structure FinMap: FINMAP

		  structure PrettyPrint: PRETTYPRINT
		    sharing type FinMap.StringTree = ElabInfo.StringTree
				 = PrettyPrint.StringTree

		  structure Crash: CRASH
		 ): AND_OR_TREE =
  struct
    open Grammar

    structure EqSet = EqSetList

    fun to_TypeInfo i =
      case ElabInfo.to_TypeInfo i
	of Some ti => Some(ElabInfo.TypeInfo.normalise ti)
	 | None => None 

    type RuleNum = int
    type (''a, 'b) map = (''a, 'b) FinMap.map
    structure TypeInfo = ElabInfo.TypeInfo
    type TypeInfo = TypeInfo.TypeInfo

    datatype NodeInfo =
      NODEINFO of {rules: RuleNum EqSet.Set, defaults: RuleNum EqSet.Set}

   (* AndOrTree: a canonical coding of the structure of a sequence of
    match patterns. A TUPLE node corresponds to a tuple or record pattern;
    each child arc has a record label attached. A CON/SCON node
    corresponds to a constructor finite map; each child arc has a constructor
    attached. For EXCON it's a list (held in forward order), since we have
    to preserve the rule-order of the constructors to perform the tests from
    top to bottom. CON is for normal (datatype) constructors, SCON for
    constants, and EXCON for exception constructors.
      The NodeInfo at each node gives the set of live rules at this node
    (i.e. rules for which this node is viable), plus a list of defaults,
    which are the rules which will match this node through having an
    (unlayered) identifier or wildcard higher up. Each rule might contain
    a binding occurrence of an identifier. For example, the simple pattern
    `X' at top-level is a default for all of the tree, and the default
    rule at the root specifies a binding for `X'.
      Note that the live rule set and default rule set are kept distinct
    even though, strictly speaking, the default rules are also live. Consider
    the match

	(nil, _) => 1
	(_, nil) => 2
	(_ :: _, _ :: _) => 3

    It's important that we see that R1 has the highest constructor in the
    entire match. But, if we include defaults in the live rule set, we are
    unable to tell that we should initially switch on #1 (and not #2) because
    the #2 switch would keep R1 live.
      We create an initial tree for the first pattern in a match, and then
    augment it from the other patterns. *)

    datatype AndOrTree = TUPLE of {nodeInfo: NodeInfo,
				   children:
				     (lab, TypeInfo * AndOrTree) FinMap.map
				  }
                       | CON of {nodeInfo: NodeInfo,
				 children:
				   (longid, TypeInfo * AndOrTree) FinMap.map
				}
		       | SCON of {nodeInfo: NodeInfo,
				  children: (scon, AndOrTree) FinMap.map
				 }
		       | EXCON of {nodeInfo: NodeInfo,
				   children: (longid * (TypeInfo * AndOrTree)) list
				  }
		       | LEAF of NodeInfo

   (* one or two general utilities. *)
    fun explodePatrow(PATROW(i, lab, pat, patrow_opt)) =
	  (lab, (case to_TypeInfo i
		   of Some ti => ti
		    | None => Crash.impossible "explodePatrow"
		), pat
	  ) :: (case patrow_opt
		  of Some patrow' => explodePatrow patrow'
		   | None => nil
	       )

      | explodePatrow(DOTDOTDOT i) =  nil (* DOTDOTDOT was expanded by ElabDec.resolve_patrow *)

    fun getNodeInfo tree =
      case tree
	of TUPLE{nodeInfo, ...} => nodeInfo
	 | CON{nodeInfo, ...} => nodeInfo
	 | SCON{nodeInfo, ...} => nodeInfo
	 | EXCON{nodeInfo, ...} => nodeInfo
	 | LEAF nodeInfo => nodeInfo

    fun alterNodeInfo f tree =
      case tree
	of TUPLE{nodeInfo, children} =>
	     TUPLE{nodeInfo=f nodeInfo, children=children}

	 | CON{nodeInfo, children} =>
	     CON{nodeInfo=f nodeInfo, children=children}

	 | SCON{nodeInfo, children} =>
	     SCON{nodeInfo=f nodeInfo, children=children}

	 | EXCON{nodeInfo, children} =>
	     EXCON{nodeInfo=f nodeInfo, children=children}

	 | LEAF nodeInfo =>
	     LEAF(f nodeInfo)

    fun childMap (f: AndOrTree -> AndOrTree) tree =
      case tree
	of TUPLE{nodeInfo, children} =>
	     TUPLE{nodeInfo=nodeInfo,
		   children=FinMap.composemap (fn (ti, x) => (ti, f x)) children
		  }

	 | CON{nodeInfo, children} =>
	     CON{nodeInfo=nodeInfo,
		 children=
		   FinMap.composemap
		     (fn (ti, x) => (ti, f x))
		     (children: ((*eqtype*) longid,
				 TypeInfo * AndOrTree
				) FinMap.map
		     )
		}

	 | SCON{nodeInfo, children} =>
	     SCON{nodeInfo=nodeInfo,
		  children=FinMap.composemap f children
		 }

	 | EXCON{nodeInfo, children} =>
	     EXCON{nodeInfo=nodeInfo,
		   children=map (fn (e, (ti,c)) => (e, (ti,f c))) children
		  }

	 | LEAF _ => tree

   (* addRule: add a rule to nodeInfo. *)

    fun addRule(rulenum, NODEINFO{rules, defaults}) =
      NODEINFO{rules=EqSet.insert rulenum rules, defaults=defaults}

   (* usualNodeInfo: usual node info for an internal node. *)

    fun usualNodeInfo rulenum =
      NODEINFO{rules=EqSet.singleton rulenum, defaults=EqSet.empty}

   (* nullLeaf: And/Or leaf for nullary constructor/constant. *)

    fun nullLeaf rulenum: AndOrTree = LEAF(usualNodeInfo rulenum)

   (* defaultLeaf: And/Or leaf for identifier or `_' or `()' or `{}' *)

    fun defaultLeaf rulenum: AndOrTree =
      LEAF(NODEINFO{rules=EqSet.empty, defaults=EqSet.singleton rulenum})

    fun initialAndOrTree(rulenum, pat): AndOrTree =
      case pat
	of ATPATpat(_, atpat) =>
	     treeFromAtomicPat(rulenum, atpat)

	 | CONSpat(i, OP_OPT(longid, _), atpat) =>
	     (case to_TypeInfo i
		of Some (TypeInfo.CON_INFO _) => 
		     initialConNode(rulenum,  longid,
				    case to_TypeInfo i
				      of Some ti => ti
				       | None => Crash.impossible "initialAndOrTree",
				    treeFromAtomicPat(rulenum, atpat)
				   )
		 | Some (TypeInfo.EXCON_INFO _) => 
		     initialExconNode(rulenum, longid,
				      case to_TypeInfo i of
					Some ti => ti 
				      | None => Crash.impossible "initialAndOrTree",
				      treeFromAtomicPat(rulenum, atpat)
				     )
		 | _ => Crash.impossible "initialAndOrTree.no type info")
 
	 | TYPEDpat(_, pat, _) =>
	     initialAndOrTree(rulenum, pat)	(* Ignore type constraint. *)

	 | LAYEREDpat(_, _, _, pat) =>
	     initialAndOrTree(rulenum, pat)

	 | UNRES_INFIXpat _ =>
	     Crash.impossible "initialAndOrTree(UNRES_INFIX)"

    and treeFromAtomicPat(rulenum, atpat): AndOrTree =
      case atpat
	of WILDCARDatpat _ =>
	     defaultLeaf rulenum

	 | SCONatpat(_, scon) =>
	     initialSconNode(rulenum, scon)

	 | LONGIDatpat(i, OP_OPT(longid, _)) =>
	     (case to_TypeInfo i
		of Some (TypeInfo.CON_INFO _) =>
		     initialConNode(rulenum,  longid,
				    case to_TypeInfo i
				      of Some ti => ti
				       | None => Crash.impossible "treeFromAtomicPat",
				    nullLeaf rulenum
				   )
		 | Some (TypeInfo.EXCON_INFO _) =>
		     initialExconNode(rulenum, longid, 
				      case to_TypeInfo i
					of Some ti => ti
					 | None => Crash.impossible "treeFromAtomicPat",
				      nullLeaf rulenum
				     )
		 | Some (TypeInfo.VAR_PAT_INFO _) => 
		     (case Ident.decompose longid
			of (nil, id) => defaultLeaf rulenum
			 | _ => Crash.impossible "treeFromAtomicPat(LONGID)"
		     )
		 | _ => Crash.impossible "treeFromAtomicPat.no type info")

	 | RECORDatpat(_, patrowOpt) =>
	     (case patrowOpt                   (****************************************)
		of None =>                     (* Special case: treat `{}' and `()' as *)
	                                       (* wildcard. Was: `nullLeaf rulenum'    *)
	             defaultLeaf rulenum       (* 23/10/96-Martin                      *)  
		                               (****************************************)
		 | Some row =>
		     initialTupleNode(rulenum, explodePatrow row)
	     )

	 | PARatpat(_, pat) =>
	     initialAndOrTree(rulenum, pat)	(* Discard `()'. *)

    and initialTupleNode(rulenum, row: (lab * TypeInfo * pat) list) =
      let
	val children =
	  List.foldL
	    (fn (lab: (*eqtype*) Lab.lab, typeInfo, pat) => fn map =>
	       FinMap.add(lab, (typeInfo, initialAndOrTree(rulenum, pat)), map)
	    )
	    FinMap.empty
	    row
      in
	TUPLE{nodeInfo=usualNodeInfo rulenum, children=children}
      end

    and initialConNode(rulenum, con, info, tree) =
      CON{nodeInfo=usualNodeInfo rulenum,
	  children=FinMap.singleton(con, (info, tree))
	 }

    and initialSconNode(rulenum, scon: (*eqtype*) SCon.scon) =
      SCON{nodeInfo=usualNodeInfo rulenum,
	   children=FinMap.singleton(scon, nullLeaf rulenum)
	  }

    and initialExconNode(rulenum, excon, info, tree) =
      EXCON{nodeInfo=usualNodeInfo rulenum, children=[(excon, (info,tree))]}


   (* mergePattern: given an And/Or tree, merge in more patterns from
      (presumably) the same match. This is essentially a double case
      analysis, since there are lots of invariants about the corresponding
      pattern and existing tree nodes. For example, it is only possible to
      replace a LEAF And/Or node, and not an inner node, although inner
      nodes can have their NodeInfo parts extended. *)

    fun mergePattern((rulenum, pat), tree): AndOrTree =
      case pat
	of ATPATpat(_, atpat) =>
	     mergeAtomicPattern(tree, (rulenum, atpat))

	 | CONSpat(i, OP_OPT(longid, _), atpat) =>
	     (case to_TypeInfo i
		of Some (TypeInfo.CON_INFO _) =>
		     addInConNode(rulenum,  longid,
				  case to_TypeInfo i
				    of Some ti => ti
				     | None => Crash.impossible "mergePattern",
				  Some atpat, tree
				 )
		 | Some (TypeInfo.EXCON_INFO _) =>
		     addInExconNode(rulenum, longid,
				    case to_TypeInfo i
				      of Some ti => ti
				       | None => Crash.impossible "mergePattern",
				    Some atpat, tree
				   )
		 | _ => Crash.impossible "mergePattern.no type info")  
 
	 | TYPEDpat(_, pat, _) =>
	     mergePattern((rulenum, pat), tree)	(* Discard `: ty'. *)

	 | LAYEREDpat(_, OP_OPT(id, _), _, pat) =>
				(* Add the id binding here, and keep merging. *)
	     mergePattern((rulenum, pat), tree)

	 | UNRES_INFIXpat _ =>
	     Crash.impossible "mergePattern(UNRES_INFIX)"

    and mergeAtomicPattern(tree, (rulenum, atpat)) =
      case atpat
	of WILDCARDatpat _ =>	(* add a "default" here, no ident binding. *)
	     alterNodeInfo (addDefault rulenum) tree

	 | SCONatpat(_, scon) =>
	     addInSconNode(rulenum, scon, tree)

	 | LONGIDatpat(i, OP_OPT(longid, _)) =>
	     (case to_TypeInfo i
		of Some (TypeInfo.CON_INFO _) => 	(* A nullary constructor here *)
		     addInConNode(rulenum,  longid,
				  case to_TypeInfo i
				    of Some ti => ti
				     | None =>
					 Crash.impossible "MergeAtomicPattern",
				  None, tree
				 )
		 | Some (TypeInfo.EXCON_INFO _) =>
		     addInExconNode(rulenum, longid, 
				  case to_TypeInfo i
				    of Some ti => ti
				     | None =>
					 Crash.impossible "MergeAtomicPattern",
				  None, tree
				 )
		 | Some (TypeInfo.VAR_PAT_INFO _) => 	(* Add a binding for this identifier
							 * to this node, make it a default. *)
		     (case Ident.decompose longid
			of (nil, id) => alterNodeInfo (addDefault rulenum) tree
			 | _ => Crash.impossible "mergeAtomicPattern(LONGID)"
		     )
		 | _ => Crash.impossible "mergeAtomicPattern. no type info")

	 | RECORDatpat(_, patrowOpt) =>
	     (case patrowOpt                                              (***********************)
		of None => alterNodeInfo (addDefault rulenum) tree        (* treat `{}' and `()' *)
                                                                          (* as wildcard.        *)
		 | Some row =>                                            (***********************)
		     List.foldL
		       (fn (lab, info, pat) => fn tree =>
			  addInTupleNode(rulenum, lab, info, pat, tree)
		       ) tree (explodePatrow row)
	     )

	 | PARatpat(_, pat) =>
	     mergePattern((rulenum, pat), tree)

   (* extend{Tuple,Con,...}: insert a child branch into a TUPLE or CON/... node,
      doing a recursive merge if the label/constructor is already present.
      The functions are almost identical, but ML's type system isn't
      quite polymorphic enough to let me write it just once. *)

    and extendTuple(rulenum, nodeInfo, lab: (*eqtype*) Lab.lab,
		    info, pat, children
		   ) =
      let
	val infoXtree_opt = FinMap.lookup children lab
	val newSubtree =
	  case infoXtree_opt
	    of Some(_, t) => mergePattern((rulenum, pat), t)
	     | None => initialAndOrTree(rulenum, pat)
      in
	TUPLE{nodeInfo=addRule(rulenum, nodeInfo),
	      children=FinMap.add(lab, (info, newSubtree), children)
	     }
      end
	

   (* extendCon: extend this CON or LEAF node so that it has a (perhaps new)
      branch for "con", to a new leaf (atpatOpt=None), or a new sub-tree
      (atpatOpt=Some p). *)

    and extendCon(rulenum, nodeInfo, con, info, atpatOpt, children) =
      let
	val infoXtree_opt = FinMap.lookup children con
	val newSubtree =
	  case (infoXtree_opt, atpatOpt)
	    of (Some(_, t), Some atpat) =>
	         mergeAtomicPattern(t, (rulenum, atpat))

	     | (Some(_, t), None) =>
		 alterNodeInfo (fn info => addRule(rulenum, info)) t
				(* If we have this constructor already, and it
				   takes no args, then no change needed, except
				   to incorporate the new rule number. *)

	     | (None, Some atpat) =>
		 treeFromAtomicPat(rulenum, atpat)

	     | (None, None) =>
		  LEAF(usualNodeInfo rulenum)
      in
	CON{nodeInfo=addRule(rulenum, nodeInfo),
	    children=FinMap.add(con, (info, newSubtree), children)
	   }
      end

    and extendScon(rulenum, nodeInfo, scon: (*eqtype*) SCon.scon, children) =
      let
	val newLeaf =
	  case FinMap.lookup children scon
	    of Some leaf =>
	         alterNodeInfo (fn info => addRule(rulenum, info)) leaf

	     | None =>
		 LEAF(usualNodeInfo rulenum)
      in
	SCON{nodeInfo=addRule(rulenum, nodeInfo),
	     children=FinMap.add(scon, newLeaf, children)
	    }
      end

   (* extendExcon used to be bogus (dude). We were merging identical
      exception constructors; this is wrong for things like

	fn E1 CON1 => ...
	 | E2 CON2 => ...
	 | E1 CON2 => ...

      since if E1==E2, then E1 CON2 should match rule #2 - we would have
      chosen #3. Now, we don't do any merging at all. The above could
      probably still be optimised in some way, but I don't see much point. *)

    and extendExcon(rulenum, nodeInfo, excon, info, atpatOpt, children) =
      let
	val newSubtree =
	  case atpatOpt
	    of Some atpat =>
		 treeFromAtomicPat(rulenum, atpat)

	     | None =>
		  LEAF(usualNodeInfo rulenum)
      in
	EXCON{nodeInfo=addRule(rulenum, nodeInfo),
	      children=children @ [(excon, (info,newSubtree))]
	     }
      end

   (* addInTupleNode: much like addIn{Con,Excon}Node (below), except that
      there's always an atpat at the end of the labelled branch. *)

    and addInTupleNode(rulenum, lab, info, pat, here): AndOrTree =
      case here
	of TUPLE{nodeInfo, children} =>
	     extendTuple(rulenum, nodeInfo, lab, info, pat, children)

	 | CON _ =>
	     Crash.impossible "addInTupleNode: TUPLE covering CON"

	 | SCON _ =>
	     Crash.impossible "addInTupleNode: TUPLE covering SCON"

	 | EXCON _ =>
	     Crash.impossible "addInTupleNode: TUPLE covering EXCON"

	 | LEAF nodeInfo =>	(* Turn this LEAF into a TUPLE. *)
	     TUPLE{nodeInfo=addRule(rulenum, nodeInfo),
		   children=FinMap.singleton(
			      lab, (info, initialAndOrTree(rulenum, pat))
			    )
		  }

   (* addInConNode: the atpatOpt is Some p for CON(p), None for just CON. *)

    and addInConNode(rulenum, con, info, atpatOpt, here): AndOrTree =
      case here
	of TUPLE _ =>		(* Whoops: adding a constructor at the same
				   point as a previous tuple node? *)
	     Crash.impossible "addInConNode: CON covering TUPLE"

	 | CON{nodeInfo, children} =>
	     extendCon(rulenum, nodeInfo, con, info, atpatOpt, children)

	 | SCON _ =>
	     Crash.impossible "addInConNode: CON covering SCON"

	 | EXCON _ =>
	     Crash.impossible "addInConNode: CON covering EXCON"

	 | LEAF nodeInfo =>	(* Turn this LEAF into a CON, with a branch
				   ending in another LEAF (atpatOpt = None)
				   or a new sub-tree (atpatOpt=Some p). *)
	     CON{nodeInfo=addRule(rulenum, nodeInfo),
		 children=
		   FinMap.singleton(
		     con, (info, case atpatOpt
				   of Some atpat =>
					treeFromAtomicPat(rulenum, atpat)
				    | None =>
					nullLeaf rulenum
			  )
		   )
		}

    and addInSconNode(rulenum, scon, here): AndOrTree =
      case here
	of TUPLE _ =>
	     Crash.impossible "addInSconNode: SCON covering TUPLE"

	 | CON _ =>
	     Crash.impossible "addInSconNode: SCON covering CON"

	 | SCON{nodeInfo, children} =>
	     extendScon(rulenum, nodeInfo, scon, children)

	 | EXCON _ =>
	     Crash.impossible "addInSconNode: SCON covering EXCON"

	 | LEAF nodeInfo =>	(* Turn this LEAF into a SCON, with a branch
				   ending in another LEAF. *)
	     SCON{nodeInfo=addRule(rulenum, nodeInfo),
		  children=FinMap.singleton(scon, nullLeaf rulenum)
		 }

    and addInExconNode(rulenum, excon, info,atpatOpt, here): AndOrTree =
      case here
	of TUPLE _ =>
	     Crash.impossible "addInExconNode: EXCON covering TUPLE"

	 | CON _ =>
	     Crash.impossible "addInExconNode: EXCON covering CON"

	 | SCON _ =>
	     Crash.impossible "addInExconNode: EXCON covering SCON"

	 | EXCON{nodeInfo, children} =>
	     extendExcon(rulenum, nodeInfo, excon, info,atpatOpt, children)

	 | LEAF nodeInfo =>
	     EXCON{nodeInfo=addRule(rulenum, nodeInfo),
		   children=[(excon,
			      (info, 
			       case atpatOpt
			       of Some ap => treeFromAtomicPat(rulenum, ap)
			         | None    => nullLeaf rulenum
			      )
			     )
			    ]
		  }

    and addDefault rulenum (NODEINFO{rules, defaults}): NodeInfo =
      NODEINFO{rules=rules, defaults=EqSet.insert rulenum defaults}

    fun addDefaults new_rules  (NODEINFO{rules, defaults}): NodeInfo =
      NODEINFO{rules=rules, defaults=EqSet.union new_rules defaults}

    fun mergePatterns((rulenum, p :: rest), tree) =
	  mergePatterns((rulenum+1, rest),
			mergePattern((rulenum, p), tree)
		       )
      | mergePatterns((_, nil), tree) = tree


   (* propagateDefaults: each default rule at a node should appear in all the
      children below it, so we do a propagation pass. *)

    fun defaultRules(NODEINFO{defaults, ...}) = defaults

    fun allDefaultRules(allRules, NODEINFO{defaults,rules}) = 
        NODEINFO{defaults=allRules,rules=rules}

    fun propagateDefaults(rules, tree): AndOrTree =
      let
	val nodeInfo = getNodeInfo tree
	val defRules = defaultRules nodeInfo
	val allRules = EqSet.union rules defRules
      in
	childMap
	  (fn child => propagateDefaults(allRules, child))
	  (alterNodeInfo (fn info => allDefaultRules(allRules, info)) tree)
      end


   (* buildAndOrTree, as required by signature. Build an initial tree for
      the first pattern in the list, and then merge in all the other
      patterns. Finally, propagate the default rule values down. *)

    fun buildAndOrTree nil = Crash.impossible "buildAndOrTree(nil)"
      | buildAndOrTree(first :: rest) =
	  propagateDefaults(
	    EqSet.empty,
	    mergePatterns((2, rest), initialAndOrTree(1, first))
	  )


   (* printing/layout of And/Or trees. *)

    type StringTree = PrettyPrint.StringTree

    fun layoutAndOrTree tree: StringTree =
      case tree
	of TUPLE{nodeInfo, children} =>
	     PrettyPrint.NODE {start="TUPLE ", finish="", indent=3,
			       children=[layoutNodeInfo nodeInfo,
					 layoutTupleChildren children],
			       childsep=PrettyPrint.RIGHT "; "}

         | CON{nodeInfo, children} =>
	     PrettyPrint.NODE {start="CON ", finish="", indent=3,
			       children=[layoutNodeInfo nodeInfo,
					 layoutConChildren children],
			       childsep=PrettyPrint.RIGHT "; "}

         | SCON{nodeInfo, children} =>
	     PrettyPrint.NODE {start="SCON ", finish="", indent=3,
			       children=[layoutNodeInfo nodeInfo,
					 layoutSConChildren children],
			       childsep=PrettyPrint.RIGHT "; "}

	 | EXCON{nodeInfo, children} =>
	     PrettyPrint.NODE {start="EXCON ", finish="", indent=3,
			       children=[layoutNodeInfo nodeInfo,
					 layoutExconChildren children],
			       childsep=PrettyPrint.RIGHT "; "}

	 | LEAF nodeInfo =>
	     PrettyPrint.NODE {start="LEAF ", finish="", indent=3,
			       children=[layoutNodeInfo nodeInfo],
			       childsep=PrettyPrint.NONE}

    and layoutTIxAndOrTree(typeInfo, andOrTree) =
          PrettyPrint.NODE {start="TypeInfo: ", finish="", indent=0,
			    children=[ElabInfo.TypeInfo.layout typeInfo,
				      layoutAndOrTree andOrTree],
			    childsep=PrettyPrint.RIGHT "; "}

    and layoutNodeInfo(NODEINFO{rules, defaults}) =
      PrettyPrint.NODE {start="rules: ",
			indent=3,
			children=[PrettyPrint.layoutAtom
				    (List.stringSep "[" "]" ", " Int.string)
				  (EqSet.list rules)],
			childsep=PrettyPrint.NONE,
			finish=" defaults="
			^ List.stringSep "[" "]" ", " Int.string
			(EqSet.list defaults)}

    and layoutTupleChildren children =
          FinMap.layoutMap {start="", eq="=", sep=", ", finish=""}
	    (PrettyPrint.layoutAtom Lab.pr_Lab)
	      layoutTIxAndOrTree children

    and layoutConChildren children =
          FinMap.layoutMap {start="", eq="=", sep=", ", finish=""}
	    (PrettyPrint.layoutAtom Ident.pr_longid)
	       layoutTIxAndOrTree children

    and layoutSConChildren children =
          FinMap.layoutMap {start="", eq="=", sep=", ", finish=""}
	    (PrettyPrint.layoutAtom SCon.pr_scon) layoutAndOrTree children

    and layoutExconChildren children =
          let
	    fun layout(excon, (i,t)) =
	          PrettyPrint.NODE {start="", finish="", indent=0, childsep=PrettyPrint.LEFT "=",
				    children=[PrettyPrint.LEAF(Ident.pr_longid excon),
					      layoutAndOrTree t]}
	  in
	    PrettyPrint.NODE {start="", finish="", indent=0, childsep=PrettyPrint.RIGHT ", ",
			      children=map layout children}
	  end
  end;
