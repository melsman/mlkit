(*$DecisionList: LAB IDENT SCON AND_OR_TREE FINMAP PRETTYPRINT CRASH
	DECISION_LIST *)

(* DecisionList: takes an And/Or tree, builds the equivalence classes for
   the pattern matching, and does something else I haven't decided yet. *)

functor DecisionList(structure Lab: LAB
		     structure SCon: SCON
		     structure Ident: IDENT
		     structure AndOrTree: AND_OR_TREE
		       sharing type AndOrTree.lab = Lab.lab
			   and type AndOrTree.id = Ident.id
			   and type AndOrTree.scon = SCon.scon
			   and type AndOrTree.longid = Ident.longid

		     structure FinMap: FINMAP
		       sharing type FinMap.map = AndOrTree.map

		     structure PP: PRETTYPRINT
		       sharing type PP.StringTree = FinMap.StringTree
			            = AndOrTree.StringTree

		     structure Crash: CRASH
		    ): DECISION_LIST =
  struct
    structure EqSet = EqSetList
    open AndOrTree
   (* A decision node contains all the possible tests that can be
      carried out here (all the And/Or nodes immediately visible through
      tuple/record nodes). From each decision node is a set of constructors
      to further decision nodes. A decision tree is essentially an And/Or
      tree but with the tuples/records flattened into single nodes with
      multi-part paths (lists of labels). *)

    datatype Decision =
      DECISION of {path: (lab * TypeInfo) list,
		   select: Select,
		   defaults: RuleNum EqSet.Set
		  }

    and Select = CON_SELECT of (longid, TypeInfo * SubDecision) FinMap.map
               | SCON_SELECT of (scon, SubDecision) FinMap.map
               | EXCON_SELECT of (longid * (TypeInfo * SubDecision)) list

    and SubDecision =
      SUB_DECISION of {rules: RuleNum EqSet.Set, decisions: Decision list}

    fun rulesOf tree =
      let
	fun rulesOf'(NODEINFO{rules, defaults}) = EqSet.union rules defaults
      in
	case tree
	  of TUPLE{nodeInfo, ...} => rulesOf' nodeInfo
	   | CON{nodeInfo, ...} => rulesOf' nodeInfo
	   | SCON{nodeInfo, ...} => rulesOf' nodeInfo
	   | EXCON{nodeInfo, ...} => rulesOf' nodeInfo
	   | LEAF nodeInfo => rulesOf' nodeInfo
      end

   (* decisions: traverse an And/Or tree and build a list of decisions. In
      decisions', the path argument is in reverse order (for easy consing),
      and is reversed in the actual data structures. *)

    fun decisions(tree: AndOrTree): Decision list =
      decisions'(nil: (lab * TypeInfo) list, tree)

    and decisions'(path, tree) =
      case tree
	of TUPLE{children, ...} =>
	     FinMap.Fold
	       (fn ((lab: (*eqtype*) Lab.lab, (info, child)), decs) =>
	          decisions'((lab, info) :: path, child) @ decs
	       ) nil children

	 | CON{nodeInfo, children} =>
	     let
	       val NODEINFO{defaults, ...} = nodeInfo

	       fun subDecision(i, child) =
		 (i, SUB_DECISION{rules=rulesOf child,
				  decisions=decisions child
				 }
		 )
	     in
	       [DECISION{
		  path=rev path,
		  select=CON_SELECT(
		    FinMap.composemap subDecision
		      (children: ((*eqtype*) longid, (TypeInfo * AndOrTree))
				   FinMap.map
		      )
		  ),
		  defaults=defaults
	        }
	       ]
	     end

	 | SCON{nodeInfo, children} =>
	     let
	       val NODEINFO{defaults, ...} = nodeInfo

	       fun subDecision child =
		 SUB_DECISION{rules=rulesOf child,
			      decisions=decisions child
			     }
	     in
	       [DECISION{
		  path=rev path,
		  select=SCON_SELECT(
		    FinMap.composemap
		      subDecision
		      (children: ((*eqtype*) SCon.scon, AndOrTree) FinMap.map)
		  ),
		  defaults=defaults
	        }
	       ]
	     end

	 | EXCON{nodeInfo, children} =>
	     let
	       val NODEINFO{defaults, ...} = nodeInfo
		 
	       fun subDecision(excon, (i,child)) =
		 (excon,
		  (i,SUB_DECISION{rules=rulesOf child,
			       decisions=decisions child
			      }
		 ))
	     in
	       [DECISION{
		  path=rev path,
		  select=EXCON_SELECT(map subDecision children),
		  defaults=defaults
	        }
	       ]
	     end

	 | LEAF _ => nil


   (* printing/layout routines. *)

    val printRules: RuleNum EqSet.Set -> string =
      (List.stringSep "[" "]" ", " Int.string) o EqSet.list

    fun layoutSelect(CON_SELECT select): StringTree =
          FinMap.layoutMap
	    {start="CON_SELECT: ", eq=" -> ", sep="; ", finish=""}
	    (PP.layoutAtom Ident.pr_longid)
	    (fn (_, d) => layoutSubDecision d)
	    select

      | layoutSelect(SCON_SELECT select) =
          FinMap.layoutMap
	    {start="SCON_SELECT: ", eq=" -> ", sep="; ", finish=""}
	    (PP.layoutAtom SCon.pr_scon)
	    layoutSubDecision
	    select

      | layoutSelect(EXCON_SELECT select) =
	  let
	    fun layout(excon, (i,subdec)) =
	      PP.NODE{start="", finish="", indent=0, childsep=PP.LEFT " -> ",
		      children=[PP.LEAF(Ident.pr_longid excon),
				layoutSubDecision subdec
			       ]
		     }
	  in
	    PP.NODE{start="EXCON_SELECT: ", finish="", indent=0,
		    childsep=PP.RIGHT "; ",
		    children=map layout select
		   }
	  end

    and layoutSubDecision(SUB_DECISION{rules, decisions}) =
      PP.NODE{start=printRules rules ^ ": ", finish="", indent=3,
	      children=map layoutDecision decisions,
	      childsep=PP.RIGHT "; "
	     }

    and layoutDecision(DECISION{path, select, defaults}) =
      PP.NODE{start=printPath path,
	      finish="; " ^ printRules defaults,
	      indent=3, children=[layoutSelect select],
	      childsep=PP.RIGHT "; "
	     }

    and printPath nil = "<root> : "
      | printPath path =
	  List.stringSep "" "" "/"
	  		 (fn (L, _) => "#" ^ Lab.pr_Lab L)
			 path
	  ^ " : "


    fun rulesDecision(DECISION{select, defaults,...}) acc= 
             rulesSelect select (defaults :: acc)
    and rulesSelect(CON_SELECT m) acc =
             FinMap.Fold (fn ((id,(_,subdec)), acc) => rulesSubDecision subdec acc) acc m
      | rulesSelect(SCON_SELECT(m)) acc = 
             FinMap.Fold (fn ((scon,subdec), acc) => rulesSubDecision subdec acc) acc m
      | rulesSelect(EXCON_SELECT(l)) acc = 
             List.foldL (fn (longid,(_,subdec)) => rulesSubDecision subdec) acc l
    and rulesSubDecision(SUB_DECISION{rules, decisions}) acc =
             List.foldL rulesDecision (rules :: acc) decisions
             
    type rule = int

    fun deterministic (inter: rule list -> rule list) (l: Decision list): int Option = 
       let
           exception FALSE
           val minima =   
               List.foldL (fn rules => fn acc => 
                  case inter rules of [] => acc (*raise FALSE*) | r::_ => r::acc) [] 
               (List.foldL rulesDecision [] l)
           fun all_equal(x:: l) = if List.forAll (fn y:int => x=y) l then Some x else None
             | all_equal [] = None
       in
          all_equal(minima) 
       end handle FALSE => None

  end;
