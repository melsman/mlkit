(*$DecisionTree : LAB IDENT SCON DEC_GRAMMAR COMPILER_ENV
	DECISION_LIST PAT_BINDINGS LVARS TYPE_INFO FINMAP
	BASIC_IO REPORT FLAGS PRETTYPRINT CRASH DECISION_TREE*)

functor DecisionTree(structure Lab: LAB
		     structure Ident: IDENT
		     structure SCon: SCON
		     structure Grammar: DEC_GRAMMAR
		     structure CompilerEnv: COMPILER_ENV

		     structure DecisionList: DECISION_LIST
		       sharing type DecisionList.lab = Lab.lab
		           and type DecisionList.id = Ident.id
			   and type DecisionList.scon = SCon.scon
			   and type DecisionList.longid = Ident.longid

		     structure PatBindings: PAT_BINDINGS
		       sharing type PatBindings.lab = Lab.lab
			   and type PatBindings.pat = Grammar.pat
			   and type PatBindings.CEnv = CompilerEnv.CEnv

		     structure Lvars: LVARS
		       sharing type PatBindings.lvar = Lvars.lvar

		     structure TypeInfo: TYPE_INFO
		       sharing type PatBindings.TypeInfo
			            = DecisionList.TypeInfo
				    = TypeInfo.TypeInfo

		     structure FinMap: FINMAP
		       sharing type PatBindings.map = DecisionList.map
			            = FinMap.map

		     structure BasicIO: BASIC_IO
		     structure Report: REPORT
		     structure Flags: FLAGS

		     structure PP: PRETTYPRINT
		       sharing type PatBindings.StringTree
				    = CompilerEnv.StringTree
			 	    = FinMap.StringTree
				    = PP.StringTree
			   and type PP.Report = Report.Report

		     structure Crash: CRASH
		    ) : DECISION_TREE =
  struct
    val pr = Report.print o PP.reportStringTree

    open DecisionList Lvars

    type id = Ident.id
    type longid = Ident.longid
    type pat = Grammar.pat
    type Path = (Lab.lab * TypeInfo) list
    type CEnv = CompilerEnv.CEnv

    datatype DecisionTree =
        LAB_DECOMPOSE of {bind: lvar,
			  parent: lvar,
			  lab: lab,
			  child: DecisionTree,
			  info: TypeInfo
			 }

      | CON_DECOMPOSE of {bind: lvar, parent: lvar, child: DecisionTree,
			  info: TypeInfo}
      | EXCON_DECOMPOSE of {bind: lvar, parent: lvar, child: DecisionTree,
			  info: TypeInfo}

      | CON_SWITCH of {arg: lvar,
		       selections: (id, (TypeInfo * DecisionTree))
		       		      FinMap.map,
		       wildcard: DecisionTree Option
				(* An `option' because we may notice that all
				   the constructors are present. *)
		      }

      | SCON_SWITCH of {arg: lvar,
			selections: (scon, DecisionTree) FinMap.map,
			wildcard: DecisionTree
		       }

      | EXCON_SWITCH of {arg: lvar,
			 selections: (longid * DecisionTree) list,
			 wildcard: DecisionTree
			}

      | END of {ruleNum: RuleNum, environment: CompilerEnv.CEnv}
		(* Once we reach the END, we discard (with warning message?) all
		   but the first rule, and build an environment from identifiers
		   to lvars. Currently, we wastefully generate a whole new set
		   of lvars from the And/Or tree. *)

      | FAIL	(* Failing pattern-match. These nodes are used to determine
		   whether a match is exhaustive. We plant a FAIL if one can
		   get to a decision tree node with no live rules left -
		   this is essentially our inexhaustiveness condition. *)

  (* Printing (layout) routines: I need them here because I use them
     internally. *)

    type StringTree = PP.StringTree

    fun finmapMap f map =
      FinMap.Fold (fn ((d, r), strs) => f (d, r) :: strs) nil map

    val printRules = List.stringSep "[" "]" ", " Int.string

   (* layoutDecisionTree pulls the same sort of trick as layoutLambdaExp:
      any sequence of decomposition bindings is printed at a single level
      as a sequence of children, rather than being nested, which would cause
      too much indentation. *)

    fun layoutDecisionTree tree: StringTree =
      let
	fun ll() =
	  PP.NODE{start="let ", finish="", indent=3,
		  children=layoutDecompositions tree, childsep=PP.RIGHT ": "
		 }
      in
	case tree
	  of LAB_DECOMPOSE _ => ll()
	   | CON_DECOMPOSE _ => ll()
	   | EXCON_DECOMPOSE _ => ll()

	   | CON_SWITCH{arg, selections, wildcard} =>
	       let
		 val start = implode ["switch ", pr_lvar arg, ": ("]

		 val selections' =
		   FinMap.layoutMap {start="", eq=" -> ", sep="; ", finish=""}
				    (PP.layoutAtom Ident.pr_id)
				    (fn (_, t) => layoutDecisionTree t)
				    selections

		 val wildcard' =
		   case wildcard
		     of None => PP.LEAF "(none)"
		      | Some w => layoutDecisionTree w
	       in
		 PP.NODE{start=start, finish=")", indent=3,
			 children=[selections', wildcard'],
			 childsep=PP.LEFT "; wildcard = "
			}
	       end

	   | SCON_SWITCH{arg, selections, wildcard} =>
	       let
		 val start = implode ["switch ", pr_lvar arg, ": ("]

		 val selections' =
		   FinMap.layoutMap {start="", eq=" -> ", sep="; ", finish=""}
				    (PP.layoutAtom SCon.pr_scon)
				    layoutDecisionTree
				    selections

		 val wildcard' = layoutDecisionTree wildcard
	       in
		 PP.NODE{start=start, finish=")", indent=3,
			 children=[selections', wildcard'],
			 childsep=PP.LEFT "; wildcard = "
			}
	       end

	   | EXCON_SWITCH{arg, selections, wildcard} =>
	       let
		 val start = implode ["switch ", pr_lvar arg, ": ("]

		 fun layout(excon, dectree) =
		   PP.NODE{start="", finish="", indent=0,
			   childsep=PP.LEFT " -> ",
			   children=[PP.LEAF(Ident.pr_longid excon),
				     layoutDecisionTree dectree
				    ]
			  }

		 val selections' =
		   PP.NODE{start="", finish="", indent=0,
			   childsep=PP.RIGHT "; ",
			   children=map layout selections
			  }
	       in
		 PP.NODE{start=start, finish=")", indent=3,
			 children=[selections', layoutDecisionTree wildcard],
			 childsep=PP.LEFT "; wildcard = "
			}
	       end

	   | END{ruleNum, environment} =>
	       PP.NODE{start="END[R=" ^ Int.string ruleNum ^ "]",
		       finish="", indent=3,
		       children=[CompilerEnv.layoutCEnv environment],
		       childsep=PP.NONE
		      }

	   | FAIL => PP.LEAF "FAIL"
      end

    and layoutDecompositions tree: StringTree list =
      case tree
	of LAB_DECOMPOSE{bind, parent, lab, child, info=_} =>
	     PP.LEAF(implode [pr_lvar bind,
			      " = ", Lvars.pr_lvar parent,
			      ".#", Lab.pr_Lab lab
			     ]
		    ) :: layoutDecompositions child

	 | CON_DECOMPOSE{bind, parent, child,info} =>
	     PP.LEAF(implode [pr_lvar bind,
			      " = decon(", pr_lvar parent, ")"
			     ]
		    ) :: layoutDecompositions child

	 | EXCON_DECOMPOSE{bind, parent, child,info} =>
	     PP.LEAF(implode [pr_lvar bind,
			      " = deexcon(", pr_lvar parent, ")"
			     ]
		    ) :: layoutDecompositions child

	 | _ => [layoutDecisionTree tree]


   (* exhaustive-check. Given some TypeInfo and a map from constructors, this
      check tells us whether all the constructors are present or not. There
      are TypeInfo fields on each of the map elements, but they all have the
      same numCons field; we therefore look at an arbitrary one. *)

    fun exhaustive(info, map) =
      case (*nj_sml_bug*) info
	of TypeInfo.CON_INFO{numCons, ...} =>
	     (EqSet.size(FinMap.dom map) = numCons)
	 | _ =>
	     Crash.impossible "DecisionTree.exhaustive(TypeInfo)"

    fun prPath [(lab, _)] = "#" ^ Lab.pr_Lab lab
      | prPath ((lab, _) :: rest) = "#" ^ Lab.pr_Lab lab ^ "/" ^ prPath rest
      | prPath nil = "---"

    fun N_to_M(n, m) = if n > m then nil else n :: N_to_M(n+1, m)

    val sort: RuleNum list -> RuleNum list = ListSort.sort(General.curry op <)

   (* Convenient union and intersection notation. *)

    infix /\ \/

    val op \/ = General.uncurry EqSet.union
    and op /\ = General.uncurry EqSet.intersect

    (* decOrdered: we must order decisions so that the ones which have actual
       constructor occurrences come first. The choice between two such
       decisions is a heuristic one. We punt here. *)

    local
      fun firstOfSet'(x, set) =
	if EqSet.isEmpty set then x
	else Int.min x (firstOfSet'(EqSet.select set))
    in
      fun firstOfSet set =
	firstOfSet'(EqSet.select set)
	handle EqSet.Empty _ => Crash.impossible "DecisionTree.firstOfSet"

      fun firstRule(SUB_DECISION{rules, ...}) = firstOfSet rules
    end

    fun firstFromList(s :: rest) =
          List.foldL Int.min (firstRule s) (map firstRule rest)
      | firstFromList nil =
	  Crash.impossible "DecisionTree.firstFromList(nil)"

    fun firstFromMap m = firstFromList(FinMap.range m)

    fun firstConstructorRule(DECISION{select, ...}) =
      case select
	of CON_SELECT(map: ((*eqtype*) Ident.id, TypeInfo * SubDecision)
		      	     FinMap.map
		     )
	     => firstFromMap(FinMap.composemap #2 map)

	 | SCON_SELECT(map: ((*eqtype*) SCon.scon, SubDecision) FinMap.map)
	     => firstFromMap map

	 | EXCON_SELECT list => firstFromList(map (#2 o #2) list)

    val firstConstructorRule =
      if !Flags.DEBUG_DECISIONTREE then
	fn d as DECISION{path, ...} =>
	  let val result = firstConstructorRule d
	  in
	    BasicIO.println("first " ^ prPath path ^ " = " ^ Int.string result);
	    result
	  end
      else
	firstConstructorRule

    fun decOrdered(dec1, dec2) =
      firstConstructorRule dec1 <= firstConstructorRule dec2

    val sortDecs =
      ListSort.sort(fn (_, d1) => fn (_, d2) => decOrdered(d1, d2))

   (* buildPath: takes an lvar and a list of labels (with associated type info
      so we can enumerate them), and creates intermediate
      bindings by taking apart the root according to the labels. The result
      contains the final created lvar, plus a function which maps the inner
      decision tree to the outer one. This is because we may not have the
      inner tree at the time we build the path; what if the inner tree needs
      the final lvar? *)

    fun buildPath(root: lvar, path: Path)
          : (lvar * (DecisionTree -> DecisionTree)) =
      let
	fun build(lvar, nil, f) = (lvar, f)
	  | build(lvar, (lab, info) :: rest, f) =
	      let
		val new = newLvar()
	      in
		build(new, rest,
		      fn t => f(LAB_DECOMPOSE{bind=new, parent=lvar,
					      lab=lab, child=t, info=info
					     }
			       )
		     )
	      end
      in
					(* Don't forget, "path" is backwards. *)
	build(root, path, fn t => t)
      end

   (* conDecompose pulls the same kind of trick. *)

    fun conDecompose (info:TypeInfo) (root: lvar) : 
              (lvar * (DecisionTree -> DecisionTree)) =
      let
	val new = newLvar()
      in
	if !Flags.DEBUG_DECISIONTREE then
	  BasicIO.println("conDecompose")
	else ();
	(new, fn t => CON_DECOMPOSE{bind=new, parent=root, child=t,info=info})
      end

   (* exconDecompose pulls the same kind of trick. *)

    fun exconDecompose (info:TypeInfo) (root: lvar): 
               (lvar * (DecisionTree -> DecisionTree)) =
      let
	val new = newLvar()
      in
	if !Flags.DEBUG_DECISIONTREE then
	  BasicIO.println("exconDecompose")
	else ();
	(new, fn t => EXCON_DECOMPOSE{bind=new, parent=root, child=t,info=info})
      end

    fun pairWith x = map (fn y => (x, y))

    fun demap m = FinMap.Fold (op ::) nil m

   (* This decomposition stuff is a wee bit hairy. It's essentially generating
      lvar bindings for all the identifiers for a particular rule in the
      And/Or tree. It calls PatBindings.patBindings to generate a simple
      BindingTree for the pattern, as well as an environment saying
      where all the identifiers are bound. The BindingTree we get back
      still has TUPLE nodes with a child for each label, and we have to
      flatten these out into a nesting of LAB_DECOMPOSE nodes with all
      the tuple children nested. The `pending' parameter to `decompose' is
      a kind of continuation which is hanging on to the tuple children we
      haven't dealt with yet. When `decompose' runs out of things to do, it
      picks another tuple child off this list (fn `continue'). *)

    local
      open PatBindings

      fun decompose(parent: lvar, btree, rule, env,
		    pending: {lab: lab, info: TypeInfo,
			      parentLv: lvar, childLv: lvar,
			      btree: BindingTree
			     } list
		   ): DecisionTree =
	case btree
	  of TUPLEbtree(m: ((*eqtype*) Lab.lab,
			    (TypeInfo * lvar * BindingTree)
			   ) map
		       ) =>
	       let
		 fun pendingItem(lab, (info, lvar, btree)) =
		   {lab=lab, info=info,
		    parentLv=parent, childLv=lvar, btree=btree
		   }
	       in
		 continue(pending @ map pendingItem (demap m), rule, env)
	       end
			
	   | CONbtree{child, childLvar,info} =>
	       CON_DECOMPOSE{
		 bind=childLvar,
		 parent=parent,
	         child=decompose(childLvar, child, rule, env, pending),
		 info=info
	       }

	   | EXCONbtree{child, childLvar,info} =>
	       EXCON_DECOMPOSE{
		 bind=childLvar,
		 parent=parent,
		 child=decompose(childLvar, child, rule, env, pending),
		 info=info
	       }

	   | NILbtree =>	(* Any more bits of tree pending? *)
	       continue(pending, rule, env)

      and continue(pendingList, rule, env) =
	case pendingList
	  of {lab, info, parentLv, childLv, btree} :: rest =>
	       LAB_DECOMPOSE{bind=childLv, parent=parentLv, lab=lab,
			     child=decompose(childLv, btree, rule, env, rest),
			     info=info
			    }
	   | nil =>
               END{ruleNum=rule, environment=env}
				(* FINALLY, nothing else to decompose. *)

    in
      fun bindIdentifiers(root, pats, rule): DecisionTree =
	let
	  val pat = List.nth (rule-1) pats
	  val (btree, env) = patBindings(root, pat)

	  val _ =
	    if !Flags.DEBUG_DECISIONTREE then
	      pr(layoutPatBindings(btree, env))
	    else ()

	  val dt = decompose(root, btree, rule, env, nil)
	in
	  if !Flags.DEBUG_DECISIONTREE then
	    pr(PP.NODE{start="Binding identifiers(root="
			     ^ Lvars.pr_lvar root ^ "):",
		       finish="",
		       indent=3,
		       childsep=PP.NONE,
		       children=[layoutDecisionTree dt]
		      }
	      )
	  else ();
	  dt
	end
    end

   (* decisionTree': takes a list of pairs of lvars and decisions; from
      the appropriate lvar, we generate a series of bindings to run down
      the path in the decision, and then perform the selection. *)

    fun decisionTree'(numRules: int,
		      sortedList: (lvar * Decision) list,
		      liveRules: RuleNum EqSet.Set,
		      root: lvar,
		      pats: pat list
		     ): DecisionTree =
      case sortedList
	of nil =>	(* No more decisions: plant an END. But, we must go a
			   little further first, and generate bindings for all
			   the identifiers for this pattern. *)
	     (if EqSet.isEmpty liveRules
	      then FAIL
	      else bindIdentifiers(root, pats, firstOfSet liveRules)
	     )

	 | (here, (dec as DECISION{path, select, defaults})) :: decs =>
	     let
			(* includeIt: only hang on to those constructors which
			   have rules which are live. `includeIt' determines
			   which branches of this selection actually make it
			   into the decision tree, and there are ramifications
			   regarding wildcards and so on. *)

	       fun includeIt(SUB_DECISION{rules, ...}) =
		 not(EqSet.isEmpty(rules /\ liveRules))

	       val includeIt =
		 if !Flags.DEBUG_DECISIONTREE then
		   fn (subdec as SUB_DECISION{rules, ...}) =>
		     (BasicIO.print(
		        implode ["Include? dec. rules=",
				 List.stringSep
				   "[" "]" ", " Int.string
				   (EqSet.list rules),
				 ", live rules=",
				 List.stringSep
				   "[" "]" ", " Int.string
				   (EqSet.list liveRules),
				 "); "
				]
		      );
		      case includeIt subdec
			of true => (BasicIO.println "Yes"; true)
		         | false => (BasicIO.println "No"; false)
		     )
		 else
		   includeIt

	       val _ =
		 if !Flags.DEBUG_DECISIONTREE then
		   BasicIO.println(
		     implode ["DT'(",
			      pr_lvar root,
			      ", path=", prPath path,
			      "):\n  live=",
			      List.stringSep
			        "[" "]" ", " Int.string (EqSet.list liveRules),
			      ", #defs=",
			      List.stringSep
			        "[" "]" ", " Int.string (EqSet.list defaults)
			     ]
		   )
		 else ()

			(* Build a set of decomposition bindings
			   to run down this decision path. *)
	       val (unlabLvar, unlabF) = buildPath(here, path)

			(* A function from a SubDecision to its decision tree.
			   Here, we also do the DECON operation to strip
			   away the constructor, but only if there are
			   sub-decisions here which will need it; otherwise,
			   it would get us into trouble (e.g. decon(NIL)). *)
	       fun subtree(SUB_DECISION{rules, decisions}) decompose =
		 let
			(* Live rules for the next stage of the decision
			   tree: *)
		   val subLiveRules = liveRules /\ (rules \/ defaults)
		 in
		   case decisions
		     of nil =>
		       (if !Flags.DEBUG_DECISIONTREE then 
			  BasicIO.println("subtree --- decisions = nil")  
			else ();
			  decisionTree'(numRules, decs,
					subLiveRules, root, pats))

		      | _ =>
		       (if !Flags.DEBUG_DECISIONTREE then 
			  BasicIO.println("subtree --- decisions <> nil")  
			else ();

			  (* Decompose through the constructor: *)
			 let
			   val (deconLvar, deconF) = decompose(unlabLvar)
			 in
			   deconF(
			     decisionTree'(
			       numRules,
			       sortDecs(
				 decs
				 @ pairWith deconLvar decisions
			       ),
			       subLiveRules, root, pats
			     )
			   )
			 end)
		 end

	     (* foldFn: we fold this function over the constructor decisions.
	        It omits any constructor trees which are covered by the
		defaults list (see "includeIt").*)

	       fun foldFn((x, subdec), map) =
		 (if !Flags.DEBUG_DECISIONTREE then BasicIO.println("foldFN")  else ();
		  Crash.assert
		    ("DecisionTree.foldFn (used in case SCON_SELECT) --- how can there \
                     \be subdecisions for an scon?",
		       (not (includeIt subdec)) 
		     orelse 
		       ((fn SUB_DECISION{rules,decisions} =>
			 (case decisions of nil => true | _ => false))
			subdec));

		 if includeIt subdec then
		    (if !Flags.DEBUG_DECISIONTREE then 
		       BasicIO.println("foldFN --- includeIt subdec is true")  
		     else ();
		     FinMap.add(x, subtree subdec (fn t => Crash.impossible "foldFn"),
                                                  (* was: conDecompose -- Lars *)
				        
				map))
		 else
		    (if !Flags.DEBUG_DECISIONTREE then 
		       BasicIO.println("foldFN --- includeIt subdec is false")  
		     else ();
		       map))

	      (* foldFn' is the equivalent, but over lists rather than maps. *)

	       fun foldFn'((x, (i,subdec)), list) =
		 (if !Flags.DEBUG_DECISIONTREE then BasicIO.println("foldFN'")  else ();
		 if includeIt subdec then
		    (if !Flags.DEBUG_DECISIONTREE then 
		       BasicIO.println("foldFN' --- includeIt subdec is true")  
		     else ();
		     (x, subtree subdec (exconDecompose i)) :: list)
		 else
		   (if !Flags.DEBUG_DECISIONTREE then 
		      BasicIO.println("foldFN' --- includeIt subdec is false")
		    else ();
		   list))

	      (* foldFn2 is a foldFn which knows that the children have
	         type (TypeInfo * x) rather than just x. *)

	       fun foldFn2((x, (i, subdec)), map) =
		 if includeIt subdec then
		   FinMap.add(x, (i, subtree subdec (conDecompose i)), map)
		 else
		   map

		       (* if there is a wildcard, this is what it'll be. I've
			  lambda-abstracted it since I don't know what
			  decisionTree' will do if it's called in the case
			  of there being no wildcard (probably benign,
			  but this is clearer...) *)
	       val wildLiveRules = liveRules /\ defaults

	       fun theWildcard() =
		 decisionTree'(numRules, decs, wildLiveRules, root, pats)

	     in
	       case select
		 of CON_SELECT(
		      map: ((*eqtype*) Ident.id, TypeInfo * SubDecision)
			     FinMap.map
		    ) =>
			(* For a constructor-selection, we ask the TypeInfo
			   whether we have all the constructors here. If we
			   do, don't bother with the wildcard. This is
			   essentially a code-size optimisation, but it's
			   best to be neat and tidy. *)
		      let
			val selections = FinMap.Fold foldFn2 FinMap.empty map

			val wildcard =
			  case FinMap.range map	(* get the type info *)
			    of (info, _) :: _ =>
			         if exhaustive(info, selections)
				 then None else Some(theWildcard())

			     | _ =>	(* Selection map is empty, so we must
					   supply a wildcard. *)
				 Some(theWildcard())
		      in
			unlabF(CON_SWITCH{arg=unlabLvar,
					  selections=selections,
					  wildcard=wildcard
			  		 }
			      )
		      end

		  | SCON_SELECT(
		      map: ((*eqtype*) SCon.scon, SubDecision) FinMap.map
		    ) =>
		      unlabF(
			SCON_SWITCH{
			  arg=unlabLvar,
			  selections=FinMap.Fold foldFn FinMap.empty map,
			  wildcard=theWildcard()
			}
                      )

		  | EXCON_SELECT(
		      list: ((*eqtype*) Ident.longid * (TypeInfo * SubDecision)) list
		    ) =>
		      (if !Flags.DEBUG_DECISIONTREE then
			 BasicIO.println("EXCON_SELECT")
		       else ();
		      unlabF(
		        EXCON_SWITCH{
			  arg=unlabLvar,
			  selections=
			    List.foldR (General.curry foldFn') nil list,
			  wildcard=theWildcard()
			}
		      ))
	     end

    fun decisionTree{pats: pat list, root: lvar, decisions: Decision list}
          : DecisionTree =
      let
	val numRules = List.size pats
      in
	decisionTree'(numRules, sortDecs(pairWith root decisions),
		      EqSet.fromList(N_to_M(1, numRules)), root, pats
		     )
      end
  end;
