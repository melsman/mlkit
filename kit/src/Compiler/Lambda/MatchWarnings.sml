(*$MatchWarnings:
	IDENT SCON COMPILER_ENV DECTREE_DT FINMAP MATCH_WARNINGS
 *)

(* MatchWarnings just takes a signature for the DecisionTree datatype, since
   that's all it needs. *)

functor MatchWarnings(structure Ident: IDENT
		      structure SCon: SCON	(* ditto. *)
		      structure CompilerEnv: COMPILER_ENV

		      structure DecTreeDT: DECTREE_DT
			sharing type DecTreeDT.id = Ident.id
			    and type DecTreeDT.longid = Ident.longid
			    and type DecTreeDT.scon = SCon.scon
			    and type DecTreeDT.CEnv = CompilerEnv.CEnv

		      structure FinMap: FINMAP
			sharing type DecTreeDT.map = FinMap.map
		     ): MATCH_WARNINGS =
  struct
    open DecTreeDT
    val union = EqSet.union

   (* exhaustive: a tree is exhaustive if it doesn't contain any FAIL
      nodes, since the match compiler wouldn't have bothered putting in
      FAIL nodes for any complete sets of constructors. We pull our
      depth-first search exception trick again here... *)

    local
      exception FoundFail

      fun traverse tree: unit =		(* return () or raise FoundFail *)
	case tree
	  of LAB_DECOMPOSE{child, ...} => traverse child
	   | CON_DECOMPOSE{child, ...} => traverse child
	   | EXCON_DECOMPOSE{child, ...} => traverse child

	   | CON_SWITCH{
	       selections: ((*eqtype*) Ident.longid, (TypeInfo * DecisionTree)) FinMap.map,
	       wildcard,
	       ...
	     } =>
	       (case wildcard of None => () | Some w => traverse w;
		FinMap.fold (fn ((_, t), _) => traverse t) () selections
	       )

	   | SCON_SWITCH{
	       selections: ((*eqtype*) SCon.scon, DecisionTree) FinMap.map,
	       wildcard,
	       ...
	     } =>
	       (traverse wildcard;
		FinMap.fold (fn (t, _) => traverse t) () selections
	       )

	   | EXCON_SWITCH{selections, wildcard, ...} =>
	       (traverse wildcard;
		map (fn (_, t) => traverse t) selections;
		()
	       )

	   | END _ => ()
	   | FAIL => raise FoundFail
    in
      fun exhaustive t = (traverse t; true)
			 handle FoundFail => false
    end

   (* binds: does this decision tree bind any variables? *)
    local
      exception Yes

      fun traverse tree: unit =		(* return () or raise FoundFail *)
	case tree
	  of LAB_DECOMPOSE{child, ...} => traverse child
	   | CON_DECOMPOSE{child, ...} => traverse child
	   | EXCON_DECOMPOSE{child, ...} => traverse child

	   | CON_SWITCH{
	       selections: ((*eqtype*) Ident.longid, (TypeInfo * DecisionTree))
			     FinMap.map,
	       wildcard,
	       ...
	     } =>
	       (case wildcard of None => () | Some w => traverse w;
		FinMap.fold (fn ((_, t), _) => traverse t) () selections
	       )

	   | SCON_SWITCH{
	       selections: ((*eqtype*) SCon.scon, DecisionTree) FinMap.map,
	       wildcard,
	       ...
	     } =>
	       (traverse wildcard;
		FinMap.fold (fn (t, _) => traverse t) () selections
	       )

	   | EXCON_SWITCH{selections, wildcard, ...} =>
	       (traverse wildcard;
		map (fn (_, t) => traverse t) selections;
		()
	       )

	   | END{environment, ...} =>
	       (case List.size(CompilerEnv.lvarsOfCEnv environment)
		  of 0 => ()
		   | _ => raise Yes
	       )

	   | FAIL => ()
    in
      fun binds t = (traverse t; false)
		    handle Yes => true
    end

   (* reachable: just dig out all the rule numbers in the END nodes of
      the decision tree. *)

    fun reachable t =
      case t
	of LAB_DECOMPOSE{child, ...} => reachable child
	 | CON_DECOMPOSE{child, ...} => reachable child
	 | EXCON_DECOMPOSE{child, ...} => reachable child

	 | CON_SWITCH{
	     selections: ((*eqtype*) Ident.longid, (TypeInfo * DecisionTree))
			   FinMap.map,
	     wildcard,
	     ...
	   } =>
	     union (FinMap.Fold (fn ((_, (_, t)), L) => union (reachable t) L)
		    	        EqSet.empty selections
		   ) (case wildcard
			of Some w => reachable w
			 | None => EqSet.empty
		     )

	 | SCON_SWITCH{
	     selections: ((*eqtype*) SCon.scon, DecisionTree) FinMap.map,
	     wildcard,
	     ...
	   } =>
	     union (FinMap.Fold (fn ((_, t), L) => union (reachable t) L)
		    	        EqSet.empty selections
		   ) (reachable wildcard)

	 | EXCON_SWITCH{selections, wildcard, ...} =>
	     union(List.foldL (fn (_, t) => fn L => union (reachable t) L)
		   	      EqSet.empty selections
		  ) (reachable wildcard)

         | END{ruleNum, ...} => EqSet.singleton ruleNum
	 | FAIL => EqSet.empty
  end;
