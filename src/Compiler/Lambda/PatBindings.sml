(*$PatBindings : LAB IDENT DEC_GRAMMAR COMPILER_ENV TYPE_INFO
        ELAB_INFO LVARS PRETTYPRINT FINMAP CRASH PAT_BINDINGS*)

functor PatBindings (structure Lab : LAB
		     structure Ident : IDENT
		     structure Grammar : DEC_GRAMMAR
		     sharing type Grammar.lab = Lab.lab
		     sharing type Grammar.id = Ident.id
		     sharing type Grammar.longid = Ident.longid
		     structure CompilerEnv : COMPILER_ENV
		     sharing type CompilerEnv.id = Ident.id
		     structure ElabInfo : ELAB_INFO
		     sharing type Grammar.info = ElabInfo.ElabInfo
                    structure Lvars: LVARS
                      sharing type CompilerEnv.lvar = Lvars.lvar

                    structure PP: PRETTYPRINT
                      sharing type CompilerEnv.StringTree = PP.StringTree
                                   = Grammar.StringTree

                    structure FinMap: FINMAP
                      sharing type FinMap.StringTree = PP.StringTree

                    structure Crash: CRASH
                   ): PAT_BINDINGS =
  struct
    structure TypeInfo = ElabInfo.TypeInfo
    type TypeInfo = TypeInfo.TypeInfo
    open Grammar
    structure CE = CompilerEnv 
    type CEnv = CE.CEnv

    type lvar = Lvars.lvar
    type RuleNum = int
    type (''a, 'b) map = (''a, 'b) FinMap.map
    infix plus
    val (op plus) = CompilerEnv.plus

    datatype BindingTree =
        TUPLEbtree of (lab, (TypeInfo * lvar * BindingTree)) map
      | CONbtree   of {info: TypeInfo, child: BindingTree, childLvar: lvar}
      | EXCONbtree of {info: TypeInfo, child: BindingTree, childLvar: lvar}
      | NILbtree

   (*A couple of utility functions not needed elsewhere.*)

    fun zip3(hd :: tl, hd' :: tl', hd'' :: tl'') =
          (hd, hd', hd'') :: zip3(tl, tl', tl'')
      | zip3(nil, nil, nil) = nil
      | zip3 _ = Crash.impossible "zip3"

    local
      fun unzip3'((x, y, z) :: rest, xs, ys, zs) =
            unzip3'(rest, x :: xs, y :: ys, z :: zs)
        | unzip3'(nil, xs, ys, zs) = (xs, ys, zs)
    in
      fun unzip3 triples = unzip3'(rev triples, nil, nil, nil)
    end

    exception GetArity
    fun get_arity i =
      case ElabInfo.to_TypeInfo i of
        Some (TypeInfo.VAR_PAT_INFO {tyvars,...}) => List.size tyvars
      | _ => raise GetArity (*Crash.impossible "PatBindings.get_arity"*)

    fun patBindings(root, pat): (BindingTree * CE.CEnv) =
      case pat
        of ATPATpat(_, atpat) =>
             atpatBindings(root, atpat)

        | CONSpat(i, OP_OPT(longid,_), atpat) =>
	     (case ElabInfo.to_TypeInfo i
		of Some (info as TypeInfo.CON_INFO _) =>
		  let
		    val childLv = Lvars.newLvar()
		    val (bt, env) = atpatBindings(childLv, atpat)
		  in
		    (CONbtree{info=info, child=bt, childLvar=childLv}, env)
		  end
		 | Some (info as TypeInfo.EXCON_INFO _) => 
		  let
		    val childLv = Lvars.newLvar()
		    val (bt, env) = atpatBindings(childLv, atpat)
		  in
		    (EXCONbtree{info=info,child=bt, childLvar=childLv}, env)
		  end
		 | _ => Crash.impossible "patBindings(CONSpat..)")
            
        | TYPEDpat(_, pat, _) =>
            patBindings(root, pat)

        | LAYEREDpat(i, OP_OPT(id, _), _, pat) =>
           (let
              val bind = CE.declareVar(id, root, CE.emptyCEnv)
              val (bt, env) = patBindings(root, pat)
            in
              (bt, env plus bind)
            end handle GetArity =>
                Crash.impossible ("Cannot find arity (PatBindings.patBindings)\n\
                   \pattern: " ^ PP.flatten1(Grammar.layoutPat pat))
           )
                

        | UNRES_INFIXpat _ =>
            Crash.impossible "patBindings(UNRES_INFIX)"

    and atpatBindings(root, atpat): (BindingTree * CE.CEnv) =
      case atpat
        of WILDCARDatpat _ =>
             (NILbtree, CE.emptyCEnv)

         | SCONatpat _ =>
             (NILbtree, CE.emptyCEnv)

         | LONGIDatpat(i, OP_OPT(longid, _)) =>
             (NILbtree, 
	      case ElabInfo.to_TypeInfo i
		of Some (TypeInfo.VAR_PAT_INFO _) =>
		  (case Ident.decompose longid
		     of (nil, id) => (CE.declareVar(id, (*Lvars.rename (Ident.pr_id id)*) root, 
						    CE.emptyCEnv) 
				      handle GetArity =>
                                        Crash.impossible ("Cannot find arity (PatBindings.atpatBindings)\n\
                                        \pattern: " ^ PP.flatten1(Grammar.layoutAtpat atpat))
				     )
		      | _ => Crash.impossible "atpatBindings"
		   )
		 | _ => CE.emptyCEnv) 

         | RECORDatpat(_, patrowOpt) =>
             let
               val (labs, infosXlvarsXpats) =
                 let
                   fun f None = nil
                     | f (Some(PATROW(i, lab, pat, rest))) =
                                        (* We must always have TypeInfo here
                                           for labels... *)
                           let
                             val ti =
                               case ElabInfo.to_TypeInfo i
                                 of Some ti => ti
                                  | None =>
                                      Crash.impossible
                                        "PatBindings.atpatBindings(ti)"

                               (* invent lvar, re-using program name, if possible *)
                               (* rarely has such a little change resulted in so big an improvement: 30/3/97, mads:*)
                               val lvar = case Grammar.find_topmost_id_in_pat pat of
                                            Some string => Lvars.new_named_lvar string
                                          | None => Lvars.newLvar()
                           in
                             (lab, (ti, lvar, pat)) :: f rest
                           end
                     | f (Some(DOTDOTDOT _)) = nil
                 in
                   ListPair.unzip(f patrowOpt)
                 end

               val (infos, lvars, pats) = unzip3 infosXlvarsXpats

               val (trees, envs) =
                 ListPair.unzip(map patBindings (ListPair.zip(lvars, pats)))

               val L: (lab * (TypeInfo * lvar * BindingTree)) list =
                 ListPair.zip(labs, zip3(infos, lvars, trees))

               val map =
                 List.foldL
                   (fn (lab: (*eqtype*) Lab.lab, infoXlvarXbtree) =>
                      fn map => FinMap.add(lab, infoXlvarXbtree, map)
                   ) FinMap.empty L

               val totalEnv = List.foldL (General.curry op plus) CE.emptyCEnv envs
             in
               (TUPLEbtree map, totalEnv)
             end

         | PARatpat(_, pat) =>
             patBindings(root, pat)


    type StringTree = PP.StringTree

    fun layoutPatBindings(tree: BindingTree, env: CE.CEnv) =
      let
        fun layoutBTree tree =
          case tree
            of TUPLEbtree map =>
                 FinMap.layoutMap
                   {start="TUPLEbtree{", eq=" -> ", sep="; ", finish="}"}
                   (PP.layoutAtom Lab.pr_Lab)
                   (fn (_, lv, t) =>
                      PP.NODE{start="(" ^ Lvars.pr_lvar lv ^ ", ",
                              finish=")", indent=3, childsep=PP.NONE,
                              children=[layoutBTree t]
                             }
                   )
                   map

             | CONbtree{info,child, childLvar} =>
                 PP.NODE{start="CONbtree(" ^ Lvars.pr_lvar childLvar ^ ": ",
                         finish=")", indent=3, childsep=PP.NONE,
                         children=[layoutBTree child]
                        }

             | EXCONbtree{info,child, childLvar} =>
                 PP.NODE{start="EXCONbtree(" ^ Lvars.pr_lvar childLvar ^ ": ",
                         finish=")", indent=3, childsep=PP.NONE,
                         children=[layoutBTree child]
                        }

             | NILbtree => PP.LEAF "NILbtree"
      in
        PP.NODE{start="Pat Bindings: ", finish="",
                indent=3, childsep=PP.RIGHT "; ",
                children=[layoutBTree tree, CE.layoutCEnv env]
               }
      end
  end;
