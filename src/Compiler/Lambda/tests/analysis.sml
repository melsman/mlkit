structure AnalysisTests =
struct
  open LambdaExp
  fun assert label b = if b then () else raise Fail label
  val plain = RecordField.ordinary
  val mut = RecordField.mutable
  val intTy = int31Type
  fun int d = INTEGER(IntInf.fromInt d,intTy)
  fun var x = VAR {lvar=x,instances=[],regvars=[]}
  fun tuple es = recordExp(NONE,es)
  fun ty ts = recordType(ts,NONE)
  fun select i e = PRIM(SELECTprim {index=i,repvar=NONE},[e])
  fun read i e = PRIM(READprim {index=i,repvar=NONE},[e])
  fun store i e v = PRIM(STOREprim {index=i,repvar=NONE},[e,v])
  val frame = FRAME {declared_lvars=[],declared_excons=[]}
  fun bind t e k =
      let val x = Lvars.newLvar()
      in LET {pat=[(x,[],t)],bind=e,scope=k (var x)}
      end
  fun discard t e = bind t e (fn _ => frame)
  fun pgm e = PGM(DATBINDS [],e)
  fun analysis e = Flatten.analyse {env=LambdaStatSem.initial,pgm=pgm e,references=true,tuples=true}
  fun eligible cs = List.exists (fn {decision=Flatten.Eligible,...}:Flatten.candidate => true | _ => false) cs
  fun rejected cs = List.exists (fn {decision=Flatten.Rejected _,...}:Flatten.candidate => true | _ => false) cs
  val innerTy = ty [intTy]
  val outerTy = ty [innerTy,intTy]
  val allocation = tuple [tuple [int 1],int 2]
  val good = bind outerTy allocation (fn x =>
      bind outerTy x (fn alias => discard intTy (select 0 (select 0 alias))))
  val {pgm=prepared,candidates} = analysis good
  val () = assert "nested tuple through outer alias" (eligible candidates)
  val c = hd candidates
  val () = assert "removed selection is undefined" (Flatten.accessMap c (#outer c,0) = NONE)
  val () = assert "shifted outer selection" (Flatten.accessMap c (#outer c,1) = SOME 1)
  val () = assert "absorbed inner selection" (Flatten.accessMap c (valOf(#inner c),0) = SOME 0)
  val wideTy = ty [intTy,ty [intTy,intTy],intTy]
  val wide = bind wideTy (tuple [int 0,tuple [int 1,int 2],int 3])
      (fn x => discard intTy (select 1 (select 1 x)))
  val wideCandidate = hd (#candidates (analysis wide))
  val () = assert "wide tuple is eligible" (eligible [wideCandidate])
  val () = assert "access map shifts following fields"
      (Flatten.accessMap wideCandidate (#outer wideCandidate,2) = SOME 3)
  val () = assert "access map offsets absorbed fields"
      (Flatten.accessMap wideCandidate (valOf(#inner wideCandidate),1) = SOME 2)
  val rawInner = RECORDtype {fields=[(f64Type,RecordField.raw)],regvar=NONE,repvar=NONE}
  val rawAllocation = RECORD {elements=[(F64 "1.5",RecordField.raw)],regvar=NONE,repvar=NONE}
  val rawSuffix = bind (ty [intTy,rawInner]) (tuple [int 0,rawAllocation])
      (fn x => discard f64Type (select 0 (select 1 x)))
  val () = assert "raw fields can move to the record prefix"
      (eligible (#candidates (analysis rawSuffix)))
  fun convert name a b e = PRIM(CCALLprim {name=name,instances=[],regvars=[],tyvars=[],
      Type=ARROWtype([a],NONE,[b],NONE)},[e])
  fun box e = convert "__f64_to_real" f64Type realType e
  fun unbox e = convert "__real_to_f64" realType f64Type e
  val () = Flags.turn_on "data_flattening_floats"
  val floatTy = ty [intTy,realType,intTy]
  val floatAllocation = tuple [int 1,box(F64 "1.5"),int 2]
  val floatGood = bind floatTy floatAllocation (fn x =>
      bind intTy (select 0 x) (fn _ => bind intTy (select 2 x)
        (fn _ => discard f64Type (unbox(select 1 x)))))
  val {pgm=floatPrepared,candidates=floatCandidates} = analysis floatGood
  val fc = hd floatCandidates
  val () = assert "float boxing is eligible" (eligible floatCandidates)
  val () = assert "float prefix permutation" (#order fc = [1,0,2])
  val () = assert "preceding pointer index remapped" (Flatten.accessMap fc (#outer fc,0) = SOME 1)
  val floatResult = Flatten.transform {env=LambdaStatSem.initial,candidate=fc,pgm=floatPrepared}
  val _ = LambdaStatSem.type_check {env=LambdaStatSem.initial,pgm=floatResult,letrec_polymorphism_only=false}
  val () = assert "boxed float escape rejected" (rejected (#candidates (analysis
      (bind floatTy floatAllocation (fn x => discard realType (select 1 x))))))
  val floatMutTy = RECORDtype{fields=[(intTy,mut),(realType,mut)],regvar=NONE,repvar=NONE}
  val floatMutAllocation = RECORD{elements=[(int 1,mut),(box(F64 "1.5"),mut)],regvar=NONE,repvar=NONE}
  val floatMut = bind floatMutTy floatMutAllocation (fn x =>
      bind unitType (store 0 x (int 4)) (fn _ =>
        bind unitType (store 1 x (box(F64 "3.5"))) (fn _ => discard f64Type (unbox(read 1 x)))))
  val {pgm=mp,candidates=mc} = analysis floatMut
  val () = assert "mutable float eligible" (eligible mc)
  val _ = LambdaStatSem.type_check {env=LambdaStatSem.initial,
      pgm=Flatten.transform {env=LambdaStatSem.initial,candidate=hd mc,pgm=mp},letrec_polymorphism_only=false}
  val () = assert "opaque float store rejected" (rejected (#candidates (analysis
      (bind floatMutTy floatMutAllocation (fn x =>
        discard unitType (store 1 x (REAL("2.5",NONE))))))))
  (* Reordering must infer initializer types under the binding's own type
     abstraction, rather than reuse one concrete instantiation of the class. *)
  val fa = fresh_tyvar()
  val fat = TYVARtype{tv=fa}
  val ff = Lvars.newLvar()
  val fx = Lvars.newLvar()
  val fr = ty [fat,realType]
  val polyFloat = LET{pat=[(ff,[fa],ARROWtype([fat],NONE,[fr],NONE))],
      bind=FN{pat=[(fx,fat)],body=tuple[var fx,box(F64 "1.5")]},
      scope=bind (ty[intTy,realType])
        (APP(VAR{lvar=ff,instances=[intTy],regvars=[]},int 4,NONE))
        (fn x => discard f64Type (unbox(select 1 x)))}
  val {pgm=fp,candidates=fcs} = analysis polyFloat
  val () = assert "unrelated polymorphism survives float reordering" (eligible fcs)
  val _ = LambdaStatSem.type_check {env=LambdaStatSem.initial,
      pgm=Flatten.transform {env=LambdaStatSem.initial,candidate=hd fcs,pgm=fp},letrec_polymorphism_only=false}
  val () = Flags.turn_off "data_flattening_floats"
  val {candidates=disabled,...} = Flatten.analyse
      {env=LambdaStatSem.initial,pgm=pgm good,references=true,tuples=false}
  val () = assert "disabled is distinct from rejection"
      (List.all (fn {decision=Flatten.Disabled,...}:Flatten.candidate => true | _ => false) disabled)
  val () = assert "independent inner extraction" (rejected (#candidates (analysis
      (bind outerTy allocation (fn x => discard innerTy (select 0 x))))))
  val mutableInnerTy = RECORDtype {fields=[(intTy,mut)],regvar=NONE,repvar=NONE}
  val mutableOuterTy = ty [mutableInnerTy]
  val mutableAllocation = tuple [RECORD {elements=[(int 1,mut)],regvar=NONE,repvar=NONE}]
  val () = assert "read and store through outer tuple" (eligible (#candidates (analysis
      (bind mutableOuterTy mutableAllocation (fn x =>
        bind unitType (store 0 (select 0 x) (int 42)) (fn _ =>
          discard intTy (read 0 (select 0 x))))))))
  val refTy = CONStype([intTy],TyName.tyName_REF,NONE)
  val refOuterTy = ty [refTy,intTy]
  val refAllocation = tuple [PRIM(REFprim {instance=intTy,regvar=NONE},[int 1]),int 2]
  fun deref e = PRIM(DEREFprim {instance=refTy},[e])
  val refGood = bind refOuterTy refAllocation (fn x =>
      bind unitType (PRIM(ASSIGNprim {instance=ty [refTy,intTy]},[select 0 x,int 3]))
        (fn _ => discard intTy (deref(select 0 x))))
  val () = assert "reference allocation and accesses" (eligible (#candidates (analysis refGood)))
  val () = assert "reference escape rejected" (rejected (#candidates (analysis
      (bind refOuterTy refAllocation (fn x => discard refTy (select 0 x))))))

  (* Separate function annotations are connected by ordinary applications. *)
  val make = Lvars.newLvar()
  val inspect = Lvars.newLvar()
  val arg = Lvars.newLvar()
  val obj = Lvars.newLvar()
  val makeTy = ARROWtype([intTy],NONE,[outerTy],NONE)
  val inspectTy = ARROWtype([outerTy],NONE,[intTy],NONE)
  val coordinated = LET {pat=[(make,[],makeTy)],
      bind=FN {pat=[(arg,intTy)],body=tuple [tuple [var arg],int 2]},
      scope=LET {pat=[(inspect,[],inspectTy)],
        bind=FN {pat=[(obj,outerTy)],body=select 0 (select 0 (var obj))},
        scope=bind outerTy (APP(var make,int 1,NONE)) (fn x =>
          discard intTy (APP(var inspect,x,NONE)))}}
  val () = assert "coordinated function layouts" (eligible (#candidates (analysis coordinated)))
  (* Bind inspect around both allocations so the outer classes are unified. *)
  val incompatible = LET {pat=[(inspect,[],inspectTy)],
      bind=FN {pat=[(obj,outerTy)],body=select 0 (select 0 (var obj))},
      scope=LET {pat=[(make,[],makeTy)],bind=FN {pat=[(arg,intTy)],body=allocation},
        scope=bind outerTy (APP(var make,int 0,NONE)) (fn x =>
          bind intTy (APP(var inspect,x,NONE)) (fn _ =>
            bind innerTy (tuple [int 4]) (fn inner =>
              bind outerTy (tuple [inner,int 5]) (fn y =>
                discard intTy (APP(var inspect,y,NONE))))))}}
  val () = assert "all allocations must be nested" (rejected (#candidates (analysis incompatible)))

  val alpha = fresh_tyvar()
  val tv = TYVARtype {tv=alpha}
  val first = Lvars.newLvar()
  val pair = Lvars.newLvar()
  val polyTy = ty [tv,intTy]
  val poly = LET {pat=[(first,[alpha],ARROWtype([polyTy],NONE,[intTy],NONE))],
      bind=FN {pat=[(pair,polyTy)],body=select 1 (var pair)},
      scope=bind outerTy allocation (fn x => discard intTy
        (APP(VAR {lvar=first,instances=[innerTy],regvars=[]},x,NONE)))}
  val () = assert "opaque polymorphic field"
      (List.exists (fn {decision=Flatten.Rejected "opaque or incompatible field type",...}:Flatten.candidate => true
                    | _ => false) (#candidates (analysis poly)))

  fun imported (vs,t) =
      let val f = Lvars.newLvar()
          val x = Lvars.newLvar()
          val ft = ARROWtype([t],NONE,[t],NONE)
          val p = pgm (LET {pat=[(f,vs,ft)],bind=FN {pat=[(x,t)],body=var x},
                            scope=FRAME {declared_lvars=[{lvar=f,tyvars=vs,Type=ft}],declared_excons=[]}})
          val env = LambdaStatSem.type_check {env=LambdaStatSem.initial,pgm=p,letrec_polymorphism_only=false}
          val env = Pickle.unpickle LambdaStatSem.pu (Pickle.pickle LambdaStatSem.pu env)
      in (f,LambdaStatSem.plus(LambdaStatSem.initial,env))
      end
  val (identity,abstractEnv) = imported ([alpha],tv)
  val abstractCall = bind outerTy allocation (fn x =>
      bind outerTy (APP(VAR {lvar=identity,instances=[outerTy],regvars=[]},x,NONE))
        (fn y => discard intTy (select 0 (select 0 y))))
  val () = assert "abstract boundary parameter preserves local layout" (eligible (#candidates
      (Flatten.analyse {env=abstractEnv,pgm=pgm abstractCall,references=true,tuples=true})))
  val (concrete,concreteEnv) = imported ([],outerTy)
  val concreteCall = bind outerTy allocation (fn x =>
      bind outerTy (APP(var concrete,x,NONE)) (fn y => discard intTy (select 0 (select 0 y))))
  val () = assert "concrete boundary pins both layouts" (rejected (#candidates
      (Flatten.analyse {env=concreteEnv,pgm=pgm concreteCall,references=true,tuples=true})))
  val exported = bind outerTy allocation (fn VAR {lvar,...} =>
      FRAME {declared_lvars=[{lvar=lvar,tyvars=[],Type=outerTy}],declared_excons=[]}
    | _ => raise Fail "variable")
  val () = assert "exports pin layouts" (rejected (#candidates (analysis exported)))
  val () = assert "analysis does not annotate its input"
      (case allocation of RECORD {repvar=NONE,...} => true | _ => false)
  val savedClass = RepVar.fresh 1
  val savedType = RECORDtype {fields=[(intTy,plain)],regvar=NONE,repvar=SOME savedClass}
  val boundaryType = Pickle.unpickle pu_Type_top (Pickle.pickle pu_Type_top savedType)
  val () = assert "boundary decoding uses top"
      (case boundaryType of RECORDtype {repvar=SOME r,...} => RepVar.isTop r | _ => false)
  val () = assert "boundary decoding does not pin a shared inline template" (not(RepVar.isTop savedClass))

  val sharedTemplate = TYPED(
      RECORD {regvar=NONE,repvar=SOME savedClass,elements=[(int 0,plain)]},savedType,[])
  fun copyClass () = case LambdaBasics.new_instance sharedTemplate of
      TYPED(RECORD {repvar=SOME a,...},RECORDtype {repvar=SOME b,...},_) =>
        (assert "inline copy preserves internal sharing" (RepVar.same(a,b)); a)
    | _ => raise Fail "inline template"
  val copy1 = copyClass()
  val copy2 = copyClass()
  val () = assert "inline copies have independent layouts"
      (not(RepVar.same(copy1,copy2)) andalso not(RepVar.same(copy1,savedClass)))

  (* A loaded inline template gets a new layout graph when copied locally;
     its imported interface's top annotations must not leak into that graph. *)
  val factory = Lvars.newLvar()
  val parameter = Lvars.newLvar()
  val factoryTy = ARROWtype([intTy],NONE,[refOuterTy],NONE)
  val producer = pgm (LET {pat=[(factory,[],topType factoryTy)],
      bind=FN {pat=[(parameter,intTy)],body=
        RECORD {regvar=NONE,repvar=SOME RepVar.top,
          elements=[(PRIM(REFprim {instance=intTy,regvar=NONE},[var parameter]),plain),
                    (var parameter,plain)]}},
      scope=FRAME {declared_lvars=[{lvar=factory,tyvars=[],Type=topType factoryTy}],declared_excons=[]}})
  val (_,optimizerEnv) = OptLambda.optimise(OptLambda.initial,producer)
  val optimizerEnv = Pickle.unpickle OptLambda.pu (Pickle.pickle OptLambda.pu optimizerEnv)
  val localFactory = Lvars.newLvar()
  val consumer = pgm (LET {pat=[(localFactory,[],factoryTy)],bind=var factory,
      scope=FRAME {declared_lvars=[{lvar=localFactory,tyvars=[],Type=factoryTy}],declared_excons=[]}})
  val (PGM(db,expanded),_) = OptLambda.optimise(optimizerEnv,consumer)
  val localBody = LambdaBasics.passBU (fn FRAME _ => frame | e => e) expanded
  val () = assert "loaded inline body is analysed locally" (eligible (#candidates
      (Flatten.analyse {env=LambdaStatSem.initial,pgm=PGM(db,localBody),references=true,tuples=true})))
  (* Every accepted choice must remove an allocation and remain well typed.
     Exercise the fixed point separately, since one splice may expose another. *)
  fun allocations (PGM(_,e)) = LambdaBasics.foldTD
      (fn n => fn RECORD _ => n+1 | PRIM(REFprim _,_) => n+1 | _ => n) 0 e
  fun rewrite env e =
      let val {pgm=p,candidates} = Flatten.analyse
              {env=env,pgm=pgm e,references=true,tuples=true}
          val c = valOf(List.find (fn {decision,...}:Flatten.candidate =>
                                      decision=Flatten.Eligible) candidates)
          val p' = Flatten.transform {env=LambdaStatSem.initial,candidate=c,pgm=p}
          val _ = LambdaStatSem.type_check {env=env,pgm=p',letrec_polymorphism_only=false}
          val () = assert "flattening removes an allocation" (allocations p' < allocations p)
      in p'
      end
  val () = List.app (fn e => ignore(rewrite LambdaStatSem.initial e))
      [good,wide,refGood,coordinated,
       bind mutableOuterTy mutableAllocation (fn x =>
         bind unitType (store 0 (select 0 x) (int 42)) (fn _ =>
           discard intTy (read 0 (select 0 x))))]
  val _ = rewrite abstractEnv abstractCall
  val empty = bind (ty [ty [],intTy]) (tuple [tuple [],int 42])
      (fn x => discard intTy (select 1 x))
  val _ = rewrite LambdaStatSem.initial empty
  val deep = bind (ty [ty [ty [intTy]]]) (tuple [tuple [tuple [int 42]]])
      (fn x => discard intTy (select 0 (select 0 (select 0 x))))
  val () = assert "default pass flattens references"
      (allocations (Flatten.optimise {enabled=true,env=LambdaStatSem.initial} (pgm refGood))
       < allocations (pgm refGood))
  val deepResult = Flatten.optimise {enabled=true,env=LambdaStatSem.initial} (pgm deep)
  val () = assert "sequential flattening reaches a fixed point" (allocations deepResult=1)
  val () = assert "no eligible opportunity remains"
      (not(eligible (#candidates(Flatten.analyse
          {env=LambdaStatSem.initial,pgm=deepResult,references=true,tuples=true}))))
  (* Restricted imported templates may mention a hidden payload datatype.
     Do not expose that unavailable dependency through a fresh declaration. *)
  fun freshName () = TyName.freshTyName
      {tycon=TyName.tycon TyName.tyName_LIST,arity=0,arity_reml=(0,0),equality=false}
  val hiddenName = freshName ()
  val publicName = freshName ()
  val hiddenCon = Con.mk_con "Hidden"
  val publicCon = Con.mk_con "Box"
  val otherCon = Con.mk_con "Other"
  val importedType = CONStype([],publicName,NONE)
  val definitions = DATBINDS [[([],hiddenName,[(hiddenCon,NONE)])],
      [([],publicName,[(publicCon,SOME refOuterTy),
                      (otherCon,SOME(CONStype([],hiddenName,NONE)))])]]
  val imported = LambdaStatSem.type_check
      {env=LambdaStatSem.initial,pgm=PGM(definitions,frame),letrec_polymorphism_only=false}
  val imported = LambdaStatSem.plus(LambdaStatSem.initial,
      LambdaStatSem.restrict(imported,{lvars=[],excons=[],tynames=[publicName],cons=[publicCon,otherCon]}))
  val privateUse = pgm (bind importedType
      (PRIM(CONprim{con=publicCon,instances=[],regvar=NONE},
            [tuple [PRIM(REFprim{instance=intTy,regvar=NONE},[int 7]),int 0]]))
      (fn x => discard intTy (PRIM(DEREFprim{instance=refTy},
          [select 0 (PRIM(DECONprim{con=publicCon,instances=[],lv_opt=NONE},[x]))]))))
  val PGM(DATBINDS freshDefinitions,_) = Flatten.optimise {enabled=true,env=imported} privateUse
  val () = assert "hidden imported payload dependencies remain ordinary" (null freshDefinitions)
  (* A private wrapper and its nonrecursive payload must be specialised
     together, with the wrapper referring to the new payload datatype. *)
  val leafName = freshName ()
  val ownerName = freshName ()
  val leafCon = Con.mk_con "LeafPayload"
  val ownerCon = Con.mk_con "OwnerPayload"
  val leafType = CONStype([],leafName,NONE)
  val ownerType = CONStype([],ownerName,NONE)
  val nestedType = ty [ty [intTy,intTy],intTy]
  val dependencyDefs = DATBINDS [[([],leafName,[(leafCon,SOME nestedType)])],
                                [([],ownerName,[(ownerCon,SOME leafType)])]]
  fun construct c e = PRIM(CONprim{con=c,instances=[],regvar=NONE},[e])
  fun destruct c e = PRIM(DECONprim{con=c,instances=[],lv_opt=NONE},[e])
  val dependencyBody = bind ownerType
      (construct ownerCon (construct leafCon (tuple [tuple [int 4,int 7],int 0])))
      (fn x => discard intTy (select 1 (select 0 (destruct leafCon (destruct ownerCon x)))))
  val dependencyProgram = PGM(dependencyDefs,dependencyBody)
  val dependencyResult as PGM(DATBINDS dependencyGroups,_) =
      Flatten.optimise {enabled=true,env=LambdaStatSem.initial} dependencyProgram
  val () = assert "nonrecursive dependency allocation is flattened"
      (allocations dependencyResult = allocations dependencyProgram - 1)
  val declarations = List.concat dependencyGroups
  val flattenedLeaf = valOf(List.find (fn (_,n,cs) => not(TyName.eq(n,leafName)) andalso
      List.exists (fn (_,SOME(RECORDtype{fields,...})) => length fields = 3 | _ => false) cs) declarations)
  val () = assert "specialised owner refers to specialised dependency"
      (List.exists (fn (_,n,cs) => not(TyName.eq(n,ownerName)) andalso
          List.exists (fn (_,SOME(CONStype([],m,_))) => TyName.eq(m,#2 flattenedLeaf)
                       | _ => false) cs) declarations)
  val generationName = freshName ()
  val generationCon = Con.mk_con "GenerationPayload"
  val coordinateType = ty [intTy,intTy]
  val coordinatesType = CONStype([coordinateType],TyName.tyName_LIST,NONE)
  val generationType = CONStype([],generationName,NONE)
  val generationDefs = DATBINDS [[([],generationName,[(generationCon,SOME coordinatesType)])]]
  val nilCoordinates = PRIM(CONprim{con=Con.con_NIL,instances=[coordinateType],regvar=NONE},[])
  val coordinateList = PRIM(CONprim{con=Con.con_CONS,instances=[coordinateType],regvar=NONE},
      [tuple [tuple [int 4,int 7],nilCoordinates]])
  val generationBody = bind generationType (construct generationCon coordinateList)
      (fn x => discard intTy (select 1 (select 0
          (PRIM(DECONprim{con=Con.con_CONS,instances=[coordinateType],lv_opt=NONE},
                [destruct generationCon x])))))
  val generationProgram = PGM(generationDefs,generationBody)
  val generationResult as PGM(DATBINDS generationGroups,_) =
      Flatten.optimise {enabled=true,env=LambdaStatSem.initial} generationProgram
  val () = assert "fixed coordinate-list dependency flattens"
      (allocations generationResult = allocations generationProgram - 1)
  val () = assert "generation payload uses a specialised monomorphic list"
      (List.exists (fn (_,n,cs) => not(TyName.eq(n,generationName)) andalso
          List.exists (fn (_,SOME(CONStype([],m,_))) => not(TyName.eq(m,TyName.tyName_LIST))
                       | _ => false) cs) (List.concat generationGroups))
  (* Region inference needs dependencies before their users, with cycles
     contained in a single declaration group. Check payloads and type arguments. *)
  fun dependencyOrder (groups : (tyvar list * TyName * (con * Type option) list) list list) =
    let val declared = TyName.Set.fromList (map #2 (List.concat groups))
        fun walk _ [] = true
          | walk seen (group::rest) =
            let val available = TyName.Set.addList (map #2 group) seen
                fun field NONE = true
                  | field (SOME t) = foldType (fn ok => fn CONStype(_,n,_) =>
                        ok andalso (not(TyName.Set.member n declared) orelse
                                    TyName.Set.member n available)
                      | _ => ok) true t
            in List.all (fn (_,_,cs) => List.all (field o #2) cs) group
               andalso walk available rest
            end
    in walk TyName.Set.empty groups
    end
  val () = assert "specialised datatype groups precede their users"
      (dependencyOrder dependencyGroups andalso dependencyOrder generationGroups)
  (* Candidate refinement must preserve unrelated polymorphism and reject
     incompatible uses and exported schemes. *)
  val ra = fresh_tyvar()
  val rb = fresh_tyvar()
  val at = TYVARtype{tv=ra}
  val bt = TYVARtype{tv=rb}
  val rf = Lvars.newLvar()
  val rx = Lvars.newLvar()
  val rt = ARROWtype([ty [at,bt]],NONE,[at],NONE)
  fun rfUse a b x y = APP(VAR{lvar=rf,instances=[a,b],regvars=[]},tuple [x,y],NONE)
  fun rfProgram scope = pgm (LET{pat=[(rf,[ra,rb],rt)],
        bind=FN{pat=[(rx,ty [at,bt])],body=select 0 (var rx)},scope=scope})
  val partial = rfProgram (bind intTy (rfUse intTy intTy (int 1) (int 2))
        (fn _ => discard intTy (rfUse intTy stringType (int 3) (STRING("x",NONE)))))
  val SOME partialResult = FlattenRefine.refine [ra] partial
  val _ = LambdaStatSem.type_check{env=LambdaStatSem.initial,pgm=partialResult,letrec_polymorphism_only=false}
  val () = assert "unrelated quantified parameter survives refinement"
      (case partialResult of PGM(_,LET{pat=[(_,vs,_)],...}) => vs = [rb] | _ => false)
  val incompatible = rfProgram (bind intTy (rfUse intTy intTy (int 1) (int 2))
        (fn _ => discard stringType (rfUse stringType intTy (STRING("x",NONE)) (int 2))))
  val () = assert "incompatible concrete uses reject refinement"
      (not(Option.isSome(FlattenRefine.refine [ra] incompatible)))
  val exported = rfProgram (FRAME{declared_lvars=[{lvar=rf,tyvars=[ra,rb],Type=rt}],declared_excons=[]})
  val () = assert "exported polymorphism is fixed"
      (not(Option.isSome(FlattenRefine.refine [ra] exported)))
  val ma = fresh_tyvar()
  val mt = TYVARtype{tv=ma}
  val mf = Lvars.newLvar()
  val mg = Lvars.newLvar()
  val mx = Lvars.newLvar()
  val my = Lvars.newLvar()
  val mtype = ARROWtype([mt],NONE,[mt],NONE)
  fun mutualFun (f,x,g) = {lvar=f,tyvars=[ma],regvars=[],constrs=[],Type=mtype,
        bind=FN{pat=[(x,mt)],body=APP(var g,var x,NONE)}}
  val mutualProgram = pgm (FIX{functions=[mutualFun(mf,mx,mg),mutualFun(mg,my,mf)],
        scope=discard intTy (APP(VAR{lvar=mf,instances=[intTy],regvars=[]},int 1,NONE))})
  val SOME mutualResult = FlattenRefine.refine [ma] mutualProgram
  val _ = LambdaStatSem.type_check{env=LambdaStatSem.initial,pgm=mutualResult,letrec_polymorphism_only=false}
  val () = assert "recursive group shares refinement and monomorphic recursive calls"
      (case mutualResult of PGM(_,FIX{functions,...}) => List.all (null o #tyvars) functions | _ => false)
  val pa = fresh_tyvar()
  val pt = TYVARtype{tv=pa}
  val pf = Lvars.newLvar()
  val px = Lvars.newLvar()
  fun listTy t = CONStype([t],TyName.tyName_LIST,NONE)
  val pairTy = ty [pt,intTy]
  val packTy = ARROWtype([pt],NONE,[listTy pairTy],NONE)
  val packBody = PRIM(CONprim{con=Con.con_CONS,instances=[pairTy],regvar=NONE},
        [tuple [tuple [var px,int 1],PRIM(CONprim{con=Con.con_NIL,instances=[pairTy],regvar=NONE},[])]])
  val concretePair = ty [intTy,intTy]
  val packProgram = pgm (LET{pat=[(pf,[pa],packTy)],bind=FN{pat=[(px,pt)],body=packBody},
        scope=bind (listTy concretePair) (APP(VAR{lvar=pf,instances=[intTy],regvars=[]},int 7,NONE))
          (fn xs => discard intTy (select 0 (select 0
            (PRIM(DECONprim{con=Con.con_CONS,instances=[concretePair],lv_opt=NONE},[xs])))))})
  val () = Flags.turn_off "data_flattening_refine"
  val packBaseline = Flatten.optimise{enabled=true,env=LambdaStatSem.initial} packProgram
  val () = Flags.turn_on "data_flattening_refine"
  val packResult = Flatten.optimise{enabled=true,env=LambdaStatSem.initial} packProgram
  val () = assert "candidate-driven refinement enables an additional flattening"
      (allocations packResult < allocations packBaseline)
  (* Extracting the inner pair prevents the proposed flattening. Its polymorphic
     producer must remain unchanged when the trial is discarded. *)
  val blockedPack = pgm (LET{pat=[(pf,[pa],packTy)],bind=FN{pat=[(px,pt)],body=packBody},
        scope=bind (listTy concretePair) (APP(VAR{lvar=pf,instances=[intTy],regvars=[]},int 7,NONE))
          (fn xs => discard concretePair (select 0
            (PRIM(DECONprim{con=Con.con_CONS,instances=[concretePair],lv_opt=NONE},[xs]))))})
  val blockedResult = Flatten.optimise{enabled=true,env=LambdaStatSem.initial} blockedPack
  val () = assert "failed flattening trial retains polymorphism"
      (case blockedResult of PGM(_,LET{pat=[(_,vs,_)],...}) => vs = [pa] | _ => false)
  val () = List.app Flags.turn_off ["data_flattening_refs","data_flattening_tuples"]
  val () = Flags.turn_on "data_flattening"
  val _ = Flatten.optimise {enabled=false,env=LambdaStatSem.empty} (pgm good)
  val () = Flags.turn_off "optimiser"
  val () = assert "global optimiser disables analysis" (not(Flatten.active()))
  val () = assert "master request retains typed cache" (Flatten.requested())
  val () = Flags.turn_on "optimiser"
  val () = Flags.turn_off "data_flattening"
  (* Executable Barry output must handle singleton mutable records and nested
     assignment on the right-hand side, including SML infix precedence. *)
  val () = case CommandLine.arguments() of
      [file] =>
        let val unitRef = CONStype([unitType],TyName.tyName_REF,NONE)
            val objectTy = ty [unitRef]
            fun assign r value = PRIM(ASSIGNprim {instance=ty [unitRef,unitType]},[r,value])
            val e = bind objectTy (tuple [PRIM(REFprim {instance=unitType,regvar=NONE},[tuple []])])
                (fn x => discard unitType (assign (select 0 x) (assign (select 0 x) (tuple []))))
            val result = rewrite LambdaStatSem.initial e
            val out = TextIO.openOut file
            val () = PrettyPrint.outputTree ((fn s => TextIO.output(out,s)),LambdaExp.barify result,100)
            val () = TextIO.output(out,"\nval () = print \"mutable singleton barification passed\\n\"\n")
        in TextIO.closeOut out
        end
    | _ => ()
  (* Reports describe only the accepted program and never rename compiler
     identities or alter their layout metadata. *)
  val reportName = freshName()
  val reportEnum = freshName()
  val reportOriginal = freshName()
  val () = TyName.setBoxity(reportName,TyName.UNB_LOW)
  val () = TyName.setBoxity(reportEnum,TyName.ENUM)
  val reportClass = RepVar.fresh 5
  val reportClassBefore = RepVar.layout reportClass
  val retained = [([],reportOriginal,[(Con.mk_con "Unchanged",NONE)])]
  val reportOriginalPgm = PGM(DATBINDS[retained],frame)
  val reportFields = [(f64Type,RecordField.raw),
      (f64Type,{mutable=true,traversable=false}),
      (CONStype([],reportName,NONE),plain),
      (CONStype([],reportEnum,NONE),plain),
      (RECORDtype{fields=[(CONStype([],reportOriginal,NONE),plain)],
                  regvar=NONE,repvar=SOME RepVar.top},plain)]
  val reportPgm = PGM(DATBINDS[retained,
      [([],reportName,[(Con.mk_con "Cell",SOME(RECORDtype{fields=reportFields,
          regvar=NONE,repvar=SOME reportClass})),(Con.mk_con "End",NONE)]),
       ([],reportEnum,[(Con.mk_con "Flag",NONE)])]],frame)
  fun representationText p = case DatatypeRepr.layout{original=reportOriginalPgm,result=p} of
      NONE => "" | SOME tree => PrettyPrint.flatten1 tree
  val reportText = representationText reportPgm
  val () = assert "short distinct names and recursive references"
      (String.isSubstring "list#1 [lub; inherited]" reportText andalso
       String.isSubstring "list#2 [enum; inherited]" reportText andalso
       String.isSubstring "* list#1 * list#2" reportText)
  val () = assert "field modes and representation classes survive reporting"
      (String.isSubstring "raw f64" reportText andalso
       String.isSubstring "var raw f64" reportText andalso
       String.isSubstring ("^" ^ reportClassBefore) reportText andalso
       String.isSubstring "^top" reportText)
  val () = assert "unchanged declarations are not presented as specialisations"
      (not(String.isSubstring "Unchanged" reportText) andalso
       String.isSubstring "Referenced types (not specialised here)" reportText)
  val () = assert "no retained specialisation produces no report"
      (representationText reportOriginalPgm = "")
  val () = Flags.turn_on "print_type_name_stamps"
  val () = assert "report names do not depend on stamp-printing flags"
      (representationText reportPgm = reportText)
  val () = Flags.turn_off "print_type_name_stamps"
  val () = assert "reporting preserves boxities and classes"
      (TyName.boxity reportName = TyName.UNB_LOW andalso RepVar.layout reportClass = reportClassBefore)
  val () = assert "actual accepted datatype rewrite can be reported"
      (Option.isSome(DatatypeRepr.layout{original=dependencyProgram,result=dependencyResult}))
  val () = print "data flattening analysis tests passed\n"
end
