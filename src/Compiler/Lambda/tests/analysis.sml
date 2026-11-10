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
  val () = assert "flattening cannot move raw fields after pointer fields"
      (List.exists (fn {decision=Flatten.Rejected "flattening would break the raw-field prefix",...}:Flatten.candidate => true
                    | _ => false) (#candidates (analysis rawSuffix)))
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
          val p' = Flatten.transform c p
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
  val () = print "data flattening analysis tests passed\n"
end
