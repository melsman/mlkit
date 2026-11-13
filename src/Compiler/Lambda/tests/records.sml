structure RecordTests =
struct
  open LambdaExp
  fun assert name b = if b then () else raise Fail name
  fun rejected name f =
      assert name ((f (); false) handle ex => if List.exists (fn n => General.exnName ex = n) ["CRASH","AbortExp"] then true else raise ex)
  val intTy = int31Type
  fun int n = INTEGER(IntInf.fromInt n,intTy)
  fun var x = VAR {lvar=x,instances=[],regvars=[]}
  fun record rp es = RECORD {regvar=NONE,repvar=rp,elements=es}
  fun recordTy rp fs = RECORDtype {regvar=NONE,repvar=rp,fields=fs}
  fun pgm e = PGM(DATBINDS [],e)
  val frame = FRAME {declared_lvars=[],declared_excons=[]}
  fun check e = ignore (LambdaStatSem.type_check
      {env=LambdaStatSem.initial,pgm=pgm e,letrec_polymorphism_only=false})
  fun bind t e k =
      let val x = Lvars.newLvar()
      in LET {pat=[(x,[],t)],bind=e,scope=k (var x)}
      end
  fun discard t e = bind t e (fn _ => frame)
  val normal = RecordField.ordinary
  val mutable = RecordField.mutable
  val raw = RecordField.raw
  val r = RepVar.fresh 2
  val s = RepVar.fresh 2
  val other = RepVar.fresh 3
  val () = assert "span conflict" ((RepVar.unify(r,other); false)
                                      handle RepVar.Span _ => true)
  val () = assert "failed union is atomic" (not (RepVar.same(r,other)))
  val () = RepVar.unify(r,s)
  val () = assert "union" (RepVar.same(r,s))
  val copy = RepVar.copier()
  val () = assert "copy sharing" (RepVar.same(copy r,copy s))
  val () = assert "copy independence" (not (RepVar.same(copy r,r)))
  val pu = Pickle.pairGen(RepVar.pu,RepVar.pu)
  val bytes = Pickle.pickle pu (r,s)
  val (r1,s1) = Pickle.unpickle pu bytes
  val (r2,_) = Pickle.unpickle pu bytes
  val () = assert "pickle sharing" (RepVar.same(r1,s1))
  val () = assert "pickle independence" (not (RepVar.same(r1,r2)))
  val () = RepVar.unify(r1,RepVar.top)
  val () = assert "top propagation" (RepVar.isTop s1)
  val () = assert "top does not constrain arity" (RepVar.span s1 = NONE)
  val ty = recordTy (SOME r) [(intTy,normal),(intTy,mutable)]
  val value = record (SOME s) [(int 1,normal),(int 2,mutable)]
  val recordPU = Pickle.pairGen(pu_Type,pu_LambdaExp)
  val (savedTy,savedValue) = Pickle.unpickle recordPU (Pickle.pickle recordPU (ty,value))
  val () = check (discard savedTy savedValue)
  val () = assert "type and allocation retain one serialized class"
      (case (savedTy,savedValue) of
           (RECORDtype {repvar=SOME a,...},RECORD {repvar=SOME b,...}) =>
             RepVar.same(a,b) andalso not (RepVar.same(a,r))
         | _ => false)
  fun read i x = PRIM(READprim {index=i,repvar=SOME r},[x])
  fun select i x = PRIM(SELECTprim {index=i,repvar=SOME r},[x])
  fun store i x e = PRIM(STOREprim {index=i,repvar=SOME r},[x,e])
  val () = check (bind ty value (fn x =>
      bind intTy (select 0 x) (fn _ =>
      bind intTy (read 1 x) (fn _ => discard unitType (store 1 x (int 3))))))
  val () = rejected "selection of mutable field" (fn () =>
      check (bind ty value (fn x => discard intTy (select 1 x))))
  val () = rejected "read of immutable field" (fn () =>
      check (bind ty value (fn x => discard intTy (read 0 x))))
  val () = rejected "store to immutable field" (fn () =>
      check (bind ty value (fn x => discard unitType (store 0 x (int 3)))))
  val () = rejected "wrong stored type" (fn () =>
      check (bind ty value (fn x => discard unitType (store 1 x (F64 "2.0")))))
  val () = rejected "wrong access class" (fn () =>
      check (bind ty value (fn x => discard intTy
        (PRIM(READprim {index=1,repvar=SOME (RepVar.fresh 2)},[x])))))
  val () = rejected "wrong span" (fn () =>
      check (discard ty (record (SOME other) [(int 1,normal),(int 2,mutable)])))
  fun checkFields fs es = check (discard (recordTy NONE fs) (record NONE es))
  val () = checkFields [(f64Type,raw)] [(F64 "1.5",raw)]
  val () = checkFields [(f64Type,raw),(intTy,mutable)] [(F64 "1.5",raw),(int 2,mutable)]
  val () = rejected "raw suffix" (fn () =>
      checkFields [(intTy,normal),(f64Type,raw)] [(int 2,normal),(F64 "1.5",raw)])
  val () = rejected "boxed raw field" (fn () => checkFields [(intTy,raw)] [(int 2,raw)])
  val alpha = fresh_tyvar()
  val poly = recordTy (SOME r) [(TYVARtype {tv=alpha},normal),(intTy,mutable)]
  val subst = LambdaBasics.mk_subst (fn () => "record test") ([alpha],[intTy])
  val () = assert "substitution retains representation" (case LambdaBasics.on_Type subst poly of
      RECORDtype {repvar=SOME r',...} => RepVar.same(r,r') | _ => false)

  (* Both direct selection and explicit-record elimination must also work
     after representation annotations have been assigned. *)
  fun optimise e = #1 (OptLambda.optimise (OptLambda.initial,pgm e))
  fun records (PGM(_,e)) = LambdaBasics.foldTD
      (fn n => fn RECORD _ => n+1 | _ => n) 0 e
  fun exported t e =
      let val x = Lvars.newLvar()
      in LET {pat=[(x,[],t)],bind=e,
              scope=FRAME {declared_lvars=[{lvar=x,tyvars=[],Type=t}],declared_excons=[]}}
      end
  val immutable = record (SOME r) [(int 10,normal),(int 20,normal)]
  val () = assert "direct selection contracts" (records (optimise
      (exported intTy (select 0 immutable))) = 0)
  val () = assert "bound record is eliminated" (records (optimise
      (bind (recordTy (SOME r) [(intTy,normal),(intTy,normal)]) immutable
        (fn x => exported intTy (select 1 x)))) = 0)
  val () = assert "mutable allocation survives" (records (optimise (exported ty value)) = 1)
  (* Exceed the ordinary inline-size limit. The sole call must expose the
     identity callback, allowing all its applications to contract. *)
  val callbackTy = ARROWtype([intTy],NONE,[intTy],NONE)
  val helperTy = ARROWtype([callbackTy],NONE,[intTy],NONE)
  val helper = Lvars.newLvar()
  val callback = Lvars.newLvar()
  val argument = Lvars.newLvar()
  fun calls 0 = int 7
    | calls n = APP(var callback,calls (n-1),NONE)
  val singleUse = LET {pat=[(helper,[],helperTy)],
      bind=FN {pat=[(callback,callbackTy)],body=calls 250},
      scope=exported intTy (APP(var helper,
          FN {pat=[(argument,intTy)],body=var argument},NONE))}
  val () = check singleUse
  val PGM(_,singleUseResult) = optimise singleUse
  val () = assert "large single-use function is inlined"
      (LambdaBasics.foldTD (fn n => fn APP _ => n+1 | _ => n) 0 singleUseResult = 0)
  val () = print "record IR tests passed\n"
end
