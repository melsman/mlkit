(* Exercise the extended IR directly: source tuples still have ordinary fields. *)
structure RecordFixture =
struct
  open LambdaExp
  fun make js =
    let
      val intTy = intDefaultType()
      val raw = {mutable=true,traversable=false}
      val ptr = RecordField.mutable
      val plain = RecordField.ordinary
      val rp = SOME (RepVar.fresh 3)
      val ty = RECORDtype {fields=[(f64Type,raw),(stringType,ptr),(intTy,plain)],
                           regvar=NONE,repvar=rp}
      fun var x = VAR {lvar=x,instances=[],regvars=[]}
      fun bind t e k =
          let val x = Lvars.newLvar()
          in LET {pat=[(x,[],t)],bind=e,scope=k (var x)}
          end
      fun store i x y = PRIM(STOREprim {index=i,repvar=rp},[x,y])
      fun read i x = PRIM(READprim {index=i,repvar=rp},[x])
      fun seq e k = bind unitType e (fn _ => k)
      val allocation = RECORD {regvar=NONE,repvar=rp,
        elements=[(F64 "1.5",raw),(STRING("before",NONE),ptr),(INTEGER(7,intTy),plain)]}
      val frame = FRAME {declared_lvars=[],declared_excons=[]}
      fun checkLayout x k =
          if js then k
          else seq (PRIM(CCALLprim {name="recordLayoutCheck",instances=[],regvars=[],tyvars=[],
                                   Type=ARROWtype([ty],NONE,[unitType],NONE)},[x])) k
      val e = bind ty allocation (fn x => bind ty x (fn alias =>
        checkLayout x (seq (store 0 x
               (bind unitType (store 1 alias (STRING("after",NONE))) (fn _ => F64 "3.5")))
          (seq (PRIM(CCALLprim {name=if js then "callJS" else "@recordCheck",instances=[],regvars=[],tyvars=[],
                               Type=ARROWtype((if js then [stringType] else []) @ [f64Type,stringType,intTy],NONE,[unitType],NONE)},
                (if js then [STRING("recordCheck",NONE)] else []) @
                [read 0 alias,read 1 x,PRIM(SELECTprim {index=2,repvar=rp},[alias])])) frame))))
      val pgm = PGM(DATBINDS [],e)
      val _ = LambdaStatSem.type_check
          {env=LambdaStatSem.initial,pgm=pgm,letrec_polymorphism_only=false}
      val (pgm,_) = OptLambda.optimise(OptLambda.initial,pgm)
      val _ = LambdaStatSem.type_check
          {env=LambdaStatSem.initial,pgm=pgm,letrec_polymorphism_only=false}
    in pgm
    end
end
