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
  fun makeFlatten floats js =
    let val intTy = intDefaultType()
        val raw = if floats then RecordField.mutable else {mutable=true,traversable=false}
        val floatTy = if floats then realType else f64Type
        fun convert name a b e = PRIM(CCALLprim{name=name,instances=[],regvars=[],tyvars=[],
            Type=ARROWtype([a],NONE,[b],NONE)},[e])
        fun box e = if floats then convert "__f64_to_real" f64Type realType e else e
        fun unbox e = if floats then convert "__real_to_f64" realType f64Type e else e
        val mut = RecordField.mutable
        val plain = RecordField.ordinary
        val innerFields = if floats then [(stringType,mut),(floatTy,raw)]
                          else [(floatTy,raw),(stringType,mut)]
        val innerTy = RECORDtype {fields=innerFields,regvar=NONE,repvar=NONE}
        val outerTy = recordType([recordType([],NONE),innerTy,intTy],NONE)
        fun var x = VAR {lvar=x,instances=[],regvars=[]}
        fun bind t e k =
            let val x = Lvars.newLvar()
            in LET {pat=[(x,[],t)],bind=e,scope=k(var x)}
            end
        fun select i e = PRIM(SELECTprim {index=i,repvar=NONE},[e])
        fun index i = if floats then 1-i else i
        fun read i e =
            let val r = PRIM(READprim {index=index i,repvar=NONE},[select 1 e])
            in if i=0 then unbox r else r
            end
        fun store i e v = PRIM(STOREprim {index=index i,repvar=NONE},[select 1 e,if i=0 then box v else v])
        val allocation = recordExp(NONE,
            [recordExp(NONE,[]),RECORD {regvar=NONE,repvar=NONE,
               elements=if floats then [(STRING("before",NONE),mut),(box(F64 "1.5"),raw)]
                        else [(F64 "1.5",raw),(STRING("before",NONE),mut)]},INTEGER(7,intTy)])
        val e = bind outerTy allocation (fn x => bind outerTy x (fn alias =>
            bind unitType (store 0 x
                (bind unitType (store 1 alias (STRING("after",NONE))) (fn _ => F64 "3.5"))) (fn _ =>
              bind unitType (PRIM(CCALLprim
                  {name=if js then "callJS" else "@recordCheck",instances=[],regvars=[],tyvars=[],
                   Type=ARROWtype((if js then [stringType] else []) @ [f64Type,stringType,intTy],NONE,[unitType],NONE)},
                  (if js then [STRING("recordCheck",NONE)] else []) @ [read 0 alias,read 1 x,select 2 alias]))
                (fn _ => FRAME {declared_lvars=[],declared_excons=[]}))))
        val () = List.app Flags.turn_on ["data_flattening","data_flattening_tuples"]
        val () = if floats then Flags.turn_on "data_flattening_floats"
                 else Flags.turn_off "data_flattening_floats"
        val pgm as PGM(_,e) = Flatten.optimise {enabled=true,env=LambdaStatSem.initial} (PGM(DATBINDS [],e))
        val n = LambdaBasics.foldTD (fn n => fn RECORD _ => n+1 | _ => n) 0 e
        val () = if n=1 then () else raise Fail "mixed tuple did not flatten"
        val boxes = LambdaBasics.foldTD (fn n => fn PRIM(CCALLprim{name="__f64_to_real",...},_) => n+1 | _ => n) 0 e
        val () = if floats andalso boxes <> 0 then raise Fail "float boxes remain" else ()
    in pgm
    end

  val make = fn js => case OS.Process.getEnv "FLATTEN_RECORD_TEST" of
                         SOME "1" => makeFlatten false js
                       | SOME "float" => makeFlatten true js | _ => make js

end
