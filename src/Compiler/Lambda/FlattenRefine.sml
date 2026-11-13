structure FlattenRefine : FLATTEN_REFINE =
struct
  open LambdaExp
  exception Reject
  fun member x xs = List.exists (fn y => x = y) xs
  fun refine requested (PGM(db,e)) =
    let
      val exports = LambdaBasics.foldTD
            (fn xs => fn FRAME{declared_lvars,...} => map #lvar declared_lvars @ xs
              | RAISE(_,Frame{declared_lvars,...}) => map #lvar declared_lvars @ xs
              | _ => xs) [] e
      (* Index the compilation unit once; each trial follows only the owners
         and call sites of the parameters it actually constrains. *)
      val exported = List.foldl (fn (x,m) => Lvars.Map.add(x,(),m)) Lvars.Map.empty exports
      val bindings = ref (Lvars.Map.empty : tyvar list Lvars.Map.map)
      val owners = ref (TyvarMap.empty : (lvar * tyvar list * bool * int) list TyvarMap.map)
      val uses = ref (Lvars.Map.empty : Type list list Lvars.Map.map)
      val unsupported = ref false
      val reserved = ref ([] : tyvar list)
      val next = ref 0
      fun group () = (next := !next+1; !next)
      fun add g (x,vs,ok) =
        let val entry = (x,vs,ok andalso not(Option.isSome(Lvars.Map.lookup exported x)),g)
            fun owner v = owners := TyvarMap.add(v,entry ::
                  (case TyvarMap.lookup (!owners) v of NONE => [] | SOME xs => xs),!owners)
        in bindings := Lvars.Map.add(x,vs,!bindings);
           List.app owner vs
        end
      fun collect mono e = case e of
          FIX{functions,scope} =>
            let val g = group()
                val locals = List.foldl (fn ({lvar,...},m) => Lvars.Map.add(lvar,(),m)) mono functions
                val () = List.app
                  (fn {lvar,tyvars,regvars,constrs,bind,...} =>
                    add g (lvar,tyvars,null regvars andalso null constrs andalso
                        (case bind of FN _ => true | _ => false))) functions
            in List.app (fn {bind,...} => collect locals bind) functions; collect mono scope
            end
        | _ =>
          (case e of
               LET{pat,bind,...} => List.app (fn (x,vs,_) =>
                 add (group()) (x,vs,length pat = 1 andalso (case bind of FN _ => true | _ => false))) pat
             | VAR{lvar,instances,...} =>
                 if Option.isSome(Lvars.Map.lookup mono lvar) then
                   (if null instances then () else unsupported := true)
                 else uses := Lvars.Map.add(lvar,instances ::
                     (case Lvars.Map.lookup (!uses) lvar of NONE => [] | SOME ts => ts),!uses)
             | PRIM(CCALLprim{tyvars,...},_) => reserved := tyvars @ !reserved
             | CHECK_REML _ => unsupported := true
             | LETREGION _ => unsupported := true
             | _ => ();
           LambdaBasics.app_lamb (collect mono) e)
      val () = collect Lvars.Map.empty e
      val free = tyvars_Exp TyvarSet.empty e TyvarSet.empty
      val reserved = TyvarSet.fromList (!reserved)
      val active = ref ([] : tyvar list)
      val pending = ref ([] : (Type * Type) list)
      val subst = ref ([] : (tyvar * Type) list)
      val fuel = ref 10000
      fun tick () = if !fuel = 0 then raise Reject else fuel := !fuel - 1
      fun activate v =
        if member v (!active) then () else
        if length (!active) >= 64 orelse TyvarSet.member v free orelse TyvarSet.member v reserved then raise Reject else
        let val owners = case TyvarMap.lookup (!owners) v of NONE => [] | SOME xs => xs
            val g = case owners of (_,_,true,g)::_ => g | _ => raise Reject
            val () = if List.all (fn (_,_,ok,h) => ok andalso g = h) owners then () else raise Reject
            val calls = ref 0
            fun owner (x,vs,_,_) =
              let fun index (w::ws) i = if v = w then i else index ws (i+1)
                    | index [] _ = raise Reject
                  val i = index vs 0
                  fun equation ts =
                    if length ts <> length vs then raise Reject else
                    (calls := !calls+1; pending := (TYVARtype{tv=v},List.nth(ts,i)) :: !pending)
              in List.app equation (case Lvars.Map.lookup (!uses) x of NONE => [] | SOME ts => ts)
              end
        in active := v :: !active;
           List.app owner owners;
           if !calls = 0 then raise Reject else ()
        end
      fun resolve t =
        (tick(); case t of
             TYVARtype{tv} =>
               (case List.find (fn (v,_) => tv = v) (!subst) of
                    NONE => t | SOME(_,u) => resolve u)
           | ARROWtype(a,r,b,s) => ARROWtype(map resolve a,r,map resolve b,s)
           | CONStype(ts,n,r) => CONStype(map resolve ts,n,r)
           | RECORDtype{fields,regvar,repvar} => RECORDtype
               {fields=map (fn (t,m) => (resolve t,m)) fields,regvar=regvar,repvar=repvar})
      fun bind v t =
        (activate v;
         List.app activate (tyvars t);
         if member v (tyvars t) then raise Reject else subst := (v,t) :: !subst)
      fun pairs (a,b) = if length a = length b then ListPair.appEq unify(a,b) else raise Reject
      and unify (a,b) =
        let val a = resolve a
            val b = resolve b
        in case (a,b) of
             (TYVARtype{tv=v},TYVARtype{tv=w}) => if v = w then () else bind v b
           | (TYVARtype{tv},_) => bind tv b
           | (_,TYVARtype{tv}) => bind tv a
           | (CONStype(a,n,r),CONStype(b,m,s)) =>
               if TyName.eq(n,m) andalso not(Option.isSome r) andalso not(Option.isSome s)
               then pairs(a,b) else raise Reject
           | (ARROWtype(a,NONE,b,NONE),ARROWtype(c,NONE,d,NONE)) => (pairs(a,c); pairs(b,d))
           | (RECORDtype{fields=a,regvar=NONE,...},RECORDtype{fields=b,regvar=NONE,...}) =>
               if length a = length b andalso ListPair.allEq (fn ((_,m),(_,n)) => m = n) (a,b)
               then pairs(fieldTypes a,fieldTypes b) else raise Reject
           | _ => raise Reject
        end
      fun solve () = case !pending of [] => () | p::ps =>
            (pending := ps; unify p; solve())
      val () = if !unsupported orelse null requested then raise Reject else List.app activate requested
      val () = solve()
      fun concrete v =
        let val t = resolve(TYVARtype{tv=v})
        in if null(tyvars t) andalso not(contains_regvars t) then (v,mapTypeRepvars (fn _ => NONE) t) else raise Reject
        end
      val replacements = map concrete (!active)
      val s = LambdaBasics.mk_subst (fn () => "flattening refinement")
                  (map #1 replacements,map #2 replacements)
      fun keep v = not(member v (!active))
      fun arguments x ts =
        if null ts then ts else
        case Lvars.Map.lookup (!bindings) x of
            NONE => ts
          | SOME vs =>
              if length vs <> length ts then raise Reject else
              List.mapPartial (fn (v,t) => if keep v then SOME t else NONE) (ListPair.zip(vs,ts))
      fun rewrite e = case e of
          VAR{lvar,instances,regvars} => VAR{lvar=lvar,instances=arguments lvar instances,regvars=regvars}
        | FIX{functions,scope} => FIX{scope=scope,functions=map
            (fn {lvar,tyvars,regvars,Type,constrs,bind} =>
              {lvar=lvar,tyvars=List.filter keep tyvars,regvars=regvars,Type=Type,constrs=constrs,bind=bind}) functions}
        | LET{pat,bind,scope} => LET{pat=map (fn (x,vs,t) => (x,List.filter keep vs,t)) pat,bind=bind,scope=scope}
        | _ => e
      val result = mapExpTypes (LambdaBasics.on_Type s) (LambdaBasics.passBU rewrite e)
    in SOME(PGM(db,result))
    end handle Reject => NONE
end
