(* A presentation-only view of declarations and layout changes retained by data flattening.
   Never change name stamps, representation variables, or boxities to print. *)
structure DatatypeRepr :
sig
  val layout : {original:LambdaExp.LambdaPgm, result:LambdaExp.LambdaPgm}
               -> PrettyPrint.StringTree option
  val final : {env:LambdaStatSem.env, result:LambdaExp.LambdaPgm}
              -> PrettyPrint.StringTree option
  val batch : {fields:(string * int) list, order:int list, before:LambdaExp.Type, after:LambdaExp.Type}
               -> PrettyPrint.StringTree
  val change : {kind:string, index:int, order:int list, before:LambdaExp.Type, after:LambdaExp.Type}
               -> PrettyPrint.StringTree
end =
struct
  open LambdaExp
  structure PP = PrettyPrint

  fun render finalGroups instances changes =
    let
      val groups = finalGroups
      val selected = List.foldl (fn ((_,n,_),m) => TyName.Map.add(n,(),m))
                           TyName.Map.empty (List.concat groups)
      val names = ref TyName.Map.empty
      val next = ref 0
      val references = ref []
      fun name n =
        case TyName.Map.lookup (!names) n of
            SOME s => s
          | NONE =>
              if List.exists (fn t => TyName.eq(n,t)) TyName.tynamesPredefined then
                TyCon.pr_TyCon(TyName.tycon n)
              else
                let val () = next := !next + 1
                    val s = TyCon.pr_TyCon(TyName.tycon n) ^ "#" ^ Int.toString(!next)
                    val () = names := TyName.Map.add(n,s,!names)
                    val () = if Option.isSome(TyName.Map.lookup selected n) then ()
                             else references := (n,s) :: !references
                in s
                end
      (* Assign declaration names before printing recursive references. *)
      val () = List.app (fn (_,n,_) => ignore(name n)) (List.concat groups)
      fun node start finish sep children =
          PP.NODE{start=start,finish=finish,indent=2,childsep=sep,children=children}
      fun typeTree variable t =
        let fun ty t = case t of
          TYVARtype{tv} => PP.LEAF(variable tv)
        | ARROWtype(a,_,b,_) => node "(" ")" (PP.RIGHT " -> ")
            [node "<" ">" (PP.RIGHT ", ") (map ty a),
             node "<" ">" (PP.RIGHT ", ") (map ty b)]
        | CONStype(ts,n,_) =>
            (case ts of
                 [] => PP.LEAF(name n)
               | [t] => node "(" (") " ^ name n) PP.NOSEP [ty t]
               | _ => node "(" (") " ^ name n) (PP.RIGHT ", ") (map ty ts))
        | RECORDtype{fields,repvar,...} =>
            node "(" (")^" ^ (case repvar of NONE => "?" | SOME r => RepVar.layout r))
              (PP.RIGHT " * ")
              (map (fn (t,m) => node (RecordField.layout m) "" PP.NOSEP [ty t]) fields)
        in ty t
        end
      val ty = typeTree pr_tyvar
      fun instanceTree variable (c,result,payload) =
          node (Con.str c ^ " : ") "" (PP.RIGHT " ; payload ")
            [typeTree variable result,typeTree variable payload]
      (* Compare printed information, including field modes and representation
         classes, modulo type-variable names. Do not unify or mutate types. *)
      fun instanceKey entry =
        let val variables = ref []
            fun variable tv =
              case List.find (fn (v,_) => v = tv) (!variables) of
                  SOME(_,s) => s
                | NONE =>
                    let val s = "'a" ^ Int.toString(length(!variables))
                    in variables := (tv,s) :: !variables; s
                    end
        in PP.flatten1(instanceTree variable entry)
        end
      val byDatatype = List.foldl (fn ((_,_,NONE),m) => m
          | ((c,result as CONStype(_,n,_),SOME payload),m) =>
              TyName.Map.add(n,(c,result,payload) ::
                Option.getOpt(TyName.Map.lookup m n,[]),m)
          | (_,_) => Crash.impossible "DatatypeRepr.render: expected constructor result type")
          TyName.Map.empty instances
      fun declaration keyword (vs,n,cs) =
        let val parameters = case vs of
                  [] => ""
                | [v] => pr_tyvar v ^ " "
                | _ => "(" ^ String.concatWith ", " (map pr_tyvar vs) ^ ") "
            fun constructor (c,NONE) = PP.LEAF(Con.str c)
              | constructor (c,SOME t) = node (Con.str c ^ " of ") "" PP.NOSEP [ty t]
            val definition = node (keyword ^ parameters ^ name n ^ " [" ^
                TyName.pr_boxity(TyName.boxity n) ^ "] = ") "" (PP.LEFT " | ") (map constructor cs)
            val result = CONStype(map (fn tv => TYVARtype{tv=tv}) vs,n,NONE)
            val seen = ref (List.foldl (fn ((_,NONE),s) => s
                | ((c,SOME t),s) => StringSet.insert (instanceKey(c,result,t)) s)
                StringSet.empty cs)
            val entries = rev(Option.getOpt(TyName.Map.lookup byDatatype n,[]))
            val entries = List.filter (fn entry =>
                let val key = instanceKey entry
                in if StringSet.member key (!seen) then false
                   else (seen := StringSet.insert key (!seen); true)
                end) entries
            val details = if null entries then [] else
                [node "  " "" PP.NOSEP
                  [node "Constructor instances (final layouts): " "" (PP.RIGHT "\n")
                    (map (instanceTree pr_tyvar) entries)]]
        in PP.NODE{start="",finish="",indent=0,childsep=PP.RIGHT "\n",
                   children=definition :: details}
        end
      fun group [] = []
        | group (d::ds) = declaration "datatype " d :: map (declaration "and ") ds
      val declarations = map (fn g => PP.NODE{start="",finish="",indent=0,
          childsep=PP.RIGHT "\n",children=group g}) groups
      val layouts = map (fn {label,order,before=source,after=target} =>
            node label "" (PP.RIGHT " => ")
              [ty source, node "" (if null order then "" else
                   " (target fields from spliced positions " ^
                   String.concatWith "," (map Int.toString order) ^ ")") PP.NOSEP [ty target]]) changes
      val referenced = map (fn (n,s) => PP.LEAF(s ^ (if null changes then " [" ^ TyName.pr_boxity(TyName.boxity n) ^ "]" else "")))
                           (rev(!references))
    in if null groups andalso null instances andalso null changes then NONE
       else SOME(node (if null changes then "Datatype representations (final declarations):\n" else "") "" (PP.RIGHT "\n")
         (declarations @ layouts @
          (if null referenced then [] else
             [node (if null changes then "Referenced types: "
                    else "Type names (local to this change): ") "" (PP.RIGHT ", ") referenced]) @
          (if not(null changes) then [] else [PP.LEAF "Boxities: box=boxed; lub=low-unboxed; hub=high-unboxed; enum=enumeration; single k=one unary constructor, argument boxity k.",
           PP.LEAF "Fields: var=mutable; raw=not GC-traversable; ^rN:span=tuple representation; ^top=fixed; ^?=unannotated.",
           PP.LEAF "Constructor boxity is separate from its tuple argument layout. Names are local to this report."])))
    end
  fun layout {original=_,result=PGM(DATBINDS groups,_)} = render groups [] []

  fun final {env,result=PGM(DATBINDS groups,e)} =
    let
      fun subst vs ts t =
        let val pairs = ListPair.zip(vs,ts)
            fun go (t as TYVARtype{tv}) =
                  (case List.find (fn (v,_) => v = tv) pairs of NONE => t | SOME(_,t) => t)
              | go (ARROWtype(a,r,b,s)) = ARROWtype(map go a,r,map go b,s)
              | go (CONStype(ts,n,r)) = CONStype(map go ts,n,r)
              | go (RECORDtype{fields,regvar,repvar}) =
                  RECORDtype{fields=map (fn (t,m) => (go t,m)) fields,regvar=regvar,repvar=repvar}
        in go t
        end
      val localCons = List.foldl (fn ((vs,n,cs),m) =>
          List.foldl (fn ((c,t),m) => Con.Map.add(c,(vs,n,t),m)) m cs)
          Con.Map.empty (List.concat groups)
      val declarations = ref (List.foldl (fn (d as (_,n,_),m) => TyName.Map.add(n,d,m))
          TyName.Map.empty (List.concat groups))
      val imported = ref TyName.Map.empty
      val importedGroups = ref []
      fun parts (ARROWtype([payload],_,[result],_)) = (result,SOME payload)
        | parts result = (result,NONE)
      fun importDeclaration (n,vs,cs) =
          if Option.isSome(TyName.Map.lookup (!imported) n) then () else
          let val constructors = map (fn (c,(ws,t)) =>
                  (c,Option.map (subst ws (map (fn v => TYVARtype{tv=v}) vs)) (#2(parts t)))) cs
          in imported := TyName.Map.add(n,(),!imported);
             importedGroups := [(vs,n,constructors)] :: !importedGroups;
             declarations := TyName.Map.add(n,(vs,n,constructors),!declarations)
          end
      fun external con =
        let val (vs,t) = LambdaStatSem.look_con env con
            val (result,payload) = parts t
            val n = case result of
                        CONStype(_,n,_) => n
                      | _ => Crash.impossible "DatatypeRepr.external: expected constructor result type"
            val () = importDeclaration
                (n,vs,Option.getOpt(LambdaStatSem.look_tyname env n,[(con,(vs,t))]))
        in (vs,n,payload)
        end
      fun instance con ts =
        let val (vs,n,payload) = case Con.Map.lookup localCons con of
                  SOME entry => entry | NONE => external con
        in (con,CONStype(ts,n,NONE),Option.map (subst vs ts) payload)
        end
      val instances = rev(LambdaBasics.foldTD (fn acc => fn exp => case exp of
            PRIM(CONprim{con,instances,...},_) => instance con instances :: acc
          | PRIM(DECONprim{con,instances,...},_) => instance con instances :: acc
          | _ => acc) [] e)
      (* Explicit annotations and instantiated operation types identify the
         roots. Read only the schemes of free identifiers, not the whole
         environment: unrelated imports must not keep declarations alive. *)
      val reachable = ref TyName.Set.empty
      fun discover n =
        if TyName.Set.member n (!reachable) then () else
        let val () = reachable := TyName.Set.insert n (!reachable)
            val () = case TyName.Map.lookup (!declarations) n of
                  SOME _ => ()
                | NONE =>
                    (case LambdaStatSem.look_tyname env n of
                         SOME (cs as (_,(vs,_))::_) => importDeclaration(n,vs,cs)
                       | _ => ())
        in case TyName.Map.lookup (!declarations) n of
               SOME(_,_,cs) => List.app (fn (_,t) => Option.app discoverType t) cs
             | NONE => ()
        end
      and discoverType t =
          ignore(foldType (fn () => fn CONStype(_,n,_) => discover n | _ => ()) () t)
      fun constructor con =
        let val (_,n,_) = case Con.Map.lookup localCons con of
                  SOME entry => entry | NONE => external con
        in discover n
        end
      val (boundVars,boundExceptions) = LambdaBasics.foldTD (fn (vs,es) => fn exp =>
          let fun variable (x,m) = Lvars.Map.add(x,(),m)
          in case exp of
                 FN{pat,...} => (List.foldl (fn ((x,_),m) => variable(x,m)) vs pat,es)
               | LET{pat,...} => (List.foldl (fn ((x,_,_),m) => variable(x,m)) vs pat,es)
               | FIX{functions,...} =>
                   (List.foldl (fn ({lvar,...},m) => variable(lvar,m)) vs functions,es)
               | EXCEPTION(c,_,_) => (vs,Excon.Map.add(c,(),es))
               | _ => (vs,es)
          end) (Lvars.Map.empty,Excon.Map.empty) e
      fun variable x =
          if Option.isSome(Lvars.Map.lookup boundVars x) then ()
          else discoverType(#2(LambdaStatSem.look_lvar env x))
      fun exceptionType c =
          if Option.isSome(Excon.Map.lookup boundExceptions c) then ()
          else Option.app discoverType (LambdaStatSem.look_excon env c)
      val _ = mapExpTypes (fn t => (discoverType t;t)) e
      val () = List.app (fn (_,t,p) =>
          (discoverType t; Option.app discoverType p)) instances
      val () = LambdaBasics.foldTD (fn () => fn exp => case exp of
            VAR{lvar,...} => variable lvar
          | CHECK_REML{lvar,...} => variable lvar
          | SWITCH_C(SWITCH(_,cases,_)) => List.app (constructor o #1 o #1) cases
          | SWITCH_E(SWITCH(_,cases,_)) => List.app (exceptionType o #1 o #1) cases
          | PRIM(EXCONprim c,_) => exceptionType c
          | PRIM(DEEXCONprim c,_) => exceptionType c
          | PRIM(EQUALprim _,_) => discover TyName.tyName_BOOL
          | _ => ()) () e
      fun liveGroup g = List.filter (fn (_,n,_) => TyName.Set.member n (!reachable)) g
      val groups = List.filter (not o null) (map liveGroup (groups @ rev(!importedGroups)))
    in render groups instances []
    end

  (* Freeze the presentation before subsequent constraint solving changes links. *)
  fun change {kind,index,order,before=source,after=target} =
      valOf(render [] [] [{label=kind ^ " flattening at field " ^ Int.toString index ^ ": ",
                           order=order,before=source,after=target}])
  fun batch {fields,order,before=source,after=target} =
      valOf(render [] [] [{label="batch (" ^ String.concatWith "; "
          (map (fn (kind,i) => kind ^ " flattening at field " ^ Int.toString i) fields) ^ "): ",
          order=order,before=source,after=target}])
end
