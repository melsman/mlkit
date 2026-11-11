(* A presentation-only view of declarations retained by data flattening.
   Never change name stamps, representation variables, or boxities to print. *)
structure DatatypeRepr :
sig
  val layout : {original:LambdaExp.LambdaPgm, result:LambdaExp.LambdaPgm}
               -> PrettyPrint.StringTree option
end =
struct
  open LambdaExp
  structure PP = PrettyPrint

  fun layout {original=PGM(DATBINDS originalGroups,_),result=PGM(DATBINDS finalGroups,_)} =
    let
      val originals = List.foldl (fn ((_,n,_),m) => TyName.Map.add(n,(),m))
                            TyName.Map.empty (List.concat originalGroups)
      fun specialised (_,n,_) = not(Option.isSome(TyName.Map.lookup originals n))
      val groups = List.filter (not o null) (map (List.filter specialised) finalGroups)
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
      fun ty t = case t of
          TYVARtype{tv} => PP.LEAF(pr_tyvar tv)
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
      fun declaration (vs,n,cs) =
        let val parameters = case vs of
                  [] => ""
                | [v] => pr_tyvar v ^ " "
                | _ => "(" ^ String.concatWith ", " (map pr_tyvar vs) ^ ") "
            fun constructor (c,NONE) = PP.LEAF(Con.str c)
              | constructor (c,SOME t) = node (Con.str c ^ " of ") "" PP.NOSEP [ty t]
        in node (parameters ^ name n ^ " [" ^ TyName.pr_boxity(TyName.boxity n) ^
                 "] = ") "" (PP.LEFT " | ") (map constructor cs)
        end
      val declarations = map (fn g => node "datatype " "" (PP.LEFT " and ")
                                            (map declaration g)) groups
      val referenced = map (fn (n,s) => PP.LEAF(s ^ " [" ^ TyName.pr_boxity(TyName.boxity n) ^ "]"))
                           (rev(!references))
    in if null groups then NONE
       else SOME(node "Datatype representations (final specialised declarations):\n" "" (PP.RIGHT "\n")
         (declarations @
          (if null referenced then [] else
             [node "Referenced types (not specialised here): " "" (PP.RIGHT ", ") referenced]) @
          [PP.LEAF "Boxities: box=boxed; lub=low-unboxed; hub=high-unboxed; enum=enumeration; single k=one unary constructor, argument boxity k.",
           PP.LEAF "Fields: var=mutable; raw=not GC-traversable; ^rN:span=tuple representation; ^top=fixed; ^?=unannotated.",
           PP.LEAF "Constructor boxity is separate from its tuple argument layout. Names are local to this report."]))
    end
end
