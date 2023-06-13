
structure Effect: EFFECT =  (* comment out this signature before
                             * running TestEffect *)
struct
  structure PP = PrettyPrint
  structure G = DiGraph

  (* Add some dynamic flags for pretty-printing region variables. *)

  val regionvarInitial = Flags.add_int_entry
      {long="regionvar", short=NONE, item=ref ~1,
       menu=["Control", "region variable initial value"], desc=
       "Uses the provided number as the id of the first\n\
        \generated region variable. When this option is\n\
        \provided together with the -c option, a file f.rv\n\
        \is written in the MLB/ directory with two numbers\n\
        \in it: the id for the first region variable\n\
        \generated and the id for the last region variable\n\
        \generated. The number given must be greater than\n\
        \any id for a top-level region/effect variable (>9)."}

  val print_rho_levels = Flags.add_bool_entry
      {long="print_rho_levels", short=NONE, item=ref false, neg=false,
       menu=["Layout", "print levels of region variables"], desc=
       "Print levels of region and effect variables in types and\n\
        \intermediate forms. Levels control quantification of\n\
        \region and effect variables."}

  val print_rho_types = Flags.add_bool_entry
       {long="print_rho_types", short=NONE, item=ref false, neg=false,
        menu=["Layout","print runtime types of region variables"], desc=
        "Print region types of region variables in types and\n\
         \intermediate forms. Possible region types are:\n\
         \    p  Type of regions containing pairs.\n\
         \    a  Type of regions containing arrays.\n\
         \    r  Type of regions containing references.\n\
         \    t  Type of regions containing triples.\n\
         \    s  Type of regions containing strings.\n\
         \    T  Type of regions containing other than the above\n\
         \       kinds of values."}

  val print_regions = Flags.add_bool_entry
      {long="print_regions", short=SOME "Pregions", item=ref true, neg=true,
       menu=["Layout", "print regions"], desc=
       "Print region variables in types and expressions."}

  val print_effects = Flags.add_bool_entry
      {long="print_effects", short=SOME "Peffects", item=ref false, neg=false,
       menu=["Layout", "print effects"], desc=
       "Print effects in region types."}

  val print_rho_protection = Flags.add_bool_entry
      {long="print_rho_protection", short=SOME "Prho_protection", item=ref false, neg=false,
       menu=["Layout", "print rho protection"], desc=
       "Print protectedness of region variables if set (P or U)."}

  type StringTree = PP.StringTree

  fun die s = (print ("Effect." ^ s ^ "\n"); Crash.impossible("Effect." ^ s))
  fun log_tree (tr: StringTree) =
      PP.outputTree(fn s => TextIO.output(!Flags.log, s), tr, !Flags.colwidth)
  fun say i = TextIO.output(TextIO.stdOut, i)
  fun say_etas (trl: StringTree list) =
      PP.outputTree(fn s => TextIO.output(TextIO.stdOut, s),
                    PP.NODE{start = "[", finish= "]", indent=1,
                            childsep = PP.RIGHT",", children = trl},
                    !Flags.colwidth)
  fun log_string  s = TextIO.output(!Flags.log, s ^ "\n")

  fun noSome (NONE, s) = die s
    | noSome (SOME v, _) = v

  datatype runType = STRING_RT | PAIR_RT | TOP_RT | BOT_RT
                   | ARRAY_RT | REF_RT | TRIPLE_RT

  fun ord_runType STRING_RT = 1
    | ord_runType PAIR_RT = 2
    | ord_runType ARRAY_RT = 3
    | ord_runType REF_RT = 4
    | ord_runType TRIPLE_RT = 5
    | ord_runType TOP_RT = 6
    | ord_runType BOT_RT = 7   (* for supporting explicit region declatations *)

  fun show_runType tau =
      case tau of
           PAIR_RT => "p"
         | STRING_RT => "s"
         | ARRAY_RT => "a"
         | REF_RT => "r"
         | TRIPLE_RT => "t"
         | TOP_RT => "T"
         | BOT_RT => "B"

  fun lub_runType0 (rt1,rt2) =
      if rt1 = rt2 then SOME rt1
      else if rt1 = BOT_RT then SOME rt2
      else if rt2 = BOT_RT then SOME rt1
      else NONE

  fun lub_runType (rt1,rt2) =
      case lub_runType0 (rt1,rt2) of
          SOME rt => rt
        | NONE => die ("Trying to unify runtype " ^ show_runType rt1 ^ " with runtype " ^ show_runType rt2)

  type key = int ref (* for printing and sorting of nodes *)
  fun show_key (ref i) = Int.toString i
  fun layout_key r = PP.LEAF(show_key r)

  fun key_lt (ref i, ref (j:int)) = i<j

  fun show_key (ref i) = Int.toString i

  type level = int ref (* for stratification of cones *)
  fun show_level (ref i) = Int.toString i
  fun layout_level l = PP.LEAF(show_level l)

  fun show_protection (ref 0) = ""
    | show_protection (ref 1) = "U"
    | show_protection (ref _) = "P"

  type regvar = RegVar.regvar

  (* info in nodes of effect graphs *)
  datatype einfo = EPS of {key: key,
                           level:  level,
                           represents: einfo G.node list option,
                           instance : einfo G.node option ref,
                           pix: int ref,
                           explicit : regvar option}
                 | UNION of {represents: einfo G.node list option}
                 | PUT | GET
                 | RHO of {put: einfo G.node option,
                           get: einfo G.node option,
                           key: key,
                           level: level,
                           instance : einfo G.node option ref,
                           pix : int ref,      (* pre-order index; for normalised type schemes *)
                           ty : runType,
                           explicit : regvar option,
                           protected : int ref}

  fun layout_einfo einfo =
      case einfo of
          EPS{key,level,explicit,...} =>
          let val n = case explicit of
                          NONE => "e" ^ show_key key
                        | SOME ev => "`" ^ RegVar.pr ev ^ "_" ^ show_key key
          in PP.LEAF(n ^ (if print_rho_levels() then
                            "(" ^ show_level level ^ ")"
                          else ""))
          end
        | PUT => PP.LEAF "put"
        | GET => PP.LEAF "get"
        | UNION _ => PP.LEAF "U"
        | RHO{key,level,ty,put,explicit,protected,...} =>
          let val n = case explicit of
                          NONE => "r" ^ show_key key
                        | SOME rv => "`" ^ RegVar.pr rv ^ "_" ^ show_key key
          in PP.LEAF (n ^
                      (if print_rho_types() then show_runType ty
                       else "") ^
                      (if print_rho_protection() then show_protection protected
                       else "") ^
                      (if print_rho_levels() then "(" ^ show_level level ^ ")"
                       else "")
                     )
          end

  type effect = einfo G.node
  type place = effect
  val empty = G.mk_node (UNION{represents = NONE})

  fun eq_effect (node1, node2) = G.eq_nodes(node1,node2)

  fun layout_effect e = G.layout_node layout_einfo e
  fun layout_effect_deep e = G.layout_nodes_deep layout_einfo [e]

  fun get_instance effect =
      case G.find_info effect of
          EPS{instance, ...} => instance
        | RHO{instance, ...} => instance
        | _ => die "get_instance"

  fun is_arrow_effect effect =
      case G.find_info effect of
          EPS _ => true
        | _ => false

  fun is_union (UNION _) = true
    | is_union _ = false

  fun is_rho effect =
      case G.find_info effect of
          RHO _ => true
        | _ => false

  (* acc_rho effect acc conses effect onto acc iff
     acc is a RHO node which has a put effect on it.
     When effect is consed onto acc, its visited field is set.
     (Such a region should not be dropped - see DropRegions.drop_places *)

  fun acc_rho effect (acc: effect list): effect list =
      case (G.find_info effect, G.find_visited effect) of
          (RHO{put = SOME _, ...}, r as ref false) => (r:= true; effect :: acc)
        | _ => acc

  fun explicit_rho effect : regvar option =
      case G.find_info effect of
          RHO{explicit=SOME rv,...} => SOME rv
        | _ => NONE

  fun explicit_eps effect : regvar option =
      case G.find_info effect of
          EPS{explicit=SOME ev,...} => SOME ev
        | _ => NONE

  fun explicit_var e =
      case G.find_info e of
          RHO{explicit=SOME v,...} => SOME v
       |  EPS{explicit=SOME v,...} => SOME v
       | _ => NONE

  fun is_put effect =
      case G.find_info effect of
          PUT => true
        | _ => false

  fun is_get effect =
      case G.find_info effect of
          GET => true
        | _ => false

  fun is_put_or_get effect =
      case G.find_info effect of
          GET => true
        | PUT => true
        | _ => false

  fun get_level_and_key effect : (level*key) option =
      case G.find_info effect of
          EPS{level,key,...} => SOME(level,key)
        | RHO{level,key,...} => SOME(level,key)
        | _ => NONE

  fun get_key_of_eps_or_rho effect : int option =
      case G.find_info effect of
          EPS{level,key,...} => SOME(!key)
        | RHO{level,key,...} => SOME(!key)
        | _ => NONE

  fun level_of effect : int option =
      case G.find_info effect of
          EPS{level,key,...} => SOME(!level)
        | RHO{level,key,...} => SOME(!level)
        | _ => NONE

  fun setkey generator effect =
      case G.find_info effect of
          EPS{key, ...} => key := generator()
        | RHO{key, ...} => key := generator()
        | _ => ()

  fun get_level_of_rho effect : int =
      case G.find_info effect of
          RHO{level as ref l,...} => l
        | _ => die "GetLevelOfRho"

  fun key_of_rho effect : int =
      case G.find_info effect of
          RHO{key,...} => !key
        | _ => die "key_of_rho (not a RHO)"

  fun get_key_of_eps effect : int =
      case G.find_info effect of
          EPS{key as ref k,...} => k
        | _ => die "GetKeyOfEps"

  fun get_place_ty effect : runType option =
      case G.find_info effect of
          RHO{ty,...} => SOME ty
        | _ => NONE

  fun rho_of node =
      case G.out_of_node node of
          [rho] => rho
        | _ => die "rho_of"

  fun set_protect effect : unit =
      case G.find_info effect of
          RHO{protected,...} =>
          (case !protected of
               0 => protected := 2
             | 1 => die "set_protect: rho already set to unprotected"
             | _ => ())
        | _ => die "set_protect: expecting rho"

  fun set_unprotect effect : unit =
      case G.find_info effect of
          RHO{protected,...} =>
          (case !protected of
               0 => protected := 1
             | 1 => ()
             | _ => die "set_unprotect: rho already set to protected")
        | _ => die "set_unprotect: expecting rho"

  fun get_protect effect : bool option =
      case G.find_info effect of
          RHO{protected,...} =>
          (case !protected of
               0 => NONE
             | 1 => SOME false
             | _ => SOME true)
        | _ => die "get_protect: expecting rho"

  fun edge (from,to) = G.mk_edge(from,to);

  (* explicit region variables are created with a "pinned" level,
     meaning that their level is not allowed to be lowered. *)
  fun mkRho explicit (l,k) =
      G.mk_node(RHO{key = ref k, level = ref l,
                    put = NONE, get = NONE, instance = ref NONE,
                    pix = ref ~1, ty = BOT_RT, explicit=explicit,
                    protected=ref 0})

  fun mkPut (n: effect) = (* n must represent a region variable*)
      case G.find_info n of
          RHO{put=SOME n',...} => n'  (* hash consing *)
        | RHO{put=NONE,key,level,get,instance,pix,ty,explicit,protected} =>
          let val new = G.mk_node PUT (* create new node *)
          in G.set_info n (RHO{put=SOME new,
                               get=get,key=key,level=level,instance=instance,
                               pix=pix,ty=ty,explicit=explicit,protected=protected});
             G.mk_edge(new,n);
             new
          end
        | _ => die "mkPut: node does not represent region variable"

  fun mkGet (n: effect) = (* n must represent a region variable*)
      case G.find_info n of
          RHO{get=SOME n',...} => n'  (* hash consing *)
        | RHO{get=NONE,key,level,put,instance,pix,ty,explicit,protected} =>
          let val new = G.mk_node GET  (* create new node *)
          in G.set_info n (RHO{get=SOME new,
                               put=put,key=key,level=level,instance=instance,
                               pix=pix,ty=ty,explicit=explicit,protected=protected});
             G.mk_edge(new,n);
             new
          end
        | _ => die "mkGet: node does not represent region variable"

  fun mkUnion (l : effect list) =
      let val new = G.mk_node(UNION{represents=NONE})
      in app (fn n => G.mk_edge(new, n)) l;
         new
      end

  (* Explicit effect variables are created with a "pinned" level,
     meaning that their level is not allowed to be lowered. *)
  fun mkEps explicit (l,k) =
      G.mk_node(EPS{key = ref k, level = ref l,
                    represents = NONE, pix = ref ~1,
                    instance = ref NONE,
                    explicit=explicit})

  fun remove_duplicates effects =
      let fun loop([], acc) = acc
            | loop(effect::rest, acc) =
              let val r = (G.find_visited effect)
              in if !r then loop(rest,acc)
                 else (r:= true; loop(rest, effect::acc))
              end
          val result = loop(effects,[])
      in app (fn node => G.find_visited node:= false) result;
         result
      end

  (*********************************)
  (*     cones                     *)
  (*********************************)

  (* A cone is a finite map from an initial segment of the natural numbers
     to finite maps, which map node keys to nodes *)

  structure ConeLayer = struct
     open IntFinMap
     fun mkEmpty () = empty
     fun size m = fold (fn (_,acc) => acc+1) 0 m
     val fromSortedList = addList
  end

  type coneLayer = effect ConeLayer.map

  (* The Cone is implemented as an array, which
     represents a stack of coneLayers *)

  structure Cone : sig
                     type map  (* = coneLayer map *)
                     type cone
                     val max_cone_level: int
                     val empty:  map
                     val lookup: map -> int -> coneLayer option
                     val add: int * coneLayer * map -> map
                     val remove: int * map -> map option
                     val layoutMap: {start: string, eq: string, sep: string, finish: string} ->
                         (int -> StringTree) ->
                         (coneLayer -> StringTree) ->  cone -> StringTree
                     val reset: cone -> unit
                     val level: cone -> int
                     val emptyCone : cone
                     val info : cone -> string
                   end =
  struct
       val max_cone_level = 1000
       type map = coneLayer Array.array
       type cone = int * (*coneLayer*) map
     (* The integer is the number of levels in the cone;
        initially 0 *)
       val global_array : map  = Array.array(max_cone_level, ConeLayer.empty)
       val empty = global_array
       fun lookup _ i = SOME(Array.sub(global_array, i))
                    handle _ => NONE
       fun add (i,coneLayer,_) =
          (Array.update(global_array, i, coneLayer)
                    handle _ => die ("Cone.add: index "
                                     ^ Int.toString i
                                     ^ "out of range [0.."
                                     ^ Int.toString (i-1) ^ "]\n");
           global_array)

       fun remove (i,_) =
          (Array.update(global_array, i, ConeLayer.empty);
           SOME global_array)
           handle _ => NONE

       fun reset (_,array) =      (* reset levels 0 to max_cone_level -1 in array *)
         let fun reset_loop i =
               if i >= max_cone_level then ()
               else (Array.update(array, i, ConeLayer.empty);
                     reset_loop (i+1))
         in reset_loop 0
         end

       fun layoutMap {start: string, eq: string, sep: string, finish: string}
                     layoutInt
                     layoutConeLayer
                     cone =
         let val (n, table) = cone
             fun get_layers i =
                  if i > n then []
                  else (i,Array.sub(table, i)) :: get_layers(i+1)
         in PP.NODE{start = start, finish = finish,  indent = 3, childsep=PP.RIGHT sep,
                    children= map (fn (d,r) =>
                             PP.NODE {start="",
                                      finish="",
                                      children=[layoutInt d, layoutConeLayer r],
                                      indent=3,
                                      childsep=PP.RIGHT sep})
                                  (get_layers 1)}
         end

       fun level (i,_) = i
       val emptyCone = (0,empty)

       fun info (i,m) =
           let val sl = ArraySlice.slice (m,1,SOME i)
               val is = ArraySlice.foldl (fn (layer,acc) => Int.toString(ConeLayer.size layer) :: acc) [] sl
           in "Cone(" ^ Int.toString i ^ "; " ^ String.concatWith "," is ^ ")"
           end
  end

  val info = Cone.info
  type cone = Cone.cone
  val level = Cone.level
  val emptyLayer = ConeLayer.empty
  val emptyCone = Cone.emptyCone
  fun layoutLayer (layer: coneLayer) : PP.StringTree=
      ConeLayer.layoutMap{start = "{", finish = "}", eq = "=", sep = ","}
                         ( PP.LEAF o Int.toString)
                         layout_effect_deep
                         layer
  fun layoutLayerRng (layer: coneLayer): PP.StringTree =
      let val rng_without_duplicates = remove_duplicates (ConeLayer.range layer)
      in
          PP.HNODE{start = " ", finish = "", childsep = PP.RIGHT", ",
                   children = map layout_effect_deep rng_without_duplicates}
      end

  fun layoutCone (cone:cone) : PP.StringTree =
      Cone.layoutMap{start = "{", finish = "}\n", eq = "=", sep = ","}
                    (fn i: int => PP.LEAF("level " ^ Int.toString i))
                    layoutLayer
                    cone

  (* remove "effect" with "key" from "cone" at "level" *)

  fun remove (effect, level, key, cone as (n, c)): cone=
    case Cone.lookup c (!level) of
         NONE => die "remove: (no such level in cone)"
       | SOME layer => (case ConeLayer.remove(key,layer) of
           SOME layer' => (n,Cone.add(!level,layer',c))
                                    (* replaces old layer*)
         | _ => die ("remove: failed to remove effect " ^ PP.flatten1 (layout_effect effect) ^
                     "\nfrom cone at level " ^ Int.toString (!level) ^
                     "\n(no key " ^ Int.toString key ^ " in cone)" ^
                     "\n(key(effect) = " ^
                     (case get_key_of_eps_or_rho effect of
                          NONE => "NONE"
                        | SOME k => Int.toString k)))

  (* add "effect" with "key" to "cone" at "level" *)

  fun add (effect, level:int, key:int, cone as (n,c)): cone =
       case Cone.lookup c level of
         NONE => die ("add: (no such level in cone): " ^ Int.toString level)
       | SOME layer =>
           (n,Cone.add(level, ConeLayer.add(key,effect,layer), c))
                                   (* replaces old layer*)


  (* push(cone):   start a new level on top of cone *)

  fun push (cone as (n,c):cone): cone = (n+1, Cone.add(n+1,ConeLayer.mkEmpty(), c))

  (* sort: effect list -> effect list
     l' = sort(l):
     l is a list of effects without duplicates,
     each of which is a region variable or an effect variable;
     l' is l, sorted in descending order on keys *)

  exception Take and Drop

  fun take (0, _ ) = []
    | take (n, x::xs) = x::take(n-1, xs)
    | take (n, []) = raise Take

  fun drop (0, l) = l
    | drop (n, x::xs) = drop(n-1, xs)
    | drop (n, []) = raise Drop

  fun lt_eps_or_rho (eps_or_rho1, eps_or_rho2) =
      case (get_key_of_eps_or_rho eps_or_rho1, get_key_of_eps_or_rho eps_or_rho2) of
          (SOME x', SOME y') => x' < y'
        | _ => die "lt_eps_or_rho"

  fun merge ([], ys) : effect list = ys
    | merge (xs, []) = xs
    | merge (l as x::xs, r as y:: ys) =
      case (get_key_of_eps_or_rho x, get_key_of_eps_or_rho y) of
          (SOME x', SOME y') => if x'>= y' then x::merge(xs, r)
                                else y::merge(l, ys)
        | _ => die "merge: cannot sort effects that are neither region variables nor effect variables"

  (* sort: top-down mergesort*)
  fun sort [] = []
    | sort [x] = [x]
    | sort xs = let val k = length xs div 2
                in merge(sort(take(k, xs)),
                         sort(drop(k, xs)))
                end

  (* pushLayer: see signature *)
  fun pushLayer (ateffects: effect list, cone as (n,c):cone) : cone =
      let val l = rev((map (fn effect =>
                          case get_level_and_key effect of
                            SOME(level,key) => (level:= n+1;
                                                (! key, effect))
                          | _ => die "pushLayer: atomic effect neither region- nor effect variable")
                              ateffects))
          fun is_sorted [] = true
            | is_sorted [x] = true
            | is_sorted ((i:int,_)::(rest as (j,_)::_)) =
                 i < j andalso is_sorted rest
          val _ = if is_sorted l then () else die "pushLayer: atomic effects not sorted"
          val layer = ConeLayer.fromSortedList l (ConeLayer.mkEmpty())
      in (n+1, Cone.add(n+1, layer, c))
      end


  (* pop topmost layer of cone *)
  fun pop ((n,c):cone): coneLayer * cone =
       if n<=0 then die "pop: Attempt to pop empty cone"
       else
        let val top_layer = noSome(Cone.lookup c n, "pop: no such layer")
        in  (top_layer,
             (n-1, case Cone.remove(n,c) of
                    SOME c' => c'
                  | _ => Crash.impossible ("Pop of cone failed: level = "
                                           ^ Int.toString n)
             ))
        end

  (* pop topmost layer of cone and return those effects of the topmost
     level that still have the level of the topmost level as children of "effect";
     any previous out-edges of "effect" are over-written *)

  fun topLayer ((n,c): cone) : effect list =
        let val top_layer = noSome(Cone.lookup c n, "topLayer: no such layer")
            val atomic_effects = (* the atomic effects in the topmost layer that have not been
                                    lowered to lower levels *)
             sort(
              remove_duplicates(
               List.filter (fn eff => let val l = noSome (level_of eff, "popAndClean")
                                      in l >= n
                                      end)  (ConeLayer.range top_layer)))
        in
          atomic_effects
        end

  fun popAndClean (cone:cone) : effect list  * cone =
      (topLayer cone, #2(pop cone))

  fun max (n,m):int = if n > m then n else m

  local
    val init_count : int option ref = ref NONE    (* 9 (1-9) top-level predefined rhos/eps declared below! *)
    val count = ref 1
    val countEps = ref 1
    fun incn r n = !r before r := !r + n
    fun inc r = !r before r := !r + 1
    val firstRef : int option ref = ref NONE      (* first rho/eps declared in a program unit *)
  in
    fun set_init_count () = (* to be called after declaration of top-level effects below *)
        case !init_count of
            NONE => ( init_count := SOME (!count)
                    ; countEps := !count + 1 )
          | SOME _ => die "init_count already set"

    fun resetCount () = (* to be called before region inference in Compile.sml *)
        case !init_count of
            SOME c =>
                let val first = max (c, regionvarInitial())
                in count := first ; firstRef := SOME first ; countEps := first + 1
                end
          | NONE => die "init_count not set"

    fun freshRhoInt () =
        if Option.isSome (!init_count)
        then incn count 2
        else inc count

    fun freshEpsInt () =
        if Option.isSome (!init_count)
        then incn countEps 2
        else inc count

    fun getCountFirstLast () =
        let val last = max(!count,!countEps)
        in case !firstRef of
            SOME first => (first,last)
          | NONE => die "getCountFirstLast: error"
        end
  end

  (* freshRho(cone): Generate a fresh region variable
     at the topmost layer of   cone   and insert it in
     this topmost layer *)

  local
    fun freshRho0 rvopt (cone:cone as (n, c)) : effect * cone =
        let val key = freshRhoInt()
            val node = mkRho rvopt (n,key)
        in (node, add(node, n, key, cone))
        end
  in
    fun freshRho B = freshRho0 NONE B
    fun freshRhoRegVar (B,rv) = freshRho0 (SOME rv) B
  end

  fun insertRho rho (cone as (n,c)) = add(rho,n, key_of_rho rho, cone)
  fun insertEps eps (cone as (n,c)) = add(eps,n, get_key_of_eps eps, cone)

  fun freshRhos (rhos,c: cone): effect list * cone  =
      foldr (fn (rho,(rhos',c)) =>
                  let val (rho',c) = freshRho c
                  in (rho'::rhos',c)
                  end) ([],c) rhos

  fun rename_rhos_aux (rhos, c: cone as (n,_), f, g) : effect list * cone =
      foldr (fn (rho,(rhos',c)) =>
                  case G.find_info rho of
                    RHO{level,pix,ty,protected=ref prot,...} =>
                     let val k = freshRhoInt()
                         val new_rho =
                             G.mk_node(RHO{key = ref k, level = ref(g level),
                                           put = NONE, get = NONE, instance = ref NONE,
                                           pix = ref(f pix), ty = ty, explicit=NONE,
                                           protected=ref prot})
                     in
                        (new_rho::rhos', add(new_rho, n, k, c))
                     end
                   | _ => (log_string"renameRhos: expected region variable, found:\n";
                           log_tree(layout_effect_deep rho);
                           die "renameRhos: not a region variable")
                  ) ([],c) rhos

  fun renameRhos (rhos, c: cone as (n,_)) : effect list * cone =
      rename_rhos_aux(rhos, c, fn (ref i) => i, fn (ref i) => i)

  fun cloneRhos (rhos, c: cone as (n,_)) : effect list * cone =
      rename_rhos_aux(rhos, c, fn(ref _) => ~1, fn _ => n)

  fun rename_epss_aux (epss, c: cone as (n,_), f, g) : effect list * cone =
      foldr (fn (eps,(epss',c)) =>
                  case G.find_info eps of
                    EPS{level,pix,explicit,(*represents = NONE,*)...} =>
                     let val k = freshEpsInt()
                         val new_eps =
                             G.mk_node(EPS{key = ref k, level = ref(g level),
                                           instance = ref NONE,
                                           represents = NONE,
                                           pix = ref(f(pix)),
                                           explicit=explicit})
                     in
                        (new_eps::epss', add(new_eps, n, k, c))
                     end
                   | _ => (log_string"renameEpss: expected effect variable, found:\n";
                           log_tree(layout_effect_deep eps);
                           die "renameEpss: not an effect variable")
                  ) ([],c) epss
  fun renameEpss (epss, c: cone as (n,_)) : effect list * cone =
      rename_epss_aux(epss,c,fn(ref int) => int, fn(ref int) => int)

  fun cloneEpss (epss, c: cone as (n,_)) : effect list * cone =
      rename_epss_aux(epss,c,fn(ref int) => ~1, fn _ => n)

  fun freshRhoWithTy (rt: runType, cone:cone as (n, c)): effect * cone =
      let val key = freshRhoInt()
          val node =G.mk_node(RHO{key = ref key, level = ref n,
                                  put = NONE, get = NONE, instance = ref NONE, pix = ref ~1, ty = rt,
                                  explicit=NONE,protected=ref 0})
        in (node, add(node, n, key, cone))
      end

  fun freshRhosPreserveRT (rhos,c: cone): effect list * cone  =
      foldr (fn (rho,(rhos',c)) =>
                (case get_place_ty rho of
                     NONE => die "freshRhosPreserveRT"
                   | SOME rt =>
                     let val (rho',c) = freshRhoWithTy(rt, c)
                     in (rho'::rhos',c)
                     end)) ([],c) rhos

  fun setRunType (place:place) (rt: runType) : unit =
      case G.find_info place of
          RHO{put,get,key,level,instance,pix,ty,explicit,protected} =>
          G.set_info place (RHO{put=put,get=get,key=key,level=level,instance=instance,
                                pix=pix,ty=rt,explicit=explicit,protected=protected})
        | _ => die "setRunType: node is not a region variable"

  fun getRegVar (place:place) : RegVar.regvar option =
      case G.find_info place of
          RHO{explicit,...} => explicit
        | EPS{explicit,...} => explicit
        | _ => die "getRegVar: node is not a region variable or an effect variable"

  (* freshEps(cone): Generate a fresh effect variable
     at the topmost layer of   cone   and insert it in
     this topmost layer *)

  local
    fun freshEps0 rvopt (cone:cone as (n,c)): effect * cone =
        let val key = freshEpsInt()
            val node = mkEps rvopt (n,key)
        in (node, add(node, n, key, cone))
        end
  in
    fun freshEpsRegVar (B,rv) = freshEps0 (SOME rv) B
    fun freshEps B = freshEps0 NONE B
  end

  fun freshEpss (epss, c: cone): effect list * cone =
      foldr (fn (eps,(epss',c)) =>
                let val (eps',c) = freshEps c
                in (eps'::epss',c)
                end) ([],c) epss

  fun freshRhoEpsRegVar (B,rv) =
      if RegVar.is_effvar rv then freshEpsRegVar(B,rv)
      else freshRhoRegVar(B,rv)

  (* Toplevel regions and arrow effect *)

  local

    (* Atomic effects put(r) and get(r) are memorized for each r (see
     * the definitions of mkPut and mkGet)
     *)
  in

    val (toplevel_region_withtype_top, initCone) = freshRhoWithTy(TOP_RT,push emptyCone)   (*1*)
    val (toplevel_region_withtype_bot, initCone) = freshRhoWithTy(BOT_RT,push emptyCone)   (*2*)
    val (toplevel_region_withtype_string, initCone) = freshRhoWithTy(STRING_RT,initCone)   (*3*)
    val (toplevel_region_withtype_pair, initCone) = freshRhoWithTy(PAIR_RT,initCone)       (*4*)
    val (toplevel_region_withtype_array, initCone) = freshRhoWithTy(ARRAY_RT,initCone)     (*5*)
    val (toplevel_region_withtype_ref, initCone) = freshRhoWithTy(REF_RT,initCone)         (*6*)
    val (toplevel_region_withtype_triple, initCone) = freshRhoWithTy(TRIPLE_RT,initCone)   (*7*)
    val (toplevel_arreff, initCone) = freshEps initCone                                    (*8*)
    val _ = set_init_count()

    val toplevel_effects = [toplevel_region_withtype_top, toplevel_region_withtype_bot,
                            toplevel_region_withtype_string,
                            toplevel_region_withtype_pair, toplevel_region_withtype_array,
                            toplevel_region_withtype_ref, toplevel_region_withtype_triple,
                            toplevel_arreff]
  end

  val toplevel_puts_and_gets =
      let val toplevel_rhos = [toplevel_region_withtype_top, toplevel_region_withtype_bot,
                               toplevel_region_withtype_string,
                               toplevel_region_withtype_pair, toplevel_region_withtype_array,
                               toplevel_region_withtype_ref, toplevel_region_withtype_triple]
          val puts = map mkPut toplevel_rhos
          val gets = map mkGet toplevel_rhos
          val puts_and_gets = puts@gets
      in app (fn to => edge(toplevel_arreff,to)) puts_and_gets
       ; puts_and_gets
      end

  val region_inference = Flags.is_on0 "region_inference"

  fun maybeFreshRhoWithTy (p as (rt,cone)) =
    if region_inference() then freshRhoWithTy p
    else case rt of
             TOP_RT => (toplevel_region_withtype_top,cone)
           | BOT_RT => (toplevel_region_withtype_bot,cone)
           | STRING_RT => (toplevel_region_withtype_string,cone)
           | PAIR_RT => (toplevel_region_withtype_pair,cone)
           | ARRAY_RT => (toplevel_region_withtype_array,cone)
           | REF_RT => (toplevel_region_withtype_ref,cone)
           | TRIPLE_RT => (toplevel_region_withtype_triple,cone)

  val freshRhoWithTy = fn p => freshRhoWithTy p (*maybeFreshRhoWithTy p *)

  fun toplevelRhoFromTy rt : effect =
      case rt of
          TOP_RT => toplevel_region_withtype_top
        | BOT_RT => toplevel_region_withtype_bot
        | STRING_RT => toplevel_region_withtype_string
        | PAIR_RT => toplevel_region_withtype_pair
        | ARRAY_RT => toplevel_region_withtype_array
        | REF_RT => toplevel_region_withtype_ref
        | TRIPLE_RT => toplevel_region_withtype_triple

  fun setInstance (node,node') =  (* see explanation in signature *)
      get_instance node := SOME node'

  fun clearInstance (node,_) = get_instance node := NONE

  (* Picklers *)
  val pu_intref = Pickle.refOneGen Pickle.int

  val pu_runType =
      Pickle.enumGen ("Effect.runType",
                      [STRING_RT, PAIR_RT, TOP_RT, BOT_RT,
                       ARRAY_RT, REF_RT, TRIPLE_RT])

  val pu_runTypes = Pickle.listGen pu_runType

  fun maybeNewHashInfo i =
      case i of
          PUT => NONE
        | GET => NONE
        | UNION _ => NONE
        | RHO {key=ref k,...} => SOME k
        | EPS {key=ref k,...} => SOME k

  val pu_node_nodes : einfo Pickle.pu -> einfo G.node Pickle.pu * einfo G.node list Pickle.pu =
      let fun key_effect e =
              case get_key_of_eps_or_rho e of
                  SOME i => if i <> 0 then i else die "pu_node"
                | NONE => 0  (* could be optimized! *)
      in Pickle.cache2 "Effect.node_nodes"
          (G.pu {maybeNewHashInfo=maybeNewHashInfo,dummy=PUT,
                 register=Pickle.registerEq eq_effect key_effect "pu_node"
                                            (toplevel_effects@toplevel_puts_and_gets)})
      end

  val pu_represents : einfo Pickle.pu -> einfo G.node list option Pickle.pu =
      Pickle.cache "Effect.rep"
      (Pickle.nameGen "Effect.represents" o Pickle.optionGen o #2 o pu_node_nodes)

  val pu_nodeopt : einfo Pickle.pu -> einfo G.node option Pickle.pu =
      Pickle.cache "Effect.nodeopt"
      (Pickle.optionGen o #1 o pu_node_nodes)

  val pu_einfo =
      let fun toInt (EPS _) = 0
            | toInt (UNION _) = 1
            | toInt PUT = 2
            | toInt GET = 3
            | toInt (RHO _) = 4
          fun fun_EPS pu_einfo =
              Pickle.newHash (fn EPS {key=ref k,...} => k | _ => die "pu_einfo.newHash.EPS")
              (Pickle.con1 (fn ((k,l,r),p,y) => EPS{key=k,level=l,represents=r,instance=ref NONE,pix=p,explicit=y})
               (fn EPS{key=k,level=l,represents=r,instance=ref NONE,pix=p,explicit=y} => ((k,l,r),p,y)
                 | _ => die "pu_einfo.fun_EPS")
               (Pickle.tup3Gen0(Pickle.tup3Gen0(pu_intref,pu_intref,pu_represents pu_einfo),
                                pu_intref,
                                Pickle.optionGen RegVar.pu)))
          fun fun_UNION pu_einfo =
              Pickle.con1 (fn r => UNION{represents=r})
              (fn UNION {represents=r} => r
                | _ => die "pu_einfo.fun_UNION")
              (pu_represents pu_einfo)
          val fun_PUT = Pickle.con0 PUT
          val fun_GET = Pickle.con0 GET
          fun fun_RHO pu_einfo =
              Pickle.newHash (fn RHO {key=ref k,...} => k | _ => die "pu_einfo.newHash.RHO")
              (Pickle.con1 (fn ((k,p,g,l),px,t,(y,protected)) => RHO {key=k,put=p,get=g,level=l,
                                                                      instance=ref NONE,pix=px,ty=t,explicit=y,
                                                                      protected=protected})
               (fn RHO {key=k,put=p,get=g,level=l,instance=ref NONE,pix=px,ty=t,explicit=y,protected} =>
                (((k,p,g,l),px,t,(y,protected)))
                 | _ => die "pu_einfo.fun_RHO")
               (Pickle.tup4Gen0(Pickle.tup4Gen0(pu_intref, Pickle.nameGen "put" (pu_nodeopt pu_einfo),
                                                Pickle.nameGen "get" (pu_nodeopt pu_einfo),
                                                pu_intref),
                                pu_intref,pu_runType,Pickle.pairGen0(Pickle.optionGen RegVar.pu,pu_intref))))
      in Pickle.dataGen("Effect.einfo",toInt,[fun_EPS, fun_UNION, fun_PUT, fun_GET,
                                              fun_RHO])
      end

  val (pu_effect, pu_effects) = pu_node_nodes pu_einfo

  (******************************************************)
  (*     computing effect increments during algorithm R *)
  (******************************************************)

  val algorithm_R = ref false

  datatype delta_phi = Lf of effect list | Br of delta_phi * delta_phi

  (* for profiling *)
  fun szDelta (Lf _) = 1
    | szDelta (Br(d1,d2)) = szDelta d1 + szDelta d2

  fun delta_plus (d1,d2) =
      let fun loop (Lf xs, acc) = loops(xs,acc)
            | loop (Br (d1,d2), acc) = loop(d2,loop(d1,acc))
          and loops (nil,acc) = acc
            | loops (x::xs,acc) =
              let val r = G.find_visited x
              in loops(xs, if !r then acc else (r:=true;x::acc))
              end
          val unique_nodes = loop(Br(d1,d2),nil)
      in List.app (fn x => G.find_visited x := false) unique_nodes
       ; Lf unique_nodes
      end

  structure PlaceOrEffectMap =
      OrderFinMap(struct type t = effect
                         val lt = lt_eps_or_rho
                  end)

  structure Increments = PlaceOrEffectMap

  val globalIncs : delta_phi Increments.map ref = ref Increments.empty

  val profGlobalIncs : unit -> unit =
   fn () =>
      if true then ()
      else let val sz = Increments.fold (fn (_,a) => a+1) 0 (!globalIncs)
               val tot = Increments.fold (fn (d,a) => a+szDelta d) 0 (!globalIncs)
           in print("[SZ=" ^ Int.toString sz ^ ";TOT=" ^ Int.toString tot ^ "]")
           end

  fun unvisitDelta (Lf effects) = app G.unvisit_all effects
    | unvisitDelta (Br(d1,d2)) = (unvisitDelta d1; unvisitDelta d2)

  fun update_increment (eff,Lf[]) = ()
    | update_increment (eff,delta_new) =
       if is_arrow_effect eff
       then
        case Increments.lookup (!globalIncs) eff of
          SOME delta => globalIncs:= Increments.add(eff,delta_plus(delta, delta_new),!globalIncs)
        | NONE =>       globalIncs:= Increments.add(eff,delta_new,!globalIncs)
       else ()

  fun key_of_eps_or_rho node =
      case get_key_of_eps_or_rho node of
          SOME k => k
        | _ => die "key_of_eps_or_rho"

  fun computeIncrement delta =
    let fun search' ([],acc) = acc
          | search' (x::xs,acc) = search'(xs,search(x,acc))

        and searchDelta (Lf effects, acc) = search'(effects,acc)
          | searchDelta (Br(d1,d2), acc) = searchDelta(d1,searchDelta(d2,acc))

        and search (n:effect, ns:effect list) : effect list =
          let val r = G.find_visited n
          in if !r then ns
             else (r := true;
                   let val i = G.find_info n
                   in case i of
                          UNION _ =>
                          (* do not include n itself, but search children *)
                          search'(G.out_of_node n, ns)
                        | RHO _ => (* do not include it; a PUT or GET will be
                                      included, when necessary *)
                          ns
                        | PUT => n::ns
                        | GET => n::ns
                        | EPS _ =>
                          search'(G.out_of_node n,
                                  case Increments.lookup (!globalIncs) n of
                                      SOME delta' => searchDelta(delta', n::ns)
                                    | NONE => n::ns
                                 )
                   end)
          end
      in searchDelta(delta,[]) before unvisitDelta delta
      end

  fun current_increment eps =
      case Increments.lookup (!globalIncs) eps of
          SOME delta =>  delta
        | NONE => Lf []

  (*****************************************************)
  (*     unification of region- and effect variables   *)
  (*****************************************************)

  (* lower: See explanation in signature;
     Lower use  a depth-first traversal  of effect.
  *)

  fun lower (newlevel:int) : effect -> cone -> cone =
      let fun low' ([],b) = b
            | low' (x::xs,b) = low'(xs,low(x, b))
          and low (effect, cone:cone) : cone =
              case get_level_and_key effect of
                  SOME (l as ref n, key) =>
                  if newlevel >= n then cone
                  else   (* newlevel < level: lower level *)
                    let val _ = case explicit_var effect of
                                    NONE => ()
                                  | SOME v =>
                                    let open Report infix //
                                        val report0 = case RegVar.get_location_report v of
                                                          SOME rep => rep
                                                        | NONE => Report.null
                                        val kind = if is_rho effect then "region"
                                                   else if is_arrow_effect effect then "effect"
                                                   else die "lower - expecting region or effect variable"
                                        val report = Report.line
                                                         ("Explicit " ^ kind ^ " variable `" ^
                                                          RegVar.pr v ^ " has insufficient scope.")
                                    in raise Report.DeepError (report0 // report)
                                    end
                        val cone' = remove(effect,l,!key,cone) (* take node out of cone *)
                                    handle ? => (print "lower\n"; raise ?)
                        val _  = l:= newlevel
                        val cone'' = add(effect, newlevel, !key,cone')
                                     (* put node back in cone at lower level *)
                    in low' (G.out_of_node effect, cone'')
                    end
                | NONE => (* not EPS or RHO, no level; just lower children *)
                  low'(G.out_of_node effect,cone)
   in fn effect => fn cone => low(effect,cone)
   end

  fun lower_delta level delta B =
      case delta of
          Lf(l: effect list) =>
          foldl (fn (a,b) => lower level a b
                             handle ? => (print "lower_delta\n"; raise ?)) B l
        | Br(d1, d2) => lower_delta level d2 (lower_delta level d1 B)

  fun setminus (l1: effect list, l2: effect list) : effect list =
     (* Computes l1 \ l2;
       First mark all nodes in l2; then select unmarked nodes from l1 (these
       are the result). Finally, unmark all nodes in l2 *)
      ( app (fn node => G.find_visited node:= true) l2;
        List.filter (fn node => not(!(G.find_visited node))) l1
                    before app (fn node => G.find_visited node:= false) l2
      )

  fun say_eps eps = PP.outputTree(say, layout_effect eps, !Flags.colwidth)

  (* update_areff(eps) assumes that the increments recorded for eps have
     level no greater than the level of eps *)

  fun update_areff eps =
      if is_arrow_effect eps
      then case Increments.lookup (!globalIncs) eps of
               SOME delta =>
               let val nodes = computeIncrement delta
                   val to_be_added = setminus(nodes, G.nodes(G.subgraph [eps]))
               in G.add_edges(eps, to_be_added)
               end
             | NONE => ()
      else ()

  fun min_key (key1 as ref i1,key2 as ref i2) =
      if (i1:int) < i2 then key1 else key2

  (* einfo_combine(einfo1, einfo2): this function is used as argument to
     G.union_without_edge_duplication when implementing unification of region-
     and effect variables *)

  fun removeIncr n =
      (globalIncs :=
       (case Increments.remove (n,!globalIncs) of
            SOME m => m
          | NONE => die "removeIncr"))

  fun deepError rv msg =
      let open Report infix //
          val report0 = case RegVar.get_location_report rv of
                            SOME rep => rep
                          | NONE => Report.null
          val report = line msg
      in raise DeepError (report0 // report)
      end

  fun einfo_combine_eps (eps1,eps2)(einfo1,einfo2) = (* assume einfo1 and einfo2
                                                      * have the same level *)
      case (einfo1, einfo2) of
          (EPS{key = key1 as ref k1, represents, instance, pix, explicit=explicit1, ...},
           EPS{key = key2 as ref k2, explicit=explicit2, ...}) =>
          if k1 = k2 then die "einfo_combine_eps: expected keys to be different"
          else (* merge increment information for einfo1 and einfo2 *)
            let val choose1 = case (explicit1,explicit2) of
                                  (SOME ev1, SOME ev2) =>
                                  if RegVar.eq(ev1,ev2)
                                  then k1 < k2
                                  else deepError ev1 ("Cannot unify the explicit effect variables `"
                                                      ^ RegVar.pr ev1 ^ " and `" ^ RegVar.pr ev2)
                                | (SOME _, NONE) => true
                                | (NONE, SOME _) => false
                                | (NONE, NONE) => k1 < k2
            in if choose1 then
                 (if !algorithm_R then
                    case Increments.lookup (!globalIncs) eps2 of
                        SOME delta2 => (update_increment(eps1,delta2);
                                        update_areff eps1 handle _ => die "einfo_combine_eps1";
                                        removeIncr eps2)
                      | NONE => ()
                  else (); einfo1)
               else
                 (if !algorithm_R then
                    case Increments.lookup (!globalIncs) eps1 of
                        SOME delta1 => (update_increment(eps2,delta1);
                                        update_areff eps2 handle _ => die "einfo_combine_eps2";
                                        removeIncr eps1)
                      | NONE => ()
                  else (); einfo2)
            end
        | _ => die "einfo_combine_eps"

  local
      val largest_toplevel_effect_key = 9
      fun aux_combine (op1,op2) =
          case (op1,op2) of
              (_, NONE) => op1
            | (NONE, _) => op2
            | (SOME n1, SOME n2) =>
                  (* n1 and n2 are supposed to be either both PUT nodes
                   or both GET nodes *)
                  (* The resulting node (a PUT/GET) will have only one out-edge,
                   namely to the region variable which n1 points to *)
                  SOME(G.union_left
                       (fn (a,b) =>
                        case (a,b) of
                            (PUT,PUT) => a
                          | (GET,GET) => a
                          | _ => die ("aux_combine: (a,b) = (" ^ PP.flatten1 (layout_einfo a) ^ ", " ^
                                      PP.flatten1 (layout_einfo b) ^ ")\n"))
                       (n1, n2))
  in
      fun einfo_combine_rho (einfo1, einfo2) =  (* assume einfo1 and einfo2
                                                 * have the same level *)
          case (einfo1, einfo2) of
              (RHO{level=l1,put=p1,get=g1,key=k1,instance=instance1,pix=pix1,ty=t1,
                   explicit=explicit1,protected=protected1},
               RHO{level=_,put=p2,get=g2,key=k2,instance=instance2,pix=pix2,ty=t2,
                   explicit=explicit2,protected=protected2}) =>
              if !k1 <> !k2 andalso (!k1 < largest_toplevel_effect_key
                                     andalso !k2 < largest_toplevel_effect_key)
                 orelse !k1 = 2 andalso t2<>BOT_RT
                 orelse !k2 = 2 andalso t1<>BOT_RT
              then die ("illegal unification involving global region(s) " ^
                        Int.toString (!k1) ^ show_runType t1 ^ " / " ^ Int.toString (!k2) ^ show_runType t2)
              else
                let fun lub_check rv1 t1 t2 =
                        case lub_runType0 (t1,t2) of
                            SOME ty => ty
                          | NONE => deepError rv1 ("The explicit region variable `"
                                                   ^ RegVar.pr rv1
                                                   ^ " has type "
                                                   ^ show_runType t1
                                                   ^ " but is expected to have type "
                                                   ^ show_runType t2)
                    val (ty, explicit) =
                        case (explicit1,explicit2) of
                            (SOME rv1,SOME rv2) =>
                            if RegVar.eq(rv1,rv2) then (lub_runType(t1,t2), explicit1) (*memo:merge info*)
                            else deepError rv1 ("Cannot unify the explicit region variables `"
                                                ^ RegVar.pr rv1 ^ " and `" ^ RegVar.pr rv2)
                          | (SOME rv1, _) => (lub_check rv1 t1 t2, explicit1)
                          | (_, SOME rv2) => (lub_check rv2 t2 t1, explicit2)
                          | (NONE, NONE) => (lub_runType(t1,t2), NONE)
                    val protected = if !protected1 > !protected2 then protected1 else protected2
                in RHO{level = l1, put = aux_combine(p1,p2),
                       get = aux_combine(g1,g2), key = min_key(k1,k2),
                       instance = instance1, pix = pix1, ty = ty,
                       explicit=explicit,protected=protected}
                end
             | _ => die "einfo_combine_rho"
  end

  fun mkSameLevel (node1, node2) cone : cone =
       (* node1 and node2 must both be either EPS nodes or RHO nodes *)
      case (level_of node1, level_of node2) of
          (SOME l1, SOME l2) =>
          if l1=l2 then cone
          else if l1<l2 then lower l1 node2 cone
          else (* l1>l2 *)   lower l2 node1 cone
        | _ => die "mkSameLevel: one of the two nodes was not \
                    \an EPS or a RHO node"

  (* unifyNodes f (node1, node2) cone : cone
     First lower node1 and node2 to the same level; then union
     the two nodes using union operator f.

     Even though two nodes are combined, the node that has been
     eliminated is not removed from the cone: it can still be found
     under its old key.
  *)

  fun unifyNodes f (node1, node2) cone : cone =
    if G.eq_nodes(node1,node2) then cone
    else let val cone1 = mkSameLevel(node1, node2) cone
         in f(node1, node2);
            cone1
         end

  fun unifyNodes_no_lowering f (n1, n2) : unit =
      if G.eq_nodes(n1,n2) then ()
      else (f(n1, n2); ())

  (* unifyRho(rho_node1, rho_node2) cone : cone
     First lower rho_node1 and rho_node2 to the same level; then union
     the two nodes (none of which have children)
  *)

  fun checkRho s r : unit =
      if is_rho r then ()
      else die ("checkRho." ^ s ^ ": " ^ PP.flatten1 (layout_effect r))

  fun checkNotRho s r : unit =
      if is_rho r then die ("checkNotRho." ^ s ^ ": " ^ PP.flatten1 (layout_effect r))
      else ()

  fun checkRegVarTypes (rv1,r1) r2 =
      case (get_place_ty r1, get_place_ty r2) of
          (SOME t1, SOME t2) =>
          (case lub_runType0 (t1,t2) of
               SOME _ => ()
             | NONE => deepError rv1 ("Expects a region variable of type "
                                      ^ show_runType t2
                                      ^ " but the explicit region variable `"
                                      ^ RegVar.pr rv1
                                      ^ " has type "
                                      ^ show_runType t1))
        | _ => die "checkRegVars.check_types: expecting region variables"

  fun checkRegVars r1 r2 : unit =
      case (explicit_rho r1, explicit_rho r2) of
          (SOME rv1, SOME rv2) =>
          if RegVar.eq(rv1,rv2) then ()
          else deepError rv1 ("Cannot unify the explicit region variables `"
                              ^ RegVar.pr rv1 ^ " and `" ^ RegVar.pr rv2)
        | (SOME rv1,_) => checkRegVarTypes (rv1,r1) r2
        | (_, SOME rv2) => checkRegVarTypes (rv2,r2) r1
        | (NONE, NONE) => ()

  fun unifyRho (r1,r2) cone : cone =
      (checkRho "unifyRho1" r1;
       checkRho "unifyRho2" r2;
       checkRegVars r1 r2;
       unifyNodes(G.union einfo_combine_rho)(r1, r2) cone)

  fun unifyRho_explicit ((rv,r1),r2) cone : cone =
      (checkRho "unifyRho1" r1;
       checkRho "unifyRho2" r2;
       checkRegVars r1 r2;
       checkRegVarTypes (rv,r1) r2;
       unifyNodes(G.union einfo_combine_rho)(r1, r2) cone)

  fun unifyRho_no_lowering (r1,r2) : unit =
      (checkRho "unifyRho_no_lowering1" r1;
       checkRho "unifyRho_no_lowering2" r2;
       unifyNodes_no_lowering (G.union einfo_combine_rho) (r1,r2))

  fun unifyEps (e1, e2) cone : cone =
      (checkNotRho "unifyEps1" e1;
       checkNotRho "unifyEps2" e2;
       unifyNodes(G.union_without_edge_duplication
                  (einfo_combine_eps(e1,e2))
                  is_union) (e1,e2) cone)

  fun checkEpsVars e1 e2 : unit =
      case (explicit_eps e1, explicit_eps e2) of
          (SOME ev1, SOME ev2) =>
          if RegVar.eq(ev1,ev2) then ()
          else deepError ev1 ("Cannot unify the explicit effect variables `"
                              ^ RegVar.pr ev1 ^ " and `" ^ RegVar.pr ev2)
        | _ => ()

  fun unifyEps_explicit ((rv,e1),e2) cone : cone =
      (checkNotRho "unifyEps_explicit1" e1;
       checkNotRho "unifyEps_explicit2" e2;
       checkEpsVars e1 e2;
       unifyNodes(G.union_without_edge_duplication
                  (einfo_combine_eps(e1,e2))
                  is_union) (e1,e2) cone)

  (*****************************************************)
  (* generic instance of region- and effect variables  *)
  (*****************************************************)

  (* cone' = instNodes(l) cone:

     l is a list of pairs of effects; it represents a substitution with
     domain map #1 l and range map #2 l. Edges are grafted onto the target
     nodes and level of non-generic nodes that are hence
     grafted onto the target nodes
     are lowered to be at most the level of their new parent,
     if their level is higher.
  *)

  fun instNodes l cone = #1(instNodesClever l cone)
  and instNodesClever (l : (effect * effect) list) cone : cone * (effect * delta_phi)list =
    let
      (* bound_to_free_no_transparent nodes: map each non-transparent n
         to itself, if it is not
         in the domain of "subst" and map it to "subst(n)" otherwise;
         do not included transparent n in the result. Special
         case must be taken to map PUT and GET nodes whose arguments
         are in the domain in the substitution to correpsonding
         nodes in the target *)

      (* assumption: no node in "nodes" need be subjected to "find" *)

      fun bound_to_free node =
          case G.find_info node of
              PUT =>
              (case G.out_of_node node of
                   rho_origin :: _ =>
                   (case !(get_instance rho_origin) of
                        SOME node' => (* generic *) SOME(mkPut node')
                      | NONE => (* non-generic *)
                        SOME node
                   )
                 | _ => die "instNodes: put node has no region argument"
              )
            | GET =>
              (case G.out_of_node node of
                   rho_origin :: _ =>
                   (case !(get_instance rho_origin) of
                        SOME node' => (* generic *) SOME(mkGet node')
                      | NONE => (* non-generic *) SOME node
                   )
                 | _ => die "instNodes: get node has no region argument"
                 )
            | UNION _ => NONE (* node not bound *)
            | EPS {instance as ref i,  ...} =>
              (case i of
                   g as SOME n' => (* generic *) g
                 | NONE => (* non-generic*) SOME node
              )
            | RHO{instance as ref i, key,...} => die ("bound_to_free.RHO: " ^
                                                      PP.flatten1 (layout_effect node) ^ "\n")

      fun lower_new_edges (n:effect, new_target_nodes:effect list) cone : cone =
          let val level = noSome (level_of n, "instNodes: no level")
          in foldl (fn (a,b) => lower level a b) cone new_target_nodes
          end handle ? => (print "lower_new_edges\n"; raise ?)

      val targets_and_new_children: (effect * effect list) list =
          G.multi_graft bound_to_free l
    in
      (foldl (fn (a,b) => lower_new_edges a b) cone targets_and_new_children,
       map (fn (target, children) => (target, Lf children)) targets_and_new_children)
    end

  (*************************************************************************************
   * observe(l: int, source: delta_phi, destination: effect): effect list * delta_phi  *
   *************************************************************************************
   * add all PUT, GET and EPS nodes that can be reached from 'source'                  *
   * as children of 'destination', provided they are not already reachable             *
   * from 'destination' and have level at most 'l' (the level of a PUT or GET          *
   * node is the level of the RHO node it has as its sole child).                      *
   * The nodes thus added are also returned delta_phi, whereas the atomic effects      *
   * that are reachable from source and have level l+1 are accumlated in the           *
   * resulting effect list.                                                            *
   *************************************************************************************)

  fun say s = (TextIO.output(TextIO.stdOut, s); TextIO.output(!Flags.log, s))

  fun observeDelta (l: int, source: delta_phi, destination: effect): effect list * delta_phi =
    let
      (*val _ = Profile.profileOn()*)
(*
      val _ = say("\n-----------------\nLEVEL = " ^ Int.toString l ^ "\n")
      val _ = say("SOURCE = ")
      val _ = app (fn source => PP.outputTree(say, layout_effect_deep source, !Flags.colwidth)) sources
      val _ = say("\nDESTINATION = ")
      val _ = PP.outputTree(say, layout_effect_deep destination, !Flags.colwidth)
*)

      (* include_put_or_get(node):  here node is supposed to be
         PUT or a GET node
         with one child, a RHO node. The return value is true if the RHO node
         has level <= l and false otherwise *)

      val r_acc : effect list ref = ref []  (* for accumulating nodes of level > l *)

      fun include_put_or_get node : bool =
        case G.out_of_node node of
          [rho] => (case G.find_info rho of
                      RHO{level as ref l', ...} => l'<=l
                    | _ => die "include_rho: not RHO node")
        | _ => die "include_rho: not precisely one child of PUT or GET node"

      (* collect: see description below *)
      fun collect (l:int, source: delta_phi) =
      let
        fun searchDelta (Lf effects, acc:effect list) : effect list =
            search'(effects,acc)
          | searchDelta (Br(d1,d2), acc) = searchDelta(d2,searchDelta(d1,acc))

        and search' ([],acc) = acc
          | search' (x::xs,acc) = (search'(xs,search(x,acc)))

        and search (n:effect, ns:effect list) : effect list =
          let val r = G.find_visited n
          in if !r then ns
             else (r := true;
                   let val i = G.find_info n
                   in case i of
                          UNION _ =>
                          (* do not include n itself, but search children *)
                          search'(G.out_of_node n,ns)
                        | RHO _ => (* do not include it; a PUT or GET will be
                                      included, when necessary *)
                          ns
                        | PUT  => (if include_put_or_get n then n::ns
                                   else (r_acc:= n :: !r_acc; ns))
                        | GET  => (if include_put_or_get n then n::ns
                                   else (r_acc:= n :: !r_acc; ns))
                        | EPS{level as ref l', ...} =>
                          if l'<=l then
                            (* include it, without examining children *)
                            (*apply G.visit_all (G.out_of_node n);*)
                            n::ns
                          else
                            (* do not include n itself, but search children *)
                            (r_acc:= n :: !r_acc;
                             if false (*!algorithm_R*) then
                               searchDelta(current_increment n,ns)
                             else (* S *)
                               search'(G.out_of_node n,ns)
                            )
                   end
                  )
          end
      in
        searchDelta(source,[])
      end
    in

     (*
     (1) Visit all nodes reachable from 'destination', leaving all
         visited nodes as marked;
     (2) Then traverse nodes reachable from source, collecting those
         nodes that are not reachable from 'destination' (i.e., are not marked)
         and have level at most 'l'. (This search uses the same mark
         in nodes as (1).)  The result is a list l' of nodes of low level.
         As a side-effect, the atomic effects of level > l (i.e., l+1) are
         collected in the reference r.
     (3) Then unmark all visited nodes from 'source' and 'destination'
     (4) append l' to the list of children of destination.
      *)

      G.visit_all destination;                        (* (1) *)
      let val nodes_to_add = collect(l,source)        (* (2) *)
      in
        G.unvisit_all destination;                    (* (3) *)
        unvisitDelta source;                          (* (3) *)
        G.add_edges(destination, nodes_to_add);       (* (4) *)
        (*Profile.profileOff();*)
(*        say("\nDESTINATION AFTER OBSERVE= ");
        PP.outputTree(say, layout_effect_deep destination, !Flags.colwidth);
        (*input(std_in, 1);*)
*)
        (!r_acc, Lf nodes_to_add)
      end
    end

  fun observe x = (observeDelta x; ())

  (* collapse of cycles in effects: *)
  (* all members of the scc must have the same level; otherwise the graph
     was ill-formed in the first place. Therefore we do not lower levels. *)

  (* findPutAndGets(node) : node list;
     find all the Put and Get nodes reachable from node *)

  fun findPutAndGets node =
      List.filter is_put_or_get (G.topsort [node])

  (* sameLists(l1, l2) : bool   returns true if l1 and l2 contain the same elements;
     neither l1 nor l2 contains duplicates
  *)

  fun sameLists (l1,l2) : bool =
    let fun visit l1 = app (fn node => G.find_visited node := true) l1
        fun unvisit ([], acc) = acc
          | unvisit (x::l2',acc) =
             let val r = G.find_visited x
             in if !r then (r:=false; unvisit(l2',acc))
                else unvisit(l2',false)
             end
        fun unvisited ([], acc) = acc
          | unvisited (x::xs, acc) =
             let val r = G.find_visited x
             in if !r then (r:=false; unvisited(xs, false))
                else unvisited(xs, acc)
             end
    in
       visit(l1);       (* (1):mark elements of l1 *)
       unvisit(l2,true) (* (2):check that (1) marked all elements of l2; unmark l2 in the process *)
       andalso unvisited(l1, true) (* (3): check that (2) unmarked all elements of l1; unmark
                                           those that were not unmarked *)
    end

  (* sameEffect(eps1, eps2) cone  returns true iff the same set of PUT and GET nodes
     are reachable from eps1 and eps2 *)

  fun sameEffect (node1, node2) : bool=
      sameLists(findPutAndGets node1, findPutAndGets node2)

  fun einfo_scc_combine (einfo1, einfo2) =
    case (einfo1,einfo2) of
        (UNION _ , _) => einfo2
      | (_, UNION _) => einfo1
      | (EPS {key=ref k1,explicit=explicit1,...},
         EPS {key=ref k2,explicit=explicit2,...}) =>
        let val choose1 = case (explicit1,explicit2) of
                                  (SOME ev1, SOME ev2) =>
                                  if RegVar.eq(ev1,ev2)
                                  then k1 < k2
                                  else deepError ev1 ("Cannot unify the explicit effect variables `"
                                                      ^ RegVar.pr ev1 ^ " and `" ^ RegVar.pr ev2)
                                | (SOME _, NONE) => true
                                | (NONE, SOME _) => false
                                | (NONE, NONE) => k1 < k2
        in if choose1 then einfo1 else einfo2
        end
      | _ => die "einfo_scc_combine: strongly connected\
                 \ component in effect graph contained \
                  \\nnode which was neither an arrow effect nor a union"

  (* arreffs' = contract_effects(arreffs):
     arreffs is a list of nodes, possibly with duplicates.
     arreffs' will not contain duplicates. The nodes in arreffs'
     are the nodes reachable from arreffs, except that strongly
     connnected components of nodes reachable from arreffs have been
     found and have been collapsed.
  *)

  fun subgraph l = G.nodes(G.subgraph l)

  fun contract_effects (arreffs: effect list) : effect list  =
      let val sg = G.subgraph arreffs
          val effs = G.nodes(G.quotient layout_einfo einfo_scc_combine sg);
      in effs
      end

  fun topsort x = G.topsort x

  fun pix node =
      case G.find_info node of
          RHO{pix, ...} => pix
        | EPS{pix, ...} => pix
        | _ => die "pix: cannot take pre-order index of node which is not a region or effect variable"

  fun get_visited node = G.find_visited node (*G.get_visited(G.find node)*)

  fun get_opt l = foldl (fn (opt,acc) =>
                         case opt of SOME t => t::acc | NONE => acc) [] l

  fun layoutEtas (etas: effect list): StringTree list =
       get_opt(map (fn eff => if is_rho eff then
                                if print_regions()
                                then SOME(layout_effect_deep eff)
                                else NONE
                              else if print_effects()
                              then SOME(layout_effect_deep eff)
                              else NONE) etas)

  val reset_cone = Cone.reset
  fun reset () = ((*reset_cone emptyCone;*)
                 (* resetCount(); *)
                 globalIncs:= Increments.empty)

  (* -------------------------------------------------------
   * unify_with_toplevel_rhos_eps(rhos_epss) : unit
   * ----------
   * Unify (eps_nodes@toplevel_nodes) with toplevel eps node and rho nodes
   * with toplevel rhos of corresponding runtypes. Assume
   * all nodes are of top level. The inclusion of toplevel_nodes
   * has the following effect:
     unifies all region variables at level 1 in cone that have runtime type top with r1;
     unifies all region variables at level 1 in cone that have runtime type word with r2;
     ...
     unifies all region variables at level 1 in cone that have runtime type string with r5;
     This is necessary after region inference, since it is possible to introduce level 1
     region variables that are not one of the pre-defined regions r1-r5.
     Example:
     unit1:
          val id: string -> string =
               (fn x => (output(std_out, "a");x)) o(fn x => (output(std_out, "a");x))
     unit2:
          val g= fn s:string => (let val x = "a"^"b"
                                 in fn y => (output(std_out, x); y)
                                 end)

          val result = if true then id else g "c";

     Here "a"^"b" is put in a region which is secondary in the type of g "c". Since
     the types of g "c" and id are unified in the last line, and since id has arrow
     effect e6.{put(r1), get(r1), ..., put(r6), get(r6)}, where all the variables have
     level 1, the region of "a"^"b" is lowered to level 1 without being unified with any
     of the pre-declared global regions. So it is necessary to unify the region of
     "a"^"b" with r4, the global region of type string.

   * ------------------------------------------------------- *)

  fun unify_with_toplevel_effect effect : unit =
    let
      fun union_with toplevel_rho : unit =
          if G.eq_nodes(toplevel_rho,effect) then ()
          else (G.union einfo_combine_rho (toplevel_rho,effect);())
    in if is_arrow_effect effect then
         if G.eq_nodes(toplevel_arreff,effect) then ()
         else (
              (*
               print "unifying with toplevel_arreff:";
               say_eps toplevel_arreff;
               say_eps effect;
               print "\n";
               *)
              G.union_without_edge_duplication
                (einfo_combine_eps(toplevel_arreff,effect))
                is_union (toplevel_arreff,effect);
              (*
               print "toplevel_arreff, effect :";
               say_eps toplevel_arreff;
               say_eps effect; print "\n";
               *)
              ())
      else
        if is_rho effect then
          case get_place_ty effect of
              SOME TOP_RT =>    union_with(toplevel_region_withtype_top)
            | SOME BOT_RT =>    union_with(toplevel_region_withtype_bot)
            | SOME STRING_RT => union_with(toplevel_region_withtype_string)
            | SOME PAIR_RT =>   union_with(toplevel_region_withtype_pair)
            | SOME ARRAY_RT =>  union_with(toplevel_region_withtype_array)
            | SOME REF_RT =>    union_with(toplevel_region_withtype_ref)
            | SOME TRIPLE_RT => union_with(toplevel_region_withtype_triple)
            | NONE => die "unify_with_toplevel_effect.no runtype info"
        else die "unify_with_toplevel_effect.not rho or eps"
    end

  fun unify_with_toplevel_rhos_eps (cone as (n,c),rhos_epss) : cone =
    let val nodes_for_unification =
            rhos_epss @
            ConeLayer.range(noSome(Cone.lookup c 1, (* 1 is the number of the top level *)
                                   "mk_top_level_unique: not top-level in cone"))
    in app unify_with_toplevel_effect nodes_for_unification;
       (* the above side-effects cone; now return it: *)
       cone
  end


  (* restrain: decrease the level of all variables in the topmost
   * layer by one and pop the topmost layer. *)
  fun restrain (B as (n,c) : cone) : cone =
    let val effs = topLayer B
        (* make variables top-level effect variables *)
        val B = unify_with_toplevel_rhos_eps(B,effs)
    in #2(pop B)
    end

  (**************************************)
  (*  for multiplicity inference:       *)
  (**************************************)

  fun key_of_get_or_put node =
      case G.out_of_node node of
          [rho_node] => key_of_rho rho_node
        | _ => die "key_of_get_or_put"

  exception AE_LT

  fun ae_lt (node1, node2) = (* GET > PUT > EPS *)
      case (G.find_info node1, G.find_info node2) of
          (EPS _, EPS _) => get_key_of_eps node1 < get_key_of_eps node2
        | (EPS _, _) => true
        | (PUT, PUT) => key_of_get_or_put node1 < key_of_get_or_put node2
        | (PUT, EPS _) => false
        | _ => raise AE_LT

  local (* sorting of atomic effects *)
    fun merge ([], ys) = ys:effect list
      | merge (xs, []) = xs
      | merge (l as x::xs, r as y:: ys) =
        if ae_lt(x, y) then x::merge(xs, r)
        else y:: merge(l, ys)

    (* sort: top-down mergesort *)
    fun sort [] = []
      | sort [x] = [x]
      | sort xs =
        let val k = length xs div 2
        in merge(sort(take(k, xs)),
                 sort(drop(k, xs)))
        end
  in
     val sort_ae = sort
  end

  (* mk_phi(eps_node): returns list of atomic effects in the effect which has
     eps_node as its primary effect variable. *)

  fun mk_phi eps_node =
      case G.find_info eps_node of
          EPS{represents = SOME l, ...} => l
        | UNION{represents = SOME l} => l
        | PUT  => [eps_node]
        | GET  => []
        | RHO _ => []
        | _ => die "mk_phi"

  fun visit_eps_or_rho node acc =
    let val i = G.find_info node
        val r = G.find_visited node
    in case i of
           EPS _ => (r:=true; r::acc)
         | RHO{put, ...} =>
           (case put of
                NONE => (r:=true; r::acc)
              | SOME n =>
                let val r' = G.find_visited n
                in r:= true; r':=true; r::r'::acc
                end)
         | _ => die "visit_eps_or_rho: neither eps nor rho node"
    end

  fun removeatomiceffects (psi, []) = psi
    | removeatomiceffects (psi: (effect * 'a) list, discharged_basis: effect list): (effect*'a) list =
      (* each member of discharged_basis is either a region variable or an arrow effect;
         now remove from psi all ae:m for which ae takes the form eps in discharged_basis
         or PUT rho or GET rho for rho in discharged_basis:
      *)
      let val refs = foldl (fn (a,b) => visit_eps_or_rho a b) [] discharged_basis
          fun keep (ae,mul): bool = not(!(G.find_visited ae))
      in List.filter keep psi before
         app (fn r => r := false) refs
      end

  (************************************)
  (* after region inference: compute  *)
  (* the sets of atomic effects that arrow effect *)
  (* handles represent. Only arrow effects*)
  (* and PUT effects are included      *)
  (*************************************)

  structure MultiMerge =
    struct
      (* A multi-way merge can be implemented by keeping a heap
         of list of elements to be sorted. The lists in the heap
         are non-empty. The key value of a list is the key value
         of the first element of the list.*)

      fun leq_key (i, j) = ae_lt(i,j) orelse eq_effect(i,j)

      structure HI = struct
        type elem =  effect list
        fun leq (x::_, y::_) = leq_key(x,y)
          | leq _ = die "leq"
        fun layout _ =  die "layout"
      end

      structure Heap = Heap(structure HeapInfo = HI)

      fun merge (ae1, ae2) = ae1
      fun eq (ae1, ae2) = eq_effect(ae1, ae2)

      fun makeHeap ll =
          let fun mkHeap ([], h) = h
                | mkHeap ([]::rest, h) = mkHeap(rest,h)
                | mkHeap (l::rest, h) = mkHeap(rest, Heap.insert l h)
          in mkHeap(ll, Heap.empty)
          end

      fun insert ([], h) = h
        | insert (l, h) = Heap.insert l h

      fun merge_against (min, h) =
          if Heap.is_empty h then [min]
          else case Heap.delete_min h of
                   (l1 as (x1::xs1), h1) =>
                   if eq(min,x1) then
                     if Heap.is_empty h1 then merge(min,x1)::xs1
                     else merge_against(merge(min,x1), insert(xs1, h1))
                   else
                     if Heap.is_empty h1 then min :: l1
                     else min :: merge_against(x1, insert(xs1, h1))
                 | _ => die "merge_against"

       fun merge_all h =
          if Heap.is_empty h then []
          else case Heap.delete_min h of
                   (x1::xs1, h1) => merge_against(x1, insert(xs1,h1))
                 | _ => die "merge_all"

      fun multimerge (ll: HI.elem list) =
          merge_all(makeHeap ll)
    end

  fun insert_into_list (eps,[]) = [eps]
    | insert_into_list (eps, l as eps'::rest) =
        if ae_lt(eps,eps') then eps ::l
        else if eq_effect(eps,eps') then l
        else eps' :: insert_into_list(eps, rest)

  fun check_represents l =  (* check that all members of l are atomic effects*)
      (map (fn n =>
               case G.find_info n of
                   EPS _ => ()
                 | PUT  => ()
                 | GET  => ()
                 | _ => (log_string "check_represents failed on effect:";
                         log_tree(layout_effect_deep n);
                         die "check_represents")) l;
     l)

  fun bottom_up_eval (g : effect list) : unit =
      (*
       * bottom_up_eval g : every EPS or UNION node has a
       * 'represents' fields. bottom_up_eval g sets the represents
       * field of every node n reachable from a node in g
       * to a sorted list of all the PUT and EPS nodes that
       * can be reached from n.

       * The graph is supposed to be acyclic.
       *)
      let
        fun search (n: effect) : effect list  =
          let
            val r = G.find_visited n
          in
            if !r then
              case G.find_info n of
                EPS{represents = SOME l, ...} => insert_into_list(n,l)
              | EPS{represents = NONE, ...} =>
                   (say "broken invariant: bottom_up_eval: cyclic effect: "  ;
                    say_eps n; say "\n";
                    []
                   )
              | UNION{represents = SOME l} => l
              | UNION{represents = NONE} =>
                   (say "broken invariant: bottom_up_eval: cyclic union: "  ;
                    G.layout_graph layout_einfo(G.add_node_to_graph(n,
                                                                    G.mk_graph()));
                    say "\n";
                    []
                   )
              | PUT => [n]
              | GET => []
              | _ => (say "bottom_up_eval: unexpected node(1): "  ;
                      say_eps n; say "\n";
                      []
                     )
            else
              (r:= true;
               case G.find_info n of
                 EPS{represents, key,level,pix,instance,explicit} =>
                   (let
                      val ns = G.out_of_node n
                      val result = MultiMerge.multimerge(map search ns)
                    in
                      G.set_info n (EPS{represents= SOME ((*check_represents*) result),
                                        key=key,level=level,pix =pix,instance=instance,explicit=explicit});
                      insert_into_list(n,result)
                    end)
               | UNION{represents} =>
                   (let
                      val ns = G.out_of_node n
                      val result = MultiMerge.multimerge(map search ns)
                    in
                      G.set_info n (UNION{represents= SOME ((*check_represents*) result)});
                      result
                    end)
               | PUT => [n]
               | GET => []
               | RHO _ => []
              )
          end
      in
        map search g;(* Each node may potentially begin a new tree, so
                             * we have to evaluate for each node. Note however,
                             * that the graph in total is only traversed once,
                             * (ensured by the use of the mark visited)
                             *)
        G.unvisit g
      end

  fun say s = TextIO.output(TextIO.stdOut, s^"\n")

   (* eval_phis(phis): all members of phis must be EPS nodes;
      we now first contract all cycles, then
      do a bottom-up evaluation of the graph *)

  fun eval_phis (phis: effect list) : unit =
      let val nodes = contract_effects(phis)
      in bottom_up_eval nodes handle exn =>
          (say "\neval_phis failed; nodes = ";
           say_etas (layoutEtas nodes);
           raise exn)
      end

  fun represents eps =
      case G.find_info eps of
          EPS{represents = SOME l, ...} => l
        | _ => (say "No info for eps\n";
                say_eps eps;
                die ("represents"))
end

(*

functor TestEffect() =
struct
(*$TestEffect:
        Effect DiGraph Flags BasicIO Crash Report PrettyPrint Stack UnionFindPoly
*)
structure BasicIO = BasicIO();
structure Crash = Crash(structure BasicIO = BasicIO);
structure Flags = Flags(structure Crash = Crash);
structure Report = Report(structure BasicIO = BasicIO);
structure PP = PrettyPrint(structure Report = Report
                           structure Crash = Crash
                           structure Flags = Flags);
structure UF = UF_with_path_halving_and_union_by_rank();
structure Stack = Stack();
structure DiGraph = DiGraph(structure UF = UF
                            structure Stack = Stack
                            structure PP = PP
                            structure Flags = Flags
                            structure Crash = Crash)
structure Effect = Effect(structure G = DiGraph
                          structure PP = PP
                          structure Crash = Crash
                          structure Report = Report);
open Effect;

fun pp(t) = PP.flatten1 t
fun etest(label,expected,found) =
 say(label ^ (if expected = found then " OK" else " ****** NOT OK *******" ^
"\n expected: " ^ expected ^
"\n found:    " ^ found));

fun etest'(label,expected,found) = say found;

val mkRho = mkRho NONE

val rho1 = mkRho(5,1)
val rho2 = mkRho(6,2)
val e = mkUnion[mkPut rho1, mkPut rho2]
val s1 = pp(layout_effect_deep e);
val _ = DiGraph.union einfo_combine (rho1, rho2)
val s2 = pp(layout_effect_deep e);


val _ = say "---------------testing cones ---------------------";

val _ = resetCount();
val ec = emptyCone;
val _ = say "the empty cone:";
val _ = say(pp (layoutCone ec));
val _ = say "now creating two region variables at level 0";
val c0 = push ec;
val (rho1,c1) = freshRho(c0)
val (rho2,c2) = freshRho(c1);
val _ = say "now the cone is:";
val _ = say(pp (layoutCone c2));
val _ = say "now pushing new level and creating two effect variables";
val c2' = push c2
val (eps3,c3) = freshEps(c2')
val (eps4,c4) = freshEps(c3);
val _ = say "now the cone is: ";
val _ = say(pp (layoutCone c4));
val _ = say "now lowering e4 to level 1: ";
val c5 = lower 1 eps4 c4;
val _ = say(pp (layoutCone c5));
val _ = say "now creating rho6: ";
val (rho6,c6) = freshRho c5;
val _ = say(pp (layoutCone c6));
val _ = say "now making Put(r6)";
val put_r6 = mkPut rho6;
val _ = say "now inserting edge from eps3 to put(r6): ";
val _ = DiGraph.mk_edge(eps3, put_r6);
val _ = say(pp (layoutCone c6));
val _ = say "now lowering eps3 to level 1, r6 should follow suit ";
val c7 = lower 1 eps3 c6;
val _ = say(pp (layoutCone c7));
val _ = etest("checkpoint 1: ",
              "{level 1={1=r1,2=r2,3=e3(put(r5)),4=e4,5=r5},level 2={}}",
              pp(layoutCone c7));
val _ = say "now popping layer";
val (layer1, c8) = pop c7;
val _ = say "top layer was";
val _ = say(pp(layoutLayer layer1));
val _ = say "remaing cone is";
val _ = say(pp(layoutCone c8));

val _ = say "---------------testing unification of region variables ---------------------";

val _ = resetCount();
val ec = emptyCone;
val c0 = push ec;
val (rho1,c1) = freshRho(c0)
val (rho2,c2) = freshRho(c1);
val _ = say "now the cone is:";
val _ = etest("unifyRho1", "{level 1={1=r1,2=r2}}", (pp (layoutCone c2)));
val _ = say "making put rho1 and put rho2: ";
val (p1,p2) = (mkPut rho1, mkPut rho2);
val _ = say "now the cone is:";
val _ = etest("unifyRho2", "{level 1={1=r1,2=r2}}", pp (layoutCone c2));
val _ = say "unifying rho1 and rho2";
val c3 = unifyRho(rho1,rho2) c2;
val _ = say "now the cone is:";
val _ = say(pp (layoutCone c3));
val _ = say "are rho1 and rho2 now equal (after find)?";
val equal = DiGraph.eq_nodes(DiGraph.find rho1, DiGraph.find rho2);
val _ = etest("unifyRho3", "true", Bool.string equal);

val _ = say "put rho1 has become:";
val _ = etest("unifyRho4", "put(r1)", pp(layout_effect_deep (DiGraph.find p1)));
val _ = say "put rho2 has become:";
val _ = say(pp(layout_effect_deep (DiGraph.find p2)));

val _ = say "---------------testing unification of arrow effects ---------------------";

fun mkRhos 0 (cone,acc) =acc
  | mkRhos n (cone, acc) =
       let val (rho, c') = freshRho(cone)
       in mkRhos(n-1)(c',rho::acc)
       end
fun mkEpss 0 (cone,acc) =acc
  | mkEpss n (cone, acc) =
       let val (rho, c') = freshEps(cone)
       in mkEpss(n-1)(c',rho::acc)
       end;
(* case 1 *)
val _ = resetCount();
val ec = emptyCone;
val c = push ec;
val (eps1,c) = freshEps(c)
val (rho1,c) = freshRho(c)
val _ = edge(eps1,mkPut rho1)
val c = push c
val (eps2,c) = freshEps(c)
val (rho2,c) = freshRho(c)
val _ = edge(eps2,mkPut rho2);
val _ = say "Case 1, before unification of eps1 and eps2: cone is";
val _ = say (pp(layoutCone c));
val c= unifyEps(eps1,eps2)c;
val _ = say "Case 1, after unification of eps1 and eps2: cone is";
val _ = etest("Case 1", "{level 1={1=e1(put(r2),put(r4)),2=r2,3=e1(put(r2),put(r4)),4=r4},level 2={}}",(pp(layoutCone c)));

(* case 2 *)

val _ = resetCount();
val ec = emptyCone;
val c = push ec;
val (eps1,c) = freshEps(c)
val (rho2,c) = freshRho(c)
val p2 = mkPut rho2
val _ = edge(eps1,p2)
val c = push c
val (eps3,c) = freshEps(c)
val _ = edge(eps3,p2);
val _ = say "Case 2, before unification of eps1 and eps3: cone is";
val _ = say (pp(layoutCone c));
val c= unifyEps(eps1,eps3)c;
val _ = say "Case 2, after unification of eps1 and eps3: cone is";
val _ = etest("Case 2", "{level 1={1=e1(put(r2)),2=r2,3=e1(put(r2))},level 2={}}", (pp(layoutCone c)));

(* case 3 *)

val _ = resetCount();
val ec = emptyCone;
val c = push ec;
val (eps1,c) = freshEps(c)
val (rho2,c) = freshRho(c)
val g2 = mkGet rho2
val _ = edge(eps1,g2)
val c = push c
val (eps3,c) = freshEps(c)
val (rho4,c) = freshRho(c)
val _ = edge(eps3,mkPut rho4);
val _ = edge(eps3,eps1);
val _ = say "Case 3, before unification of eps1 and eps3: cone is";
val _ = say (pp(layoutCone c));
val c= unifyEps(eps1,eps3)c;
val _ = say "Case 3, after unification of eps1 and eps3: cone is";
val _ = etest("Case 3" ,
"{level 1={1=e1(get(r2),@e1,put(r4)),2=r2,3=e1(get(r2),@e1,put(r4)),4=r4},level 2={}}",(pp(layoutCone c)));

(* case 4 *)

val _ = resetCount();
val ec = emptyCone;
val c = push ec;
val (eps1,c) = freshEps(c)
val (eps2,c) = freshEps(c)
val (eps3,c) = freshEps(c)
val (rho4,c) = freshRho(c)
val _ = edge(eps1,eps2);
val _ = edge(eps2, eps3);
val _ = edge(eps3, eps1);
val _ = edge(eps3, mkGet(rho4));
val c = push c
val (eps5,c) = freshEps(c)
val (eps6,c) = freshEps c;
val (rho7,c) = freshRho c;
val _ = edge(eps5,eps6)
val _ = edge(eps6,eps5)
val _ = edge(eps5,mkPut(rho7));
val _ = say "Case 4, before unification of eps6 and eps1: cone is";
val _ = say (pp(layoutCone c));
val c= unifyEps(eps6,eps1)c;
val _ = say "Case 4, after unification of eps6 and eps1: cone is";
val _ = etest("Case 4" ,
"{level 1={1=e6(e5(put(r7),@e6),e2(e3(get(r4),@e6))),\
\2=e2(e3(get(r4),e6(e5(put(r7),@e6),@e2))),\
\3=e3(get(r4),e6(e5(put(r7),@e6),e2(@e3))),\
\4=r4,5=e5(put(r7),e6(@e5,e2(e3(get(r4),@e6)))),\
\6=e6(e5(put(r7),@e6),e2(e3(get(r4),@e6))),7=r7},level 2={}}",(pp(layoutCone c)));

val _ = say "-------------- end of test of unification ------------";


fun loop 0 (cone, l) = (cone, l)
  | loop n (cone, l) =
      let val (rho, cone') = freshRho cone
      in loop (n-1)(cone', rho::l)
      end;

val _ = say "now building list of 50000 region variables"
val (big_cone, big_list) = loop 50000 (push ec, []);

val _ = say "ready to unify the 5000 region variables";

fun unified() = foldl (fn (rho1,(cone,rho)) => (unifyRho(rho1,rho)cone,rho1))
                         (big_cone,(hd big_list))
                         big_list;



val _ = say " ---------------- test of instNodes ---------------";
val _ = resetCount();
val c = emptyCone;
val c = push c;
     (* domain of subst: eps2,eps3,rho4 *)
val (rho1,c) = freshRho c
val (eps2,c) = freshEps c
val (eps3,c) = freshEps c
val (rho4,c) = freshRho c
val u = mkUnion [mkPut rho1, eps3];
val _ = edge(eps2, u);
val _ = edge(eps3, mkGet rho4);
    (* range of subst: eps5, eps6, rho7 *)
val (eps5,c) = freshEps c
val (eps6,c) = freshEps c
val (rho7,c) = freshRho c
val _ = edge(eps5,eps6)
val _ = edge(eps5, mkPut rho7)
    (* the substitution (bottom-up listing) *)
val subst = [(rho4,rho7), (eps3,eps6), (eps2,eps5)];
val _ = say "before generic instantiation the cone is: "
val _ = say (pp(layoutCone c));
(*val lp = (DiGraph.multi_graft transparent subst);*)
val c = instNodes subst c;
val _ = say "after generic instantiation the cone is: "
val _ = etest("instNodes, case1",
"{level 1={1=r1,2=e2(U(e3(get(r4)),put(r1))),3=e3(get(r4)),4=r4,5=e5(put(r1),put(r7),e6(get(@r7))),6=e6(get(r7)),7=r7}}",
(pp(layoutCone c)));

val _ = say " ---------------- test of observe ---------------";
fun show_node n = pp(layout_effect_deep(DiGraph.find n))

(* case 1 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (rho2,c) = freshRho c;
val (rho3,c) = freshRho c;
val (rho4,c) = freshRho c;
val (rho5,c) = freshRho c;
val putRho1 = mkPut rho1;
val putRho2 = mkPut rho2;
val putRho4 = mkPut rho4;
val getRho5 = mkGet rho5;
val destination = mkUnion[putRho2, getRho5]
val _ = edge(eps1, putRho2);
val _ = edge(eps1, mkGet rho3);
val c= push c;
val (rho6,c) = freshRho c;
val (eps7,c) = freshEps c
val _ = edge(eps7, putRho4);
val source = mkUnion[eps1,eps7,mkUnion[mkGet rho6, mkPut rho5]];
val _ = say "source before observe(1,source,destination):";
val _ = say (show_node source)
val _ = say "destination before observe(1,source,destination):";
val _ = say (show_node destination)
val destination' = observe(1,[source], destination);
val _ = say "source after observe(1,source,destination):";
val _ = say (show_node source)
val _ = say "result of observe(1,source,destination):";
val _ = etest("observe, Case 1",
"U(e1(get(r3),put(r2)),put(r4),put(r5),get(@r5),@put)",
(show_node destination'));

(* case 2 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (rho2,c) = freshRho c;
val (rho3,c) = freshRho c;
val (rho4,c) = freshRho c;
val (rho5,c) = freshRho c;
val putRho1 = mkPut rho1;
val putRho2 = mkPut rho2;
val putRho4 = mkPut rho4;
val getRho5 = mkGet rho5;
val destination = mkUnion[getRho5]
val _ = edge(eps1, putRho2);
val _ = edge(eps1, mkGet rho3);
val c= push c;
val (rho6,c) = freshRho c;
val (eps7,c) = freshEps c
val _ = edge(eps7, putRho4);
val source = mkUnion[eps1,eps7,mkUnion[mkGet rho6, mkPut rho5]];
val _ = say "source before observe(1,source,destination):";
val _ = say (show_node source)
val _ = say "destination before observe(1,source,destination):";
val _ = say (show_node destination)
val destination' = observe(1,[putRho2,source], destination);
val _ = say "source after observe(1,source,destination):";
val _ = say (show_node source)
val _ = say "result of observe(1,source,destination):";
val _ = etest("observe, Case 2",
"U(e1(get(r3),put(r2)),put(r4),put(r5),get(@r5))",
(show_node destination'));

val _ = say " ---------------- test of sameSetUptoBijection  ---------------";

(* case 1*)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (eps2,c) = freshEps c;
val (rho3,c) = freshRho c;
val (rho4,c) = freshRho c;
val _ = edge(eps1,mkPut rho3)
val _ = edge(eps2,mkPut rho4)
val case1 = sameSetUptoBijection  [(eps1,eps2),(rho3,rho4)](eps1,eps2);
val _ = etest("sameSetUptoBijection, case 1", "true", Bool.string case1);

(* case 2 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (eps2,c) = freshEps c;
val (rho3,c) = freshRho c;
val (rho4,c) = freshRho c;
val _ = edge(eps1,mkPut rho3)
val _ = edge(eps2,mkPut rho4)
val case2 = sameSetUptoBijection  [(eps1,eps2)](eps1,eps2);
val _ = etest("sameSetUptoBijection, case 2", "false", Bool.string case2);

(* case 3 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (eps2,c) = freshEps c;
val (rho3,c) = freshRho c;
val (rho4,c) = freshRho c;
val _ = edge(eps1,mkPut rho3)
val _ = edge(eps2,mkPut rho4)
val case3 = sameSetUptoBijection  [(rho3,rho4)](eps1,eps2);
val _ = etest("sameSetUptoBijection, case 3", "false", Bool.string case3);

(* case 4 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (eps2,c) = freshEps c;
val (rho3,c) = freshRho c;
val p = mkPut rho3
val _ = edge(eps1,p)
val _ = edge(eps2,p)
val case4 = sameSetUptoBijection  [(eps1,eps2)](eps1,eps2);
val _ = etest("sameSetUptoBijection, case 4", "true", Bool.string case4);

(* case 5 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (eps2,c) = freshEps c;
val (rho3,c) = freshRho c;
val p = mkPut rho3
val _ = edge(eps1,p)
val _ = edge(eps2,p)
val _ = edge(eps2, mkGet rho3)
val case5 = sameSetUptoBijection  [(eps1,eps2)](eps1,eps2);
val _ = etest("sameSetUptoBijection, case 5", "false", Bool.string case5);

(* case 6 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (rho2,c) = freshRho c;
val (rho3,c) = freshRho c;
val (eps4,c) = freshEps c;
val p = mkPut rho3
val _ = edge(eps1,mkPut rho2)
val _ = edge(eps1,p)
val _ = edge(eps4, p)
val case6 = sameSetUptoBijection  [(eps1,eps4)](eps1,eps4);
val _ = etest("sameSetUptoBijection, case 6", "false", Bool.string case6);

(* case 7 *)

val _ = resetCount();
val c = emptyCone;
val c = push c;
val (eps1,c) = freshEps c;
val (eps2,c) = freshEps c;
val (rho3,c) = freshRho c;
val (eps4,c) = freshEps c;
val (eps5,c) = freshEps c;
val p = mkPut rho3
val _ = edge(eps1,eps5)
val _ = edge(eps1,p)
val _ = edge(eps2, p)
val _ = edge(eps2, eps4)
val case7 = sameSetUptoBijection  [(eps1,eps4),(eps5,eps2)](eps1,eps2);
val _ = etest("sameSetUptoBijection, case 7", "false", Bool.string case7);

end (*TestEffect*)

*)
