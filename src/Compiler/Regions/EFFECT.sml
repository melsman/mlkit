(*$EFFECT *)

signature EFFECT =
sig

  datatype runType = WORD_RT | STRING_RT | REAL_RT | TOP_RT | BOT_RT
  val is_wordsize: runType -> bool
  val ord_runType: runType -> int
  val show_runType: runType -> string
  val lub_runType: runType * runType -> runType

  type effect and place sharing type place = effect
  val ae_lt: effect * effect-> bool  (* compares atomic effects *)
  exception AE_LT                    (* raised by ae_lt when one of the effects is not atomic *)
  val lt_eps_or_rho: effect * effect -> bool (* compares effect variables and region variables *)
  val key_of_eps_or_rho : effect -> int
  val setkey: (unit -> int) -> effect -> unit
  val empty: effect
  val find: effect -> effect
  val eq_effect: effect * effect -> bool (* nodes not necessarily canonical *)
  val eq_canonical_effect: effect * effect -> bool (* reference equality; 
                                            nodes canonical *)

  val removeatomiceffects: (effect * 'a)list * effect list-> (effect * 'a)list
  val get_visited: effect -> bool ref
  val get_instance: effect -> effect Option ref
  val pix : effect -> int ref
  val remove_duplicates: effect list -> effect list

  val is_arrow_effect: effect -> bool
  val is_rho: effect -> bool
  val is_put : effect -> bool
  val is_get : effect -> bool
  val rho_of : effect -> place (* should only be applied to PUT and GET nodes *)


  (* acc_rho effect acc conses effect onto acc iff
     acc is a RHO node which has a put effect on it.
     When effect is consed onto acc, its visited field is set.
     (Such a region should not be dropped - see DropRegions.drop_places *)

  val acc_rho: effect -> effect list -> effect list

  (* mk_phi(eps_node): returns list of atomic effects in the effect which has
     eps_node as its primary effect variable. eps_node must be an effect variable *)

  val mk_phi : effect -> effect list

  val setminus: effect list * effect list -> effect list
    (* setminus(l1, l2): 
       computes l1 \ l2; assume no duplicates in l1
    *)

  val sameEffect : effect * effect -> bool

  val level_of: effect ->  int Option

  type cone and coneLayer
  val emptyCone: cone 
  val initCone: cone  (* contains the two toplevel regions below *)

  val toplevel_region_withtype_top    : effect
  val toplevel_region_withtype_word   : effect
  val toplevel_region_withtype_bot    : effect
  val toplevel_region_withtype_string : effect
  val toplevel_region_withtype_real   : effect
  val toplevel_arreff : effect

  val push: cone -> cone

  val pop : cone -> coneLayer * cone
      (* (layer, B') = B:  B must contain at least one level. layer is
         the topmost level of B and B' is the rest of B. NOTE: not all
         variables in the range of layer need have level equal to level(B)
         -- unification may have turned some of the members of
         the top level of B into references to variables at lower levels.*)

  val topLayer: cone -> effect list

  val popAndClean : cone -> effect list * cone
      (* (etas, B') = popAndClean(B):
         B must contain at least one level.
         etas is the list of region and effect variables from the topmost layer of B
         whose level are level(B) -- unification may have turned some of the members of
         the top level of B into references to variables at lower levels. There are no
         duplicates in etas. B' is B with the topmost layer removed.
      *)
         
  val pushLayer: effect list * cone -> cone

     (* B' = pushLayer(etas, B):  
        etas is a list of region and effect variables, sorted in descending order (THIS IS IMPORTANT)
        B' is the result of creating a cone layer consisting of the 
        variables in etas and pushing the layer onto B.
     *)

  val sort: effect list -> effect list
     (* sort: sorts list of region and effect variables in descending order *)
  val sort_ae: effect list -> effect list
     (* sort_ae: sorts atomic effects *)

  val level: cone -> int

  val resetCount: 'a -> unit
  val freshRho: cone -> effect * cone
  val freshRhos: place list * cone -> place list * cone
  val renameRhos: place list * cone -> place list * cone (* fresh variables,
                                                         preserve runtime types and pix *)
  val cloneRhos: place list * cone -> place list * cone  (* fresh variables, 
                                                         preserve runtime types, pix = ~1 *)
  val freshRhoWithTy: runType * cone -> effect * cone
  val setRunType: effect -> runType -> unit
  val get_place_ty: effect -> runType Option

  val freshEps: cone -> effect * cone
  val freshEpss: effect list * cone -> effect list * cone
  val renameEpss: effect list * cone -> effect list * cone
  val cloneEpss: effect list * cone -> effect list * cone

  val mkPut: effect -> effect (* argument must 
                                 represent a region variable *)
  val mkGet: effect -> effect (* argument must 
                                 represent a region variable *)

  val edge : effect * effect -> unit
  val mkUnion: effect list -> effect

  val algorithm_R: bool ref (* false during S, true during R *)
  val unifyEps: effect * effect -> cone -> cone
  val unifyRho: effect * effect -> cone -> cone

  val insertEps: effect -> cone -> cone
  val insertRho: effect -> cone -> cone

  val unify_with_toplevel_rhos_eps : effect list -> unit

  datatype delta_phi = Lf of effect list | Br of delta_phi * delta_phi
  val observe: int* delta_phi * effect ->  unit
  val observeDelta: int* delta_phi * effect ->  effect list * delta_phi

  val update_increment: effect * delta_phi -> unit
  val update_areff: effect -> unit


  val computeIncrement: delta_phi -> effect list
  val current_increment: effect -> delta_phi (* the increment currently associated with an effect variable*)

  val instNodes: (effect*effect)list -> cone -> cone
  val instNodesClever: (effect*effect)list -> cone -> cone * (effect * delta_phi)list

  (* contract_effects(effects):
     contract cycles in "effects"; the "effects" are assumed to be
     a list of nodes from a well-formed cone. Thus all nodes on the
     same strongly connected component are assumed to have the same
     level.
  *)
  val contract_effects: effect list -> effect list

  (* lower newlevel effect cone:   lower the level of  effect  
     and all nodes reachable from it to have level at most  
     newlevel.  Whenever, a node is lowered,     
     it is moved to the lower level in the cone.
  *)

  val lower:       int -> effect    -> cone -> cone
  val lower_delta: int -> delta_phi -> cone -> cone

  val topsort: effect list -> effect list
  val subgraph: effect list -> effect list

  val eval_phis: effect list -> unit
  val represents: effect -> effect list

  val reset_cone: cone -> unit
  val reset: unit -> unit  (* reset counter to initial counter *)
  val commit: unit -> unit (* sets the initial counter to the current counter. *)
(*
  val trace: int list ref
  val traceOrderFinMap: unit -> unit
*)

  type StringTree
  val layout_effect_deep: effect -> StringTree
  val layout_effect: effect->StringTree
  val layoutLayer: coneLayer -> StringTree
  val layoutLayerRng: coneLayer -> StringTree
  val layoutCone : cone -> StringTree
  val layoutEtas: effect list -> StringTree list
end	


