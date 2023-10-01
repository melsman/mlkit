
signature EFFECT =
sig

  type lvar
  type excon
  type prop

  datatype runType = STRING_RT | PAIR_RT | TOP_RT | BOT_RT
                   | ARRAY_RT | REF_RT | TRIPLE_RT

  val ord_runType  : runType -> int
  val show_runType : runType -> string
(*
  val lub_runType: runType * runType -> runType
*)
  val lub_runType0 : runType * runType -> runType option
  type effect and place sharing type place = effect

  type Report = PrettyPrint.Report

  exception AE_LT                                 (* raised by ae_lt when one of the effects is not atomic *)
  val ae_lt             : effect * effect-> bool  (* compares atomic effects *)
  val lt_eps_or_rho     : effect * effect -> bool (* compares effect variables and region variables *)
  val key_of_eps_or_rho : effect -> int
  val setkey            : (unit -> int) -> effect -> unit
  val empty             : effect
  val eq_effect         : effect * effect -> bool
  val pp_eff            : effect -> string

  val pu_effect         : effect Pickle.pu
  val pu_effects        : effect list Pickle.pu
  val pu_runType        : runType Pickle.pu
  val pu_runTypes       : runType list Pickle.pu

  val removeatomiceffects : (effect * 'a)list * effect list-> (effect * 'a)list
  val get_visited         : effect -> bool ref
  val get_instance        : effect -> effect option ref

  (* setInstance(rho,rho') sets the instance field of  rho
     (a generic variable) to rho', its instance;
     setInstance(eps,eps') sets the instance field of eps to eps'*)

  val setInstance       : effect*effect -> unit

  (* clearInstance(rho,rho') clears the instance field of rho *)
  val clearInstance     : effect * effect -> unit

  val pix               : effect -> int ref

  val set_protect       : effect -> unit
  val set_unprotect     : effect -> unit
  val get_protect       : effect -> bool option

  val remove_duplicates : effect list -> effect list

  val is_arrow_effect   : effect -> bool
  val is_rho            : effect -> bool
  val is_put            : effect -> bool
  val is_get            : effect -> bool
  val is_mut            : effect -> bool
  val rho_of            : effect -> place (* should only be applied to PUT, GET, and MUT nodes *)

  (* acc_rho effect acc conses effect onto acc iff
     acc is a RHO node which has a put effect on it.
     When effect is consed onto acc, its visited field is set.
     (Such a region should not be dropped - see DropRegions.drop_places *)

  val acc_rho           : effect -> effect list -> effect list

  (* mk_phi(eps_node): returns list of atomic effects in the effect which has
     eps_node as its primary effect variable. eps_node must be an effect variable *)

  val mk_phi            : effect -> effect list

  val setminus          : effect list * effect list -> effect list
    (* setminus(l1, l2):
       computes l1 \ l2; assume no duplicates in l1
    *)

  val sameEffect        : effect * effect -> bool

  val level_of          : effect ->  int option

  type cone and coneLayer
  val emptyCone         : cone
  val initCone          : cone  (* contains the two toplevel regions below *)
  val info              : cone -> string

  val toplevelRhoFromTy : runType -> effect

  val toplevel_region_withtype_top    : effect
  val toplevel_region_withtype_bot    : effect
  val toplevel_region_withtype_string : effect
  val toplevel_region_withtype_pair   : effect
  val toplevel_region_withtype_array  : effect
  val toplevel_region_withtype_ref    : effect
  val toplevel_region_withtype_triple : effect
  val toplevel_arreff                 : effect

  val push              : cone -> cone

  val pop               : cone -> coneLayer * cone
      (* (layer, B') = B:  B must contain at least one level. layer is
         the topmost level of B and B' is the rest of B. NOTE: not all
         variables in the range of layer need have level equal to level(B)
         -- unification may have turned some of the members of
         the top level of B into references to variables at lower levels.*)

  val topLayer          : cone -> effect list

  val popAndClean       : cone -> effect list * cone
      (* (etas, B') = popAndClean(B):
         B must contain at least one level.
         etas is the list of region and effect variables from the topmost layer of B
         whose level are level(B) -- unification may have turned some of the members of
         the top level of B into references to variables at lower levels. There are no
         duplicates in etas. B' is B with the topmost layer removed.
      *)

  val pushLayer         : effect list * cone -> cone

     (* B' = pushLayer(etas, B):
        etas is a list of region and effect variables, sorted in descending order (THIS IS IMPORTANT)
        B' is the result of creating a cone layer consisting of the
        variables in etas and pushing the layer onto B.
     *)

  val restrain          : cone -> cone
      (* B' = restrain(B):
         B must contain at least one level.
         Lower (by one) the levels of region and effect variables in the topmost
         layer of B. B' is B with the topmost layer removed.
      *)

  val sort              : effect list -> effect list
     (* sort: sorts list of region and effect variables in descending order *)
  val sort_ae           : effect list -> effect list
     (* sort_ae: sorts atomic effects *)

  val level             : cone -> int

  val resetCount        : unit -> unit (* set initial regionid/effectid to that provided
                                        * on command-line with "-regionvar n". *)
  val getCountFirstLast : unit -> int * int (* used for storing count numbers in MLB/f.rv file
                                             * when "-c -regionvar N" is given as argument to
                                             * mlkit executable; used for region profiling. *)

  val freshRho          : cone -> effect * cone
  val freshRhos         : place list * cone -> place list * cone

  val freshRhosPreserveRT: place list * cone -> place list * cone
  val renameRhos        : place list * cone -> place list * cone (* fresh variables,
                                                                    preserve runtime types and pix *)
  val cloneRhos         : place list * cone -> place list * cone  (* fresh variables,
                                                                     preserve runtime types, pix = ~1 *)
  val freshRhoRegVar    : cone * RegVar.regvar -> effect * cone
  val freshRhoWithTy    : runType * cone -> effect * cone

  val setRunType        : place -> runType -> unit
  val get_place_ty      : place -> runType option

  val getRegVar         : place -> RegVar.regvar option

  val freshEps          : cone -> effect * cone
  val freshEpss         : effect list * cone -> effect list * cone
  val renameEpss        : effect list * cone -> effect list * cone
  val cloneEpss         : effect list * cone -> effect list * cone

  val freshEpsRegVar    : cone * RegVar.regvar -> effect * cone
  val freshRhoEpsRegVar : cone * RegVar.regvar -> effect * cone

  val mkPut             : effect -> effect (* argument must
                                              represent a region variable *)
  val mkGet             : effect -> effect (* argument must
                                              represent a region variable *)
  val mkMut             : effect -> effect (* argument must
                                              represent a region variable *)

  val edge              : effect * effect -> unit
  val mkUnion           : effect list -> effect

  val algorithm_R       : bool ref (* false during S, true during R *)
  val unifyEps          : effect * effect -> cone -> cone
  val unifyRho          : effect * effect -> cone -> cone
  val unifyRho_no_lowering: effect * effect -> unit  (* hack; mael 2002-11-19 *)

  val unifyRho_explicit : (RegVar.regvar * effect) * effect -> cone -> cone
  val unifyEps_explicit : (RegVar.regvar * effect) * effect -> cone -> cone

  val insertEps         : effect -> cone -> cone
  val insertRho         : effect -> cone -> cone

  val unify_with_toplevel_rhos_eps : cone * effect list -> cone  (* assumes effects has top level *)
  val unify_with_toplevel_effect : effect -> unit

  (* [rho_add_constraint r (lvopt,r')] adds a constraint to r saying it cannot be
   * identical to r'; the optional lvar indicates the function with the constraint. *)
  val rho_add_constraint  : effect -> Report * lvar option * effect -> unit
  val rho_get_constraints : effect -> (Report * lvar option * effect) list

  (* [eps_add_prop_constraint e (p,rep,lvopt)] adds a constraint to e saying it has
   * to obey the property p; the optional lvar indicates the function with the
   * constraint. *)
  val eps_add_prop_constraint  : effect -> Report * lvar option * prop -> unit
  val eps_get_prop_constraints : effect -> (Report * lvar option * prop) list

  (* [eps_add_constraint e (rep,lvopt,e',putonly)] adds a constraint
   * to e saying it cannot intersect with e' (up to the putonly boolean); the
   * optional lvar indicates the function with the constraint. *)

  val eps_add_constraint  : effect -> Report * lvar option * effect * bool -> unit
  val eps_get_constraints : effect -> (Report * lvar option * effect * bool) list

  datatype delta_phi = Lf of effect list | Br of delta_phi * delta_phi
  val observe           : int * delta_phi * effect -> unit
  val observeDelta      : int * delta_phi * effect -> effect list * delta_phi

  val update_increment  : effect * delta_phi -> unit
  val update_areff      : effect -> unit

  val computeIncrement  : delta_phi -> effect list
  val current_increment : effect -> delta_phi (* the increment currently associated with an effect variable*)

  val profGlobalIncs    : unit -> unit (* for profiling *)

  val instNodes         : (effect*effect)list -> cone -> cone
  val instNodesClever   : (effect*effect)list -> cone -> cone * (effect * delta_phi)list

  (* contract_effects(effects):
     contract cycles in "effects"; the "effects" are assumed to be
     a list of nodes from a well-formed cone. Thus all nodes on the
     same strongly connected component are assumed to have the same
     level.
  *)
  val contract_effects  : effect list -> effect list

  (* lower newlevel effect cone:   lower the level of  effect
     and all nodes reachable from it to have level at most
     newlevel.  Whenever, a node is lowered,
     it is moved to the lower level in the cone.
  *)

  val lower             : int -> effect    -> cone -> cone
  val lower_delta       : int -> delta_phi -> cone -> cone

  val topsort           : effect list -> effect list
  val subgraph          : effect list -> effect list

  val eval_phis         : effect list -> unit
  val represents        : effect -> effect list

  val reset_cone        : cone -> unit
  val reset             : unit -> unit  (* reset list of effect updates; done once pr module *)

  type StringTree = PrettyPrint.StringTree

  val layout_effect_deep: effect -> StringTree       (* sets and clears visited field *)
  val layout_effect     : effect->StringTree         (* no side-effect *)
  val layoutLayer       : coneLayer -> StringTree    (* sets and clears visited field *)
  val layoutLayerRng    : coneLayer -> StringTree    (* sets and clears visited field *)
  val layoutCone        : cone -> StringTree         (* sets and clears visited field *)
  val layoutEtas        : effect list -> StringTree list (* sets and clears visited field *)

  structure PlaceOrEffectMap : MONO_FINMAP where type dom = effect
end
