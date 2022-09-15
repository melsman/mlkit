
(* contains multiplicities, multiplicity effects,
   effect environments, multiplicity substitutions, etc*)

signature MUL = sig
  type mulef                    (* multiplicity effects, psi *)
  type mularef                  (* multiplicity arrow effects *)
  type mularefset                   (* multiplicity arrow effect sets *)
  type mularefmap                   (* multiplicity arrow effect maps, Psi *)
  type imp_mularefmap               (* imperative multiplicity arrow effect maps, Psi *)
  type efenv                        (* effect environments *)
  type efsubst                      (* effect substitutions *)
  type qmularefset                  (* quantified multiplicity arrow effect set*)
  type lvar                         (* lambda variables *)
  type place                        (* region variables; from EFFECT *)

  structure Effect: EFFECT
  type effectvar = Effect.effect
  type effect (* = ateff list *)
  type arroweffect (* = effectvar * effect *)
  type ateffect = Effect.effect

  datatype mul = INF | NUM of int   (* multiplicities *)
  val sumef    : mulef*mulef -> mulef
  val sum_psis : mulef list -> mulef
  val maxef    : mulef*mulef -> mulef
  val max_psis : mulef list -> mulef
  val diffef   : mulef*mulef -> mulef
  val timesef  : mul*mulef -> mulef

  (* nf(eps, psi) computes the normal form of the arrow effect (eps, psi);
     normal forms eliminate cycles (making the multiplicities in psi infinite).
     diffef may only be used on multiplicity effects that stem from two arrow
     effects which are both in normal form.
   *)
  val nf: mularef -> mularef

  (* these functions are used in the letregion case of mulinf*)
  val removeatomiceffects        : mulef*Effect.effect list-> mulef
  val getmultiplicities          : mulef*place list -> mul list
  val getmultiplicities_unsorted : mulef*place list -> mul list

  val K: int                (* largest finite multiplicity (e.g., 1 *)
  val empty_psi         : mulef
  val empty_qmularefset : qmularefset
  val empty_efenv       : efenv
  val initial           : efenv

  (* create an effect containing a zero multiplicity put-effect to place
   is used in the ASG-case of mulinf*)
  val putzero            : place ->mulef
  val makezero_muleffect : Effect.effect list -> mulef
  val reify              : mularef ref list -> mularefmap
  val mk_infinite        : mularef -> mularef

  (* the functions create effect with multiplicity one*)

  val put                : place -> mulef
  val get                : place -> mulef
  val efvar              : effectvar->mulef

  (* the functions create effect with multiplicity INF *)

  val putInf             : place -> mulef
  val getInf             : place -> mulef

  val placeof            : qmularefset -> place option
  val makearef           : effectvar * mulef -> mularef
  val makeqmularefset    : place list * effectvar list * imp_mularefmap * place option * Effect.cone
                           -> qmularefset
  val makesubst          : effectvar * mularef -> efsubst
  val un_mularef         : mularef -> effectvar * mulef
  val make_arroweffects  : (Effect.effect * Effect.effect list) list -> arroweffect list (* the identity*)
  val makezero_Phi       : (Effect.effect * Effect.effect list) list -> imp_mularefmap
                           (*i.e.,  arroweffect list -> imp_mularefmap *)
  val lookup_mularefmap  : imp_mularefmap*effectvar -> mularef ref
  val combine            : mularefmap * imp_mularefmap -> imp_mularefmap
  val plus_mularefmap    : mularefmap * mularefmap -> mularefmap
  val empty_mularefmap   : mularefmap
  val initial_mularefmap : mularefmap
  val restrict_mularefmap : mularefmap * effectvar list -> mularefmap
  val enrich_mularefmap  : mularefmap * mularefmap -> bool

  val lookup_efenv       : efenv*lvar->qmularefset ref
  val declare            : efenv*lvar*qmularefset ref -> efenv
  val plus               : efenv * efenv -> efenv
  val restrict_efenv     : efenv * lvar list -> efenv

  type regionStatEnv
  val enrich_efenv : (efenv*regionStatEnv) * (efenv*regionStatEnv) -> bool

  val apply_mulef               : efsubst*mulef -> mulef
  val apply_mularef     : efsubst*mularef -> mularef
  val apply_mularefset  : efsubst*mularefset -> mularefset
  val apply_qmularefset         : efsubst*qmularefset -> qmularefset

  val instantiateRegions    : place list * qmularefset -> qmularefset

  (* inteffect psi converts the multiplicities of psi to integers (INF = 10000)
     Is used when passing the effect of an expression to the interpreter *)
  val inteffect:mulef->(ateffect*int)list

  datatype shared = MULEFF of mulef ref
                  | MULAREFF of mularef ref
                  | MULSCHEME of qmularefset ref

  type dependency_map
  val empty_dep        : dependency_map
  val reset_dep        : unit -> unit
  val lookup_dep       : dependency_map * effectvar -> shared list
  val add_dependency   : dependency_map * effectvar * shared -> dependency_map
  val add_dependencies : dependency_map * shared * Effect.effect list -> dependency_map
  val doSubst          : effectvar*mulef*dependency_map ->unit
    (* doSubst(eps,delta_psi,Psi): if delta_psi is not just a zero increment,
       then increment every shared semantic object in dep(eps)
       by delta_psi *)
  val instantiateeffects : arroweffect list * qmularefset * imp_mularefmap * dependency_map -> unit

  val last_increment : unit -> int (* last time a multiplicity was increased *)

  val mk_init_dependency_map : imp_mularefmap -> dependency_map
  (* pretty printing *)
  type StringTree
  val layout_mul: mul -> StringTree
  val layout_mulef: mulef -> StringTree
  val layout_mularef: mularef -> StringTree
  val layout_mularefset: mularefset -> StringTree
  val layout_mularefmap: mularefmap -> StringTree
  val layout_imp_mularefmap: imp_mularefmap -> StringTree
  val layout_qmularefset: qmularefset -> StringTree
  val layout_subst: efsubst -> StringTree
  val layout_Phi: arroweffect list -> StringTree
  val layout_effect : effect -> StringTree
  val layout_efenv : efenv -> StringTree

  (* Picklers *)
  val pu_efenv      : efenv Pickle.pu
  val pu_mularefmap : mularefmap Pickle.pu
  end
