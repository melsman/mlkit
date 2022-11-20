
signature MUL_EXP =
  sig

    (* Next language after REGION_EXP is MUL_EXP:
       every node in the abstract syntax tree has been annotated by an effect
       (now represented as a list of atomic effects) and an updatable
       multiplicity effect. Multiplicity inference side-effects these references,
       so it does not have to reconstruct the abstract syntax tree

       MUL_EXP is the intermediate language for:

       1) Multiplicity Inference (MUL_INF)
       2) Storage Mode Analysis (AT_INF)
       3) Drop Region Analysis (DROP_REGIONS)
       4) Physical Size Inference (PHYS_SIZE_INF)

       The next language after MUL_EXP is KAM (COMPILE_LAMBDA)
    *)

    type lvar
    type con
    type excon
    type TyName
    type place and effect and mul and efenv
    type effectvar = effect
    type StringTree

    eqtype tyvar

    type Type and mu and sigma and il and cone and constructorKind and
         datbinds and metaType and ateffect and mulef and mularef and
         mularefmap and dependency_map and qmularefset and regionStatEnv

    type lvarset
    type liveset = lvarset * excon list

    structure RegionExp: REGION_EXP
      where type lvar = lvar
      where type con = con
      where type excon = excon
      where type place = place
      where type TyName = TyName
      where type effect = effect
      where type tyvar = tyvar
      where type sigma = sigma
      where type il = il
      where type cone = cone
      where type StringTree = StringTree
      where type metaType = metaType
      where type datbinds = datbinds
      where type Type = Type
      where type mu = mu

    datatype callKind = JMP      (* tail call     to fix-bound function *)
                      | FUNCALL  (* non-tail call to fix-bound function *)
                      | FNJMP    (* tail call     to non-fix-bound function *)
                      | FNCALL   (* non-tail call to non-fix-bound function *)

    datatype saveRestore = NOT_YET_DETERMINED
                         | SR of {store: lvar list, fetch: lvar list}

    datatype ('a,'b,'c) LambdaPgm = PGM of
                        {expression: ('a,'b,'c)trip,
                         export_datbinds: datbinds,
                         import_vars: (lvar list * excon list * place list) option ref,
                         export_vars: lvar list * excon list * place list,
                         export_basis: ateffect list,  (* list of region variables and arrow effects *)
                         export_Psi:   mularef ref list
                        }


      (* list of mutual recursive datatype declarations *)

    and ('a,'b,'c)trip = TR of ('a,'b,'c)LambdaExp * metaType * ateffect list * mulef ref

    and ('a,'b,'c)LambdaExp =
        VAR      of {lvar: lvar, il : il, plain_arreffs: (effectvar * ateffect list) list,
                     fix_bound: bool, rhos_actuals: 'a list ref, other: 'c}
      | INTEGER  of IntInf.int * Type * 'a option
      | WORD     of IntInf.int * Type * 'a option
      | STRING   of string * 'a
      | REAL     of string * 'a (* reals are represented as strings
                                 * for the precision to be preserved. *)
      | F64      of string      (* f64s are represented as strings
                                 * for the precision to be preserved. *)
      | UB_RECORD of ('a,'b,'c) trip list (* unboxed records *)

      | FN       of {pat : (lvar * mu) list,
                     body : ('a,'b,'c)trip,
                     free: (lvar list * excon list * place list) option ref,
                     alloc: 'a}

      | LETREGION of {B: effect list ref,  (* contains both region variables and arrow effects *)
                      rhos: 'b list ref,   (* LETREGION-bound region variables *)
                      body: ('a,'b,'c)trip}

      | LET      of {k_let: bool,
                     pat : (lvar * il ref list ref * (tyvar*effectvar option) list *
                                   effect list ref * Type * place option * 'c) list,
                     bind : ('a,'b,'c)trip,
                     scope: ('a,'b,'c)trip}
      | FIX      of {free: (lvar list * excon list * place list) option ref,
                     shared_clos: 'a,
                     functions : {lvar : lvar,
                                  occ : il list,                        (* instantiation lists              *)
                                                                        (* at non-binding occurrences of il *)
                                  tyvars : (tyvar*effectvar option) list,   (* original *)
                                  rhos: place list,                         (* region   *)
                                  epss: effect list,                        (* type     *)
                                  Type : Type,                              (* scheme.  *)
                                  rhos_formals: 'b list ref,
                                  bound_but_never_written_into: 'b list option, (* set by DropRegions; used by CompLamb*)
                                  other:  'c,
                                  bind : ('a,'b,'c)trip} list,
                     scope : ('a,'b,'c)trip}

      | APP      of callKind option * saveRestore * ('a,'b,'c)trip * ('a,'b,'c)trip

      | EXCEPTION of excon * bool * mu * 'a * ('a,'b,'c)trip
                             (* mu: of exception constructor
                                bool: true if exception is nullary *)
      | RAISE    of ('a,'b,'c)trip
      | HANDLE   of ('a,'b,'c)trip * ('a,'b,'c)trip
      | SWITCH_I of {switch: ('a,'b,'c,IntInf.int) Switch, precision: int}
      | SWITCH_W of {switch: ('a,'b,'c,IntInf.int) Switch, precision: int}
      | SWITCH_S of ('a,'b,'c,string) Switch
      | SWITCH_C of ('a,'b,'c,con)    Switch
      | SWITCH_E of ('a,'b,'c,excon)  Switch
      | CON0     of {con : con, il : il, aux_regions: 'a list, alloc: 'a option}
      | CON1     of {con : con, il : il, alloc: 'a option} * ('a,'b,'c)trip
      | DECON    of {con : con, il : il} * ('a,'b,'c)trip
      | EXCON    of excon * ('a * ('a,'b,'c)trip) option     (* nullary excons are looked up in dyn env. *)
      | DEEXCON  of excon * ('a,'b,'c)trip
      | RECORD   of 'a option * ('a,'b,'c)trip list
      | SELECT   of int * ('a,'b,'c)trip
      | DEREF    of ('a,'b,'c)trip
      | REF      of 'a * ('a,'b,'c)trip
      | ASSIGN   of ('a,'b,'c)trip * ('a,'b,'c)trip
      | DROP     of ('a,'b,'c)trip
      | EQUAL    of {mu_of_arg1: mu, mu_of_arg2: mu} * ('a,'b,'c)trip * ('a,'b,'c)trip
      | CCALL    of {name : string,
                     mu_result : mu, (*mu of result from c function*)
                     rhos_for_result : ('a * int option) list}
                    * ('a,'b,'c)trip list  (* Calling C functions *)

        (*`rhos_for_result' is technical but needed in PhysSizeInf, MulInf &
         CompLamb.  Roughly it is the rho arguments to the c function as
         described in the documentation in the chapter `Calling C Functions'.
         So do not change the order if you tamper with this list!
         `rhos_for_result' is needed in PhysSizeInf, MulInf & CompLamb.  It
         is a list of pairs (rho, i_opt), where rho is a region variable in
         the return type for the c function `name'.  `i_opt' describes how
         `name' will allocate in rho.  `Some 0' means that `name' will put an
         unboxed type in rho (e.g., bool or unit).  `Some i' means that
         `name' will allocate i words in rho.  `None' means that `name' may
         allocate unboundedly in rho.  rhos with a tyvar as tau are not in
         the list.  I think it would be wrong if c_function_effects removed
         duplicates (using Eff.remove_duplicates), because we may want the
         same region passed to the c function more than once if it simply
         happens to be used for more than one thing.*)

      | BLOCKF64 of 'a * ('a,'b,'c)trip list
      | SCRATCHMEM of int * 'a

      | EXPORT   of {name : string,
                     mu_arg : mu, (*mu of argument for c function*)
                     mu_res : mu}
                    * ('a,'b,'c)trip

      | RESET_REGIONS of {force: bool, liveset: liveset option, regions_for_resetting: 'a list}
                          * ('a,'b,'c)trip              (* for programmer-directed resetting of regions;  *)
                                                        (* resetting is forced iff "force" is true.       *)
                                                        (* Forced resetting is not guaranteed to be sound *)
      | FRAME    of {declared_lvars: {lvar : lvar,
                                      sigma : sigma,
                                      other : 'c,
                                      place : place option} list,
                     declared_excons: (excon * mu option) list}
                       (* a frame is the result of a structure-level
                        * declaration.
                        *)

    and ('a,'b,'c,'d) Switch = SWITCH of ('a,'b,'c)trip *
                                      ('d * ('a,'b,'c)trip) list * ('a,'b,'c)trip option



    val warn_puts: regionStatEnv * (place,'b,'c) LambdaPgm -> unit

    val warn_dangling_pointers: regionStatEnv * ('place,'b,'c) LambdaPgm * ('place -> place)-> unit

    (* (mulexp, dep') = mk_initial_mulexp(regexp, dep) :
       dep is a dependency map which gives dependencies between semantic objects.
       After the call, mulexp is a multiplicity expression obtained from
       regexp by inserting multiplicity effects whose domains are the effects of
       regexp but whose multiplcities are zero; dep' is dep extended by
       dependencies involving the semantic objects that were created in the process. *)

    val mk_initial_mulexp: efenv * (place,unit)RegionExp.trip * dependency_map->
                        (place,place*mul,qmularefset ref)trip * dependency_map

    val k_normPgm: (string * ('_a,'b,'_c)trip -> unit) -> '_c ->
                   ('_a, 'b, '_c)LambdaPgm -> ('_a, 'b, '_c)LambdaPgm
    val k_evalPgm: ('a, 'b, 'c)LambdaPgm -> ('a, 'b, 'c)LambdaPgm

    (* k_evalPgm o k_normPgm = I *)

    val test_knorm: (string * ('_a,'b,'_c)trip -> unit) ->
                    (('_a,'b,'_c)LambdaExp * ('_a,'b,'_c)LambdaExp -> unit) ->
                    '_c -> ('_a,'b,'_c)LambdaPgm -> bool

    val appConvert: ('b -> bool) ->
                    ('b list * 'a list -> bool) ->
                    ('b list * 'a -> 'b list) ->
                    ('a,'b,'c)LambdaPgm -> ('a,'b,'c)LambdaPgm

    val layoutLambdaPgm: ('a -> StringTree option) -> ('a -> StringTree option) ->
                         ('b -> StringTree option) ->
                         ('c -> StringTree option) -> ('a, 'b, 'c)LambdaPgm -> StringTree
    val layoutLambdaExp: ('a -> StringTree option)-> ('a -> StringTree option) ->
                         ('b -> StringTree option) ->
                         ('c -> StringTree option) -> ('a,'b,'c)LambdaExp -> StringTree

    val layoutLambdaTrip: ('a -> StringTree option) -> ('a -> StringTree option) ->
                          ('b -> StringTree option) ->
                          ('c -> StringTree option) -> ('a,'b,'c)trip -> StringTree

    (* Protection inference *)
    structure ProtInf : sig
      type pe
      val empPE    : pe
      val initPE   : pe
      val plus     : pe * pe -> pe
      val enrich   : pe * pe -> bool
      val restrict : pe * lvar list -> pe
      val layoutPE : pe -> StringTree
      val pu_pe    : pe Pickle.pu
      val protInf  : pe -> (place,'a,'b)LambdaPgm -> pe
    end

  end
