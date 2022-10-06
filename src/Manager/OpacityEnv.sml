
(* Environment for Opacity elimination *)
signature OPACITY_ENV =
  sig
    type opaq_env and funid and realisation

    val from_rea : realisation -> opaq_env
    val from_funid : funid * (TyName.Set.Set * realisation) -> opaq_env
    val rea_of : opaq_env -> realisation
    val lookup_funid : opaq_env -> funid -> (TyName.Set.Set * realisation) option

    val empty : opaq_env
    val initial : opaq_env
    val plus : opaq_env * opaq_env -> opaq_env
    val enrich : opaq_env * (opaq_env * TyName.Set.Set) -> bool

    val eq : opaq_env * opaq_env -> bool

    val restrict : opaq_env * (funid list * TyName.Set.Set) -> opaq_env
    val match : opaq_env * opaq_env -> unit

    type StringTree
    val layout : opaq_env -> StringTree

    val pu : opaq_env Pickle.pu
  end

structure OpacityEnv: OPACITY_ENV =
  struct
    structure PP = PrettyPrint
    fun die s = Crash.impossible ("OpacityEnv." ^ s)
    structure Realisation = Environments.Realisation
    type realisation = Environments.realisation
    structure FE = OrderFinMap(struct type t = FunId.funid
				      val lt = fn (a,b) => FunId.<(a,b)
			       end)

    type funenv = (TyName.Set.Set * realisation) FE.map

    type opaq_env = funenv * realisation
    type funid = FunId.funid

    fun from_rea (rea : realisation) : opaq_env = (FE.empty, rea)
    fun from_funid (funid, (T,rea)) = (FE.add(funid,(T,rea),FE.empty), Realisation.Id)
    fun rea_of (_,rea) = rea
    fun lookup_funid (fe,_) funid = FE.lookup fe funid
    val empty = (FE.empty, Realisation.Id)
    val initial = empty
    fun plus ((fe1, rea1),(fe2,rea2)) = (FE.plus(fe1,fe2),Realisation.oo(rea1,rea2))
    fun eq_fe_entry((T1,rea1),(T2,rea2)) = TyName.Set.eq T1 T2 andalso Realisation.eq (rea1,rea2)
    fun enrich ((fe1, rea1),((fe2,rea2),T)) =
      FE.enrich eq_fe_entry (fe1,fe2) andalso Realisation.enrich(rea1,(rea2,T))

    fun eq((fe1,re1),(fe2,re2)) =
	FE.enrich eq_fe_entry (fe1,fe2) andalso FE.dom fe1 = FE.dom fe2
	andalso Realisation.eq(re1,re2)

    fun restrict((fe,rea), (funids,T)) = (FE.restrict (FunId.pr_FunId,fe,funids), Realisation.restrict T rea)
      handle FE.Restrict s => die ("restrict; funid " ^ s ^ " is not in the environment")
    fun match((fe1,rea1),(fe2,rea2)) = Realisation.match(rea1,rea2)

    type StringTree = PP.StringTree
    fun layout_fe_entry (T,rea) =
      PP.NODE{start="[", finish="", childsep=PP.RIGHT "]", indent=1,
	      children=[TyName.Set.layoutSet {start="{",finish="}",sep=","} TyName.layout T,
			Realisation.layout rea]}
    fun layout_fe fe = FE.layoutMap {start="{",finish="}",eq=" -> ",sep=","} (PP.LEAF o FunId.pr_FunId)
      layout_fe_entry fe
    fun layout (fe,rea) =
      PP.NODE{start="OPAQ_ENV(",finish=")",childsep=PP.RIGHT",",indent=1,
	      children=[layout_fe fe, Realisation.layout rea]}

    val pu : opaq_env Pickle.pu =
	Pickle.pairGen(FE.pu FunId.pu (Pickle.pairGen(TyName.Set.pu TyName.pu,
						      Realisation.pu)),
		       Realisation.pu)
  end
