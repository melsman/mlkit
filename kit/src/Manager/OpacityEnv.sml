
(* Environment for Opacity elimination *)
signature OPACITY_ENV =
  sig
    structure TyName : TYNAME
    type opaq_env and funid and realisation

    val from_rea : realisation -> opaq_env
    val from_funid : funid * (TyName.Set.Set * realisation) -> opaq_env
    val rea_of : opaq_env -> realisation
    val lookup_funid : opaq_env -> funid -> (TyName.Set.Set * realisation) option

    val empty : opaq_env
    val initial : opaq_env
    val plus : opaq_env * opaq_env -> opaq_env
    val enrich : opaq_env * (opaq_env * TyName.Set.Set) -> bool
    val restrict : opaq_env * (funid list * TyName.Set.Set) -> opaq_env
    val match : opaq_env * opaq_env -> unit

    type StringTree
    val layout : opaq_env -> StringTree
  end

functor OpacityEnv(structure FunId : FUNID
		   structure Crash : CRASH
		   structure PP : PRETTYPRINT
		   structure Report : REPORT
		   structure Environments : ENVIRONMENTS   (* for TyName and for env and for Realisation *)
		     sharing type Environments.StringTree = PP.StringTree
		     ) : OPACITY_ENV =
  struct
    fun die s = Crash.impossible ("OpacityEnv." ^ s)
    structure TyName = Environments.TyName
    structure Realisation = Environments.Realisation
    type realisation = Environments.realisation
    structure FE = OrderFinMap(structure Order = 
				 struct type T = FunId.funid
				   val lt = fn a => fn b => FunId.<(a,b)
				 end
			       structure PP = PP
			       structure Report = Report)

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
    fun restrict((fe,rea), (funids,T)) = (FE.restrict (fe,funids), Realisation.restrict T rea)
      handle FE.Restrict => die "restrict"
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
  end
