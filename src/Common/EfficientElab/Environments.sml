(* Static semantic objects for the Core - Definition v3 page 17
   This functor also provides the Core objects needed by the
   Modules elaborator *)

(* Static semantic objects - Definition v3 page 17 *)

(*$Environments: DEC_GRAMMAR STRID IDENT TYCON TYNAME STATOBJECT
	PRETTYPRINT SORTED_FINMAP FINMAP TIMESTAMP REPORT FLAGS
	LIST_HACKS LIST_SORT CRASH ENVIRONMENTS*)

functor Environments(structure DecGrammar: DEC_GRAMMAR
		     structure StrId: STRID

		     structure Ident: IDENT
		       sharing type Ident.strid = StrId.strid
			   and type Ident.longid = DecGrammar.longid
			   and type Ident.id = DecGrammar.id

		     structure TyCon: TYCON
		       sharing type TyCon.strid = StrId.strid

		     structure TyName: TYNAME
		       sharing type TyName.tycon = TyCon.tycon

		     structure StatObject: STATOBJECT
		       sharing type StatObject.ExplicitTyVar = DecGrammar.tyvar
		       sharing StatObject.TyName = TyName

		     structure PP: PRETTYPRINT
		       sharing type StatObject.StringTree = PP.StringTree

		     structure SortedFinMap: SORTED_FINMAP
		       sharing type SortedFinMap.StringTree = PP.StringTree

		     structure FinMap: FINMAP
		       sharing type FinMap.StringTree = PP.StringTree

		     structure Timestamp: TIMESTAMP

		     structure Report: REPORT
		       sharing type SortedFinMap.Report
				    = FinMap.Report
				    = Report.Report

		     structure Flags: FLAGS
		     structure ListHacks: LIST_HACKS
		     structure ListSort: LIST_SORT
		     structure Crash: CRASH
		    ) : ENVIRONMENTS =
  struct
    fun impossible s = Crash.impossible ("Environments." ^ s)
    fun noSome None s = impossible s
      | noSome (Some x) s = x
    fun map_opt f (Some x) = Some (f x)
      | map_opt f None = None

    (*import from StatObject:*)
    type level             = StatObject.level
    type ExplicitTyVar     = StatObject.ExplicitTyVar
    type TyName            = StatObject.TyName.TyName
    type Type              = StatObject.Type
    type TypeScheme        = StatObject.TypeScheme
    type RecType           = StatObject.RecType
    type TypeFcn           = StatObject.TypeFcn
    type TyVar             = StatObject.TyVar
    type Substitution      = StatObject.Substitution
    type realisation       = StatObject.realisation
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure Substitution = StatObject.Substitution
    structure TypeFcn      = StatObject.TypeFcn
    structure Realisation  = StatObject.Realisation

    (*import from other modules:*)
    type id                = Ident.id
    type longid            = Ident.longid
    type valbind           = DecGrammar.valbind
    type tycon             = TyCon.tycon
    type longtycon         = TyCon.longtycon
    type strid             = StrId.strid
    type longstrid         = StrId.longstrid
    type pat               = DecGrammar.pat
    type ty                = DecGrammar.ty
    type StringTree        = PP.StringTree
    type Report            = Report.Report


(*TODO 02/02/1997 14:04. tho.   move these to StatObject.TyVar:
 Actually these are implemented using TyVar.eq which is
 TyVar.eq_free orelse eq_bound EQ_SIGNIFICANT, whilst those in
 StatObject are implemented using TyVar.eq_free.  I don't know what
 difference this makes, or whether it is intended.*)

      local
    fun memberTyVarSet x set =
      List.exists (fn y => TyVar.eq (x,y)) set
      in
    fun unionTyVarSet(set1, set2) =
	set1 @ 
	List.all 
	  (fn x => not(memberTyVarSet x set1)) set2
      end


    (********
     Syntactic type variables, and 
     function which finds the set of type variables in a ty abstract 
     syntax tree 
     ********)

    local
      open DecGrammar
    in
      fun ExplicitTyVarsTy ty =
	case ty of 
	  TYVARty(_, tyvar) => 
	    EqSet.singleton tyvar
	| RECORDty(_, None) => 
	    EqSet.empty
	| RECORDty(_, Some tyrow) =>  
	    ExplicitTyVarsTyRow tyrow
	| CONty(_, tylist, _) => 
	    List.foldL EqSet.union EqSet.empty (map ExplicitTyVarsTy tylist)
	| FNty(_,ty1,ty2) => 
	    EqSet.union (ExplicitTyVarsTy ty1)
	                (ExplicitTyVarsTy ty2)
	| PARty(_,ty) => 
	    ExplicitTyVarsTy ty

      and ExplicitTyVarsTyRow (TYROW(_,_,ty,tyrowopt)) =
	EqSet.union 
	(ExplicitTyVarsTy ty) 
	(case tyrowopt of
	   None => EqSet.empty
	 | Some tyrow => ExplicitTyVarsTyRow tyrow)
    end

    local
      val ++ = ListHacks.union
      infix ++

      fun unguarded_opt f (Some x) = f x
	| unguarded_opt f (None  ) = []

      fun unguarded_atexp(DecGrammar.RECORDatexp(_,exprow_opt)) =
	  unguarded_opt unguarded_exprow exprow_opt
	| unguarded_atexp(DecGrammar.LETatexp(_,dec,exp)) =
	  unguarded_dec dec ++ unguarded_exp exp
	| unguarded_atexp(DecGrammar.PARatexp(_,exp)) =
	  unguarded_exp exp
	| unguarded_atexp _ = []

      and unguarded_exprow(DecGrammar.EXPROW(_,_,exp,exprow_opt)) =
	  unguarded_exp exp ++ unguarded_opt unguarded_exprow exprow_opt

      and unguarded_exp(DecGrammar.ATEXPexp(_,atexp)) =
	  unguarded_atexp atexp
	| unguarded_exp(DecGrammar.APPexp(_,exp,atexp)) =
	  unguarded_exp exp ++ unguarded_atexp atexp
	| unguarded_exp(DecGrammar.TYPEDexp(_,exp,ty)) =
	  unguarded_exp exp ++ unguarded_ty ty
	| unguarded_exp(DecGrammar.HANDLEexp(_,exp,match)) =
	  unguarded_exp exp ++ unguarded_match match
	| unguarded_exp(DecGrammar.RAISEexp(_,exp)) =
	  unguarded_exp exp
	| unguarded_exp(DecGrammar.FNexp(_,match)) =
	  unguarded_match match
	| unguarded_exp(DecGrammar.UNRES_INFIXexp _) =
	    impossible "unguarded_exp(UNRES_INFIX)"

      and unguarded_match(DecGrammar.MATCH(_,mrule,match_opt)) =
	  unguarded_mrule mrule ++
	  unguarded_opt unguarded_match match_opt

      and unguarded_mrule(DecGrammar.MRULE(_,pat,exp)) =
	  unguarded_pat pat ++
	  unguarded_exp exp

      and unguarded_dec(DecGrammar.EXCEPTIONdec(_,exbind)) =
	  unguarded_exbind exbind
	| unguarded_dec(DecGrammar.LOCALdec(_,dec1,dec2)) =
	  unguarded_dec dec1 ++ unguarded_dec dec2
	| unguarded_dec(DecGrammar.SEQdec(_,dec1,dec2)) =
	  unguarded_dec dec1 ++ unguarded_dec dec2
	| unguarded_dec _ = []

      and unguarded_valbind(DecGrammar.PLAINvalbind(_,pat,exp,valbind_opt)) =
	  unguarded_pat pat ++ unguarded_exp exp ++
	  unguarded_opt unguarded_valbind valbind_opt
	| unguarded_valbind(DecGrammar.RECvalbind(_,valbind)) =
	  unguarded_valbind valbind

      and unguarded_exbind(DecGrammar.EXBIND(_,_,ty_opt,exbind_opt)) =
	  unguarded_opt unguarded_ty ty_opt ++
	  unguarded_opt unguarded_exbind exbind_opt
	| unguarded_exbind(DecGrammar.EXEQUAL(_,_,_,exbind_opt)) =
	  unguarded_opt unguarded_exbind exbind_opt

      and unguarded_atpat(DecGrammar.RECORDatpat(_,patrow_opt)) =
	  unguarded_opt unguarded_patrow patrow_opt
	| unguarded_atpat(DecGrammar.PARatpat(_,pat)) =
	  unguarded_pat pat
	| unguarded_atpat _ = []

      and unguarded_patrow(DecGrammar.DOTDOTDOT(_)) = []
	| unguarded_patrow(DecGrammar.PATROW(_,_,pat,patrow_opt)) =
	  unguarded_pat pat ++
	  unguarded_opt unguarded_patrow patrow_opt

      and unguarded_pat(DecGrammar.ATPATpat(_,atpat)) =
	  unguarded_atpat atpat
	| unguarded_pat(DecGrammar.CONSpat(_,_,atpat)) =
	  unguarded_atpat atpat
	| unguarded_pat(DecGrammar.TYPEDpat(_,pat,ty)) =
	  unguarded_pat pat ++ unguarded_ty ty
	| unguarded_pat(DecGrammar.LAYEREDpat(_,_,ty_opt,pat)) =
	  unguarded_opt unguarded_ty ty_opt ++
	  unguarded_pat pat
	| unguarded_pat(DecGrammar.UNRES_INFIXpat _) =
	    impossible "unguarded_pat(UNRES_INFIX)"

      and unguarded_ty(DecGrammar.TYVARty(_,tyvar)) =
	  [tyvar]
	| unguarded_ty(DecGrammar.RECORDty(_,tyrow_opt)) =
	  unguarded_opt unguarded_tyrow tyrow_opt
	| unguarded_ty(DecGrammar.CONty(_,ty_list,_)) =
	  List.foldL
	  (fn ty => fn tyvarset => unguarded_ty ty ++ tyvarset)
	  [] ty_list
	| unguarded_ty(DecGrammar.FNty(_,ty1,ty2)) =
	  unguarded_ty ty1 ++ unguarded_ty ty2
	| unguarded_ty(DecGrammar.PARty(_,ty)) =
	  unguarded_ty ty

      and unguarded_tyrow(DecGrammar.TYROW(_,_,ty,tyrow_opt)) =
	  unguarded_ty ty ++
	  unguarded_opt unguarded_tyrow tyrow_opt
    in
      val unguarded_valbind = unguarded_valbind
    end

    (*The type of items associated with identifiers in a VarEnv is actually
     a slightly richer private type which, for each constructor, remembers
     the fellow constructors in its type. The usual lookup functions
     return an ordinary VE.range.*)
	
    datatype range_private = LONGVARpriv of TypeScheme
                           | LONGCONpriv of TypeScheme * id list
			   | LONGEXCONpriv of Type

    (*the types Context, Env, StrEnv, TyEnv, TyStr, and VarEnv, and
     some layout and report functions for them.*)

    datatype Context = CONTEXT of {T : TyName.Set.Set, 
				   U : ExplicitTyVarEnv,
				   E : Env}
	 and Env = ENV of {SE : StrEnv, TE : TyEnv, VE : VarEnv}
	 and StrEnv = STRENV of (strid, Env) FinMap.map
	 and TyEnv = TYENV of (tycon, TyStr) FinMap.map
	 and TyStr = TYSTR of {theta : TypeFcn, VE : VarEnv}
	 and VarEnv = VARENV of (id, range_private) FinMap.map
	 and ExplicitTyVarEnv = EXPLICITTYVARENV of (ExplicitTyVar,level) FinMap.map

    fun layoutSE (STRENV m) =
          let val l = FinMap.Fold (op ::) nil m

	  fun format_strid strid =
	        implode ["structure ", StrId.pr_StrId strid, " : "]

	  fun layoutPair (strid,S) =
	        PP.NODE {start=format_strid strid,
			 finish="",
			 indent=3,
			 children=[layoutEnv S],
			 childsep=PP.NONE}
	  in
	    PP.NODE {start="", finish="", indent=0, children=map layoutPair l,
		     childsep=PP.RIGHT " "}
	  end

    and layoutTE (TYENV m) =
          let val l = FinMap.Fold (op ::) nil m

	  fun layoutPair (tycon, tystr) =
	        PP.NODE {start=TyCon.pr_TyCon tycon ^ " ",
			 finish="", indent=3, children=[layoutTystr tystr],
			 childsep=PP.NONE}
	  in
	    PP.NODE {start="", finish="", indent=0,
		     children=map layoutPair l, childsep=PP.RIGHT " "}
	  end

    and layoutVE (VARENV m) =
          let val l = FinMap.Fold (op ::) nil m

	  fun format_id id =
	        (case FinMap.lookup m id of
		   Some(LONGVARpriv _) =>
		     implode ["val ", Ident.pr_id id, ":"]
		 | Some(LONGCONpriv _) =>
		     implode ["con ", Ident.pr_id id, ":"]
		 | Some(LONGEXCONpriv _) =>
		     implode ["exception ", Ident.pr_id id, ":"]
		 | None => "<failure: Environments.layoutVE.format_id>")

	  fun layoutRng (LONGVARpriv sigma) =
	        TypeScheme.layout sigma
	    | layoutRng(LONGCONpriv(sigma, _)) =
		TypeScheme.layout sigma
	    | layoutRng(LONGEXCONpriv tau) =
		Type.layout tau

	  fun layoutPair(id, rng) = 
	        PP.NODE {start=format_id id ^ " ", finish="", indent=3,
			 children=[layoutRng rng], childsep = PP.NONE}
	  in
	    PP.NODE {start="", finish="", indent=0,
		     children=map layoutPair l, childsep=PP.RIGHT " "}
	  end

    and layoutTystr (TYSTR {theta, VE}) =
          PP.NODE {start="", finish="", indent=0,
		   children=[TypeFcn.layout theta, layoutVE VE],
		   childsep=PP.RIGHT "/ "}

    and layoutEnv (ENV{SE, TE, VE}) =
          PP.NODE {start="", finish="", indent=0,
		   children=[layoutSE SE, layoutTE TE, layoutVE VE],
		   childsep = PP.RIGHT " "}

    infix // val op // = Report.//



    (*Now a structure for each kind of environment:*)


    structure VE = struct
      (*The type of items associated with identifiers in a VarEnv:*)
      datatype range = LONGVAR   of TypeScheme
	             | LONGCON   of TypeScheme
                     | LONGEXCON of Type            (* MEMO: why LONGxxx? *)

      val empty : VarEnv = VARENV FinMap.empty
      val bogus = empty
      val singleton : id * range_private -> VarEnv =
	    VARENV o FinMap.singleton
      fun singleton_var (id : Ident.id, sigma : TypeScheme) : VarEnv =
            singleton (id, LONGVARpriv sigma)
      fun singleton_con (id : Ident.id, sigma : TypeScheme, ids : id list)
            : VarEnv = singleton (id, LONGCONpriv (sigma, ids))
      fun singleton_excon (id : Ident.id, tau : Type) : VarEnv =
            singleton (id, LONGEXCONpriv tau)
      val add : VarEnv -> id -> range_private -> VarEnv =
	    fn VARENV v => fn id => fn range_private =>
	         VARENV (FinMap.add (id,range_private,v))
      fun plus (VARENV v, VARENV v') : VarEnv = 
	    VARENV (FinMap.plus (v, v'))
      fun range_private_to_range (LONGVARpriv sigma) = LONGVAR sigma
	| range_private_to_range (LONGCONpriv(sigma, _)) = LONGCON sigma
	| range_private_to_range (LONGEXCONpriv tau) = LONGEXCON tau
      fun lookup (VARENV v) id : range Option =
            map_opt range_private_to_range (FinMap.lookup v id)
      fun dom (VARENV m) = FinMap.dom m
      fun is_empty (VARENV v) = FinMap.isEmpty v
      fun map (f : range_private -> range_private) (VARENV m) : VarEnv =
	    VARENV(FinMap.composemap f m)
      fun eq (VARENV v1, VARENV v2) : bool =
	    let val finmap_to_sorted_alist =
		      ListSort.sort
		        (fn ((id1, _), (id2, _)) => Ident.< (id1,id2))
		      o FinMap.list
		val alist1 = finmap_to_sorted_alist v1
		val alist2 = finmap_to_sorted_alist v2
	    in
	      List.foldL
	        (fn ((id1, LONGCONpriv (sigma1, ids1)),
		     (id2, LONGCONpriv (sigma2, ids2))) =>
		 (fn bool =>
		      bool andalso
		      id1 = id2 andalso
		      TypeScheme.eq (sigma1,sigma2))
		   | _ => impossible "VE.eq: VE contains non-constructors")
		  true (ListPair.zip (alist1, alist2))
		  handle ListPair.Zip => false
	    end
      fun fold (f : range -> 'a -> 'a)
      	       (start : 'a)
	       (VARENV map) : 'a = 
	    FinMap.fold
	      (fn (range, a) => f (range_private_to_range range) a)
	        start map
      fun FoldPRIVATE (f : id * range_private -> 'a -> 'a)
      		      (start : 'a) (VARENV map) : 'a =
		        FinMap.Fold (General.uncurry f) start map
      fun Fold (f :   id * range -> 'a  -> 'a)
      	       (start : 'a)
	       (VARENV map) : 'a = 
	    FinMap.Fold (fn ((id, range), a) =>
			      f (id, range_private_to_range range) a)
	      start map

      fun apply (f : (id * range -> unit)) (VARENV map) : unit = 
	    List.apply
	       (fn (id, range_private) =>
		      f (id, range_private_to_range range_private))
	          (FinMap.list map)

      (*CEfold f a VE = will crash if there is anything else than
       constructors in VE; fold f over the constructors in VE.  CEFold
       is similar*)

      fun CEfold (f : TypeScheme -> 'a -> 'a) : 'a -> VarEnv -> 'a =
	    fold
	      (fn range => fn a' =>
	       (case range of
		  LONGCON sigma => f sigma a'
		| _ => impossible "CEfold: VE contains non-constructors"))
      fun CEFold (f : id * TypeScheme -> 'a -> 'a) : 'a -> VarEnv -> 'a =
            Fold
	      (fn (id,range) => fn a =>
	       (case range of
		  LONGCON sigma => f (id,sigma) a
		| _ => impossible "CEFold: VE contains non-constructors"))
	    
      fun close (VE : VarEnv) : VarEnv =
	    FoldPRIVATE
	      (fn (id, range_private) => fn VE =>
	             add VE id
		       (case range_private of
			  LONGVARpriv sigma =>
			    LONGVARpriv (TypeScheme.close true sigma)
			| LONGCONpriv (sigma, ids) =>
			    LONGCONpriv (TypeScheme.close true sigma, ids)
			| LONGEXCONpriv tau => LONGEXCONpriv tau))
	         empty VE

      fun lookup_fellow_constructors (VARENV v) id : id list Option =
	    (case FinMap.lookup v id of
	       Some (LONGCONpriv(sigma, ids)) => Some ids
	     | _ => None)

      fun on (S : Substitution, VE as VARENV m) : VarEnv = VE

      fun restrict (VARENV m,ids) =
	    VARENV (List.foldL
		      (fn id => fn m_new =>
		       let val r = noSome (FinMap.lookup m id) "VE.restrict"
		       in FinMap.add(id,r,m_new)
		       end)
		         FinMap.empty ids)

      fun report (f, VARENV m) =
	    FinMap.reportMapSORTED (Ident.<)
	      (fn (id, range_private) =>
	            f (id, range_private_to_range range_private)) m
      val layout = layoutVE
      fun tyvars_in_range (LONGVAR sigma) = TypeScheme.tyvars sigma
	| tyvars_in_range (LONGCON sigma) = TypeScheme.tyvars sigma
	| tyvars_in_range (LONGEXCON tau) = Type.tyvars tau
      val tyvars =
	      fold
		(fn range => fn T =>
		       unionTyVarSet (T, tyvars_in_range range))
		   []
      val tyvars' =
	      Fold
		(fn (id, range) => fn criminals =>
		       (case tyvars_in_range range of
			  [] => criminals
			| tyvars => (id, tyvars) :: criminals))
		    []
      fun tynames_in_range (LONGVAR sigma) = TypeScheme.tynames sigma
	| tynames_in_range (LONGCON sigma) = TypeScheme.tynames sigma
	| tynames_in_range (LONGEXCON tau) = Type.tynames tau
      val tynames =
	    fold (TyName.Set.union o tynames_in_range) TyName.Set.empty
    end (*VE*)




    (*Type structures*)

    structure TyStr = struct
      val bogus = TYSTR {theta=TypeFcn.bogus, VE=VE.empty}
      fun from_theta_and_VE (theta : TypeFcn, VE : VarEnv) : TyStr =
	    TYSTR {theta = theta, VE = VE}
      fun to_theta_and_VE (TYSTR {theta, VE}) : TypeFcn * VarEnv = (theta, VE)
      fun to_theta (TYSTR {theta, VE}) : TypeFcn = theta
      fun to_VE (TYSTR {theta, VE}) : VarEnv = VE
      fun eq (TYSTR {theta,VE}, TYSTR {theta=theta',VE=VE'}) =
	    TypeFcn.eq (theta,theta') andalso VE.eq (VE,VE')
      fun shares (TYSTR {theta=theta1, ...},
		  TYSTR {theta=theta2, ...}) : bool =
	    TypeFcn.eq (theta1,theta2)
      fun tynames (TYSTR {theta, VE}) =
	    TyName.Set.union (TypeFcn.tynames theta) (VE.tynames VE)
      val layout = layoutTystr
    end (*TyStr*)



    (*Type environments*)

    structure TE = struct 
      val empty : TyEnv = TYENV FinMap.empty
      val singleton : tycon * TyStr -> TyEnv = TYENV o FinMap.singleton
      fun plus (TYENV t, TYENV t') : TyEnv = TYENV (FinMap.plus (t, t'))
      fun lookup (TYENV m) tycon : TyStr Option = FinMap.lookup m tycon
      fun dom (TYENV map) = FinMap.dom map
      fun map (f : TyStr -> TyStr) (TYENV m) : TyEnv =
	    TYENV (FinMap.composemap f m)
      fun fold (f : TyStr -> 'a -> 'a) (start : 'a) (TYENV map) : 'a = 
	    FinMap.fold (General.uncurry f) start map
      fun Fold (f : tycon * TyStr -> 'a -> 'a) (start : 'a) (TYENV map)
	    : 'a = FinMap.Fold (General.uncurry f) start map
      fun apply (f : (tycon * TyStr -> unit)) (TYENV map) : unit = 
	    List.apply f (FinMap.list map)

      (*equality_maximising_realisation TE = a realisation that maps all
       tynames in TE that have been found to respect equality to fresh
       tynames that admit equality.  equality_maximising_realisation is
       only used by maximise_equality below.  It works by first finding
       the set T of tynames that are tynames of datatypes that do not
       already admit equality (tynames_of_nonequality_datatypes).
       iterate then removes from T those tynames that do not respect
       equality, assuming all in T respect equality.  This is iterated
       until T does not change.  generate then makes a realisation from
       T.*)

      local
	fun tynames_of_nonequality_datatypes (TE : TyEnv) : TyName.Set.Set =
	      fold (fn TYSTR {theta, VE} => fn T =>
		          if VE.is_empty VE then T else
			  let val tyname =
			            noSome (TypeFcn.to_TyName theta)
				      "tynames_of_nonequality_datatypes"
			  in
			    if TyName.equality tyname then T
			    else TyName.Set.insert tyname T
			  end)
	               TyName.Set.empty TE

        fun iterate T TE =
	      let val T' = fold remove_a_tyname_maybe T TE
	      in
		if TyName.Set.eq T T' then T else iterate T' TE
	      end

	and remove_a_tyname_maybe (TYSTR {theta, VE}) T =
	      if VE.is_empty VE then T else
	      let val violates_equality =
		        VE.CEfold
			  (fn sigma => fn bool => 
			   TypeScheme.violates_equality T sigma orelse bool)
			       false VE
		  val tyname = noSome (TypeFcn.to_TyName theta)
			         "remove_a_tyname_maybe"
	      in
		if violates_equality then TyName.Set.remove tyname T
		else T
	      end

	(*generate T = a realisation that maps each t in T to a fresh t'
	 that admits equality*)

	fun generate (T : TyName.Set.Set) : realisation =
	      TyName.Set.fold
	        (fn tyname => fn phi => Realisation.oo (phi, generate0 tyname))
		  Realisation.Id T
	and generate0 tyname =
	      (if !Flags.DEBUG_TYPES then output (std_out,"generate0\n")
	       else () ;
	       Realisation.singleton
	         (tyname, TypeFcn.from_TyName
		           (TyName.freshTyName
			     {tycon = TyName.tycon tyname,
			      arity = TyName.arity tyname, equality = true})))
      in
        fun equality_maximising_realisation (TE : TyEnv) : realisation =
	      generate (iterate (tynames_of_nonequality_datatypes TE) TE)
      end (*local*)    

      fun init' explicittyvars tycon =
	let val tyname = TyName.freshTyName {tycon=tycon, arity=List.size explicittyvars, equality=false}
	    val TE = singleton (tycon, TyStr.from_theta_and_VE (TypeFcn.from_TyName tyname, VE.empty))
	in (tyname, TE)
	end

      fun init explicittyvars tycon = #2 (init' explicittyvars tycon)

      val tynames = fold (TyName.Set.union o TyStr.tynames) TyName.Set.empty

      (*For the TE: report TyStrs with empty ConEnv as

	  type (tvs) ty

	and those with nonempty ConEnv as

	  datatype (tvs) ty
	    con C1 : tyscheme
	    con C2 : tyscheme

	This is a reasonable syntax for TE's both in bindings and in specs.
	(saying `type' for `datatype' wouldn't look too good in specs.)*)

      local
	fun reportCE (VARENV v) =
	      FinMap.reportMapSORTED
		Ident.<
		  (fn (id, range_private) =>
			(case range_private of
			   LONGCONpriv (sigma, ids) =>
			     Report.line ("con " ^ Ident.pr_id id ^ " : "
					  ^ TypeScheme.pretty_string
					      (StatObject.newTVNames ()) sigma)
			 | _ => impossible "reportCE"))
		    v
      in
	fun report {tyEnv=TYENV map, bindings} =
	      FinMap.reportMapSORTED
		TyCon.<
		  (fn (tycon, TYSTR {theta, VE as (VARENV v)}) =>
		   let
		     val names = StatObject.newTVNames()
		     val {vars, body} =
			     TypeFcn.pretty_string names theta
		     val vars' = (case vars of "" => "" | _ => vars ^ " ")
		   in
		     Report.line
		       (if FinMap.isEmpty v then
			  "type " ^ vars' ^ TyCon.pr_TyCon tycon
			  ^ (if bindings then " = " ^ body else "")
			else
			  "datatype " ^ vars' ^ TyCon.pr_TyCon tycon)
		     // Report.indent(2, reportCE VE)
		   end)
		     map
      end (*local*)
      val layout = layoutTE
    end (*TE*)




    structure SE = struct
      val empty : StrEnv = STRENV FinMap.empty
      val singleton : strid * Env -> StrEnv = STRENV o FinMap.singleton
      fun plus (STRENV s, STRENV s') : StrEnv =
	    STRENV(FinMap.plus(s, s'))
      fun lookup (STRENV map) (strid : strid) : Env Option =
	    FinMap.lookup map strid
      fun dom (STRENV map) = FinMap.dom map
      fun is_empty (STRENV m) = FinMap.isEmpty m
      fun fold (f : Env -> 'a -> 'a) (start : 'a) (STRENV map) : 'a = 
	    FinMap.fold (General.uncurry f) start map
      fun Fold (f : strid * Env -> 'a -> 'a) (start : 'a) (STRENV map) : 'a = 
	    FinMap.Fold (General.uncurry f) start map
      fun apply (f : strid * Env -> unit) (STRENV map) : unit = 
	    List.apply f (FinMap.list map)
      fun map (f : Env -> Env) (STRENV map) =
	    STRENV (FinMap.composemap f map)
      fun report (f, STRENV m) = FinMap.reportMapSORTED (StrId.<) f m

      (*since SE.tyvars and E.tyvars are mutually recursive, E.tyvars is defined
       in structure SE rather than structure E.  Similarly with tynames:*)
      fun E_tyvars (ENV {SE, VE, ...}) = unionTyVarSet (VE.tyvars VE, tyvars SE)
      and tyvars SE =
	    fold (fn E => fn tyvars => unionTyVarSet (tyvars, E_tyvars E))
		  [] SE
      fun E_tyvars' (ENV {SE, VE, ...}) = VE.tyvars' VE @ tyvars' SE
      and tyvars' SE =
	    fold (fn E => fn criminals => E_tyvars' E @ criminals) [] SE
      fun tynames SE =
	    fold (fn E => fn T => TyName.Set.union (E_tynames E) T)
	            TyName.Set.empty SE
      and E_tynames (ENV {VE, TE, SE}) =
	      TyName.Set.union (tynames SE)
		(TyName.Set.union (VE.tynames VE) (TE.tynames TE))
      val layout = layoutSE
    end (*SE*)



    structure E = struct
      fun mk (SE, TE, VE) : Env = ENV {SE=SE, TE=TE, VE=VE}
      fun un (ENV {SE, TE, VE}) = (SE, TE, VE)
      fun from_VE_and_TE (VE, TE) = ENV {SE=SE.empty, TE=TE, VE=VE}
      fun from_VE VE = ENV {SE=SE.empty, TE=TE.empty, VE=VE}
      fun to_VE (ENV {VE, ...}) = VE
      fun from_TE TE = ENV {SE=SE.empty, TE=TE, VE=VE.empty}
      fun to_TE (ENV {TE, ...}) = TE
      fun from_SE SE = ENV {SE=SE, TE=TE.empty, VE=VE.empty}
      fun to_SE (ENV {SE, ...}) = SE
      fun plus (ENV {SE, TE, VE}, ENV {SE=SE', TE=TE', VE=VE'}) =
	    ENV {SE = SE.plus (SE, SE'), TE = TE.plus (TE, TE'),
		 VE = VE.plus (VE, VE')}
      fun lookup_tycon (ENV {TE, ...}) tycon : TyStr Option =
	    TE.lookup TE tycon
      fun lookup_strid (ENV {SE, ...}) strid : Env Option =
	    SE.lookup SE strid
      fun lookup_strids E [] = Some E
	| lookup_strids E (strid :: rest) =
	    (case lookup_strid E strid of
	       Some E' => lookup_strids E' rest
	     | None => None)

      (*`lookup_longsomething to_TE TE.lookup E ([strid1,strid2], tycon)'
       will look up `strid1.strid2.tycon'
       (or is it `strid2.strid1.tycon'?) in E:*)
      fun lookup_longsomething projection lookup E (strids, something) =
	    (case lookup_strids E strids of
	       None => None
	     | Some E' => lookup (projection E') something)
      fun lookup_longid E longid = 
	    lookup_longsomething to_VE VE.lookup E (Ident.decompose longid)
      fun lookup_fellow_constructors E longid =
	    noSome (lookup_longsomething to_VE VE.lookup_fellow_constructors
		      E (Ident.decompose longid))
	      "lookup_fellow_constructors"
      fun lookup_longtycon E longtycon =
	    lookup_longsomething to_TE TE.lookup E
	      (TyCon.explode_LongTyCon longtycon)
      fun lookup_longstrid E longstrid =
	    lookup_longsomething to_SE SE.lookup E
	      (StrId.explode_longstrid longstrid)
      (*on: Note that only VE contains free type variables:*)
      fun on (S, ENV {SE, TE, VE}) = ENV {SE=SE, TE=TE, VE=VE.on (S, VE)}
      val empty = ENV {SE=SE.empty, TE=TE.empty, VE=VE.empty}
      val bogus = empty
      val tyvars = SE.E_tyvars
      val tyvars' = SE.E_tyvars'
      val tynames = SE.E_tynames
      val layout = layoutEnv

      (*The following boring code builds the initial environment
       built-in types (`->', `int', `real', `string', `exn', `ref'),
       the constructor `ref', and the value `prim'. `prim' has type
       (int * 'a) -> 'b.  Moreover the overloaded values, and therefore
       the type `bool'.*)

      local
	fun mk_tystr (tyname, VE) =
	      TyStr.from_theta_and_VE
	        (TypeFcn.from_TyName tyname, VE)

	fun te (tycon, tyname) =
	      TE.singleton (tycon, mk_tystr(tyname, VE.empty))

	fun joinTE [] = TE.empty
	  | joinTE (TE :: rest) = TE.plus (TE, joinTE rest)

	val TE_int = te (TyCon.tycon_INT, TyName.tyName_INT)
	val TE_char = te (TyCon.tycon_CHAR, TyName.tyName_CHAR)
	val TE_word = te (TyCon.tycon_WORD, TyName.tyName_WORD)
	val TE_word8 = te (TyCon.tycon_WORD8, TyName.tyName_WORD8)
	val TE_real = te (TyCon.tycon_REAL, TyName.tyName_REAL)
	val TE_string = te (TyCon.tycon_STRING, TyName.tyName_STRING)
	val TE_exn = te (TyCon.tycon_EXN, TyName.tyName_EXN)

	val boolVE =
	      VE.plus (VE.singleton_con (Ident.id_TRUE, 
					 TypeScheme.from_Type Type.Bool,
					 [Ident.id_FALSE, Ident.id_TRUE]),
	               VE.singleton_con (Ident.id_FALSE,
					 TypeScheme.from_Type Type.Bool,
					 [Ident.id_FALSE, Ident.id_TRUE]))
	val TE_bool = TE.singleton (TyCon.tycon_BOOL,
				   mk_tystr (TyName.tyName_BOOL,
					     VE.close boolVE))

	(* constructor environments for ref type constructor *)

	local
	  val _ = Level.push()

	  val alpha = TyVar.fresh_normal ()
	  val alphaTy = Type.from_TyVar alpha

	  val refTy =
	        Type.mk_Arrow (alphaTy, Type.mk_Ref alphaTy)

	  val beta = TyVar.fresh_normal ()
	  val betaTy = Type.from_TyVar beta

	  val refTy_to_TE =
	        Type.mk_Arrow (betaTy, Type.mk_Ref betaTy)

	  val _ = Level.pop()
	in
	  val refVE = VE.singleton_con (Ident.id_REF,
					TypeScheme.from_Type refTy,
					[Ident.id_REF])
	  val refVE_to_TE = VE.singleton_con (Ident.id_REF, 
					      TypeScheme.from_Type
					        refTy_to_TE,
					      [Ident.id_REF])
	end

		 (* environments for list type constructor *)
	local
	  val _ = Level.push()

		 (* nil *)
	  val alpha1 = TyVar.fresh_normal ()
	  val alphaTy = Type.from_TyVar alpha1

	  val listTy =
		Type.from_ConsType
		  (Type.mk_ConsType ([alphaTy],TyName.tyName_LIST))

	  val nilVE = VE.singleton_con (Ident.id_NIL,
					TypeScheme.from_Type listTy,
					[Ident.id_CONS, Ident.id_NIL])

		 (* cons *)
	  val alpha1 = TyVar.fresh_normal ()

	  val alphaTy = Type.from_TyVar alpha1
	  val listTy = 
		Type.from_ConsType
		  (Type.mk_ConsType ([alphaTy],TyName.tyName_LIST))
	  val consTy = 
		Type.mk_Arrow
		  (Type.from_pair (alphaTy,listTy) , listTy)
	  val consVE = VE.singleton_con (Ident.id_CONS,
					 TypeScheme.from_Type consTy,
					 [Ident.id_CONS, Ident.id_NIL])
	  val listVE = VE.plus (consVE, nilVE)

	  (*Now the above bindings are repeated, to get fresh
	   variants of type schemes for cons and nil, for TE*)

		 (* nil *)
	  val alpha1 = TyVar.fresh_normal ()

	  val alphaTy = Type.from_TyVar alpha1

	  val listTy =
		Type.from_ConsType
		  (Type.mk_ConsType ([alphaTy],TyName.tyName_LIST))

	  val nilVE = VE.singleton_con (Ident.id_NIL,
					TypeScheme.from_Type listTy,
					[Ident.id_CONS, Ident.id_NIL])

		 (* cons *)
	  val alpha1 = TyVar.fresh_normal ()
	  val alphaTy = Type.from_TyVar alpha1
	  val listTy = 
		Type.from_ConsType
		  (Type.mk_ConsType ([alphaTy],TyName.tyName_LIST))
	  val consTy = 
		Type.mk_Arrow
		  (Type.from_pair (alphaTy,listTy) , listTy)
	  val consVE = VE.singleton_con (Ident.id_CONS,
					 TypeScheme.from_Type consTy,
					 [Ident.id_CONS, Ident.id_NIL])

	  val listVE_for_TE = VE.plus (consVE, nilVE)

	  val _ = Level.pop()
	in
	  val TE_list = TE.singleton (TyCon.tycon_LIST,
				     mk_tystr (TyName.tyName_LIST,
					       VE.close listVE_for_TE))
	  val listVE = listVE
	end

	       (* overloaded functions *)
	local
	  val _ = Level.push()
	  val alpha = TyVar.fresh_normal ()
	  val alphaTy = Type.from_TyVar alpha

	  val beta = TyVar.fresh_normal ()
	  val betaTy = Type.from_TyVar beta

	  val tau_int_X_alpha_to_beta = Type.mk_Arrow
			 (Type.from_pair (Type.Int, alphaTy),
			  betaTy)

	  val sigma_int_X_alpha_to_beta = TypeScheme.from_Type tau_int_X_alpha_to_beta

	  val tyvar_num = TyVar.fresh_overloaded [TyName.tyName_INT,
						  TyName.tyName_WORD,
						  TyName.tyName_WORD8,
						  TyName.tyName_REAL]
	  val tau_num = Type.from_TyVar tyvar_num

	  val tau_num_X_num_to_num =
		Type.mk_Arrow (Type.from_pair (tau_num, tau_num), tau_num)

	  val tyvar_realint =
	        TyVar.fresh_overloaded [TyName.tyName_REAL, TyName.tyName_INT]
	  val tau_realint = Type.from_TyVar tyvar_realint

	  val tau_realint_to_realint = Type.mk_Arrow (tau_realint, tau_realint)

	  val tyvar_numtxt = TyVar.fresh_overloaded 
				[TyName.tyName_INT, TyName.tyName_WORD, TyName.tyName_WORD8,
				 TyName.tyName_REAL, TyName.tyName_CHAR,
				 TyName.tyName_STRING]
	  val tau_numtxt = Type.from_TyVar tyvar_numtxt
	  val tau_numtxt_X_numtxt_to_bool =
		Type.mk_Arrow (Type.from_pair (tau_numtxt, tau_numtxt),
			       Type.Bool)

	  val tyvar_wordint = TyVar.fresh_overloaded [TyName.tyName_INT,
						      TyName.tyName_WORD, TyName.tyName_WORD8]
	  val tau_wordint = Type.from_TyVar tyvar_wordint
	  val tau_wordint_X_wordint_to_wordint =
	        Type.mk_Arrow (Type.from_pair (tau_wordint, tau_wordint),
			       tau_wordint)

	  val _ = Level.pop()

	  (*overloaded types manually closed, to ensure closing of overloaded 
	   type variable:*)

	  val sigma_num_X_num_to_num = 
		TypeScheme.from_TyVars_and_Type
		  ([tyvar_num], tau_num_X_num_to_num)

	  val sigma_realint_to_realint = 
		TypeScheme.from_TyVars_and_Type
		  ([tyvar_num], tau_realint_to_realint)

	  val sigma_numtxt_X_numtxt_to_bool =
		TypeScheme.from_TyVars_and_Type
		  ([tyvar_num], tau_numtxt_X_numtxt_to_bool)

	  val sigma_wordint_X_wordint_to_wordint =
	        TypeScheme.from_TyVars_and_Type
		  ([tyvar_wordint], tau_wordint_X_wordint_to_wordint)

	in
	  val primVE      = VE.singleton (Ident.id_PRIM,
					  LONGVARpriv sigma_int_X_alpha_to_beta)
	  val absVE       = VE.singleton (Ident.id_ABS, 
					  LONGVARpriv sigma_realint_to_realint)
	  val negVE       = VE.singleton (Ident.id_NEG,
					  LONGVARpriv sigma_realint_to_realint)
	  val divVE       = VE.singleton (Ident.id_DIV,
					  LONGVARpriv sigma_wordint_X_wordint_to_wordint)
	  val modVE       = VE.singleton (Ident.id_MOD,
					  LONGVARpriv sigma_wordint_X_wordint_to_wordint)
	  val plusVE      = VE.singleton (Ident.id_PLUS, 
					  LONGVARpriv sigma_num_X_num_to_num)
	  val minusVE     = VE.singleton (Ident.id_MINUS,
					  LONGVARpriv sigma_num_X_num_to_num)
	  val mulVE       = VE.singleton (Ident.id_MUL, 
					  LONGVARpriv sigma_num_X_num_to_num)
	  val lessVE      = VE.singleton (Ident.id_LESS, 
					  LONGVARpriv sigma_numtxt_X_numtxt_to_bool)
	  val greaterVE   = VE.singleton (Ident.id_GREATER, 
					  LONGVARpriv sigma_numtxt_X_numtxt_to_bool)
	  val lesseqVE    = VE.singleton (Ident.id_LESSEQ, 
					  LONGVARpriv sigma_numtxt_X_numtxt_to_bool)
	  val greatereqVE = VE.singleton (Ident.id_GREATEREQ, 
					  LONGVARpriv sigma_numtxt_X_numtxt_to_bool)

	  val DivVE       = VE.singleton (Ident.id_Div, LONGEXCONpriv Type.Exn)
	  val ModVE       = VE.singleton (Ident.id_Mod, LONGEXCONpriv Type.Exn)
	  val BindVE      = VE.singleton (Ident.id_Bind, LONGEXCONpriv Type.Exn)
	  val MatchVE     = VE.singleton (Ident.id_Match, LONGEXCONpriv Type.Exn)

	  fun joinVE [] = VE.empty
	    | joinVE (VE :: rest) = VE.plus (VE, joinVE rest)
	end

	val TE_ref = TE.singleton (TyCon.tycon_REF,
				  mk_tystr (TyName.tyName_REF, VE.close refVE_to_TE))

	local   (* initial TE for unit *)
	  val theta_unit = TypeFcn.from_TyVars_and_Type ([], Type.Unit)
	  val VE_unit = VE.empty
	in
	  val TE_unit = TE.singleton (TyCon.tycon_UNIT,
				      TyStr.from_theta_and_VE (theta_unit, VE_unit))
	end

	val TE_initial = joinTE [TE_unit, TE_int, TE_real,
				 TE_word, TE_char, 
				 TE_string, TE_exn, TE_ref, TE_bool, TE_list]
	local 
	  fun mk_sigma() = (*construct the type scheme: forall 'a.'a->unit*)
	    let val _ = Level.push()
		val alpha = TyVar.fresh_normal ()
		val alphaTy = Type.from_TyVar alpha
		val arrowType = Type.mk_Arrow (alphaTy, Type.Unit)
		val _ = Level.pop()
	    in 
	      TypeScheme.from_TyVars_and_Type ([alpha], arrowType)
	    end
	in 
	  val resetRegionsVE =  (* resetRegions: forall 'a . 'a -> unit *)
		  VE.singleton (Ident.resetRegions,LONGVARpriv (mk_sigma()))
	  val forceResettingVE = (* forceResetting: forall 'a . 'a -> unit *)
		  VE.singleton (Ident.forceResetting, LONGVARpriv (mk_sigma()))
	end

	(*notice VE.close must be applied outside the Level.push and Level.pop
	 calls:*)
	val VE_initial =
	      joinVE [VE.close refVE, VE.close boolVE, VE.close listVE,
		      VE.close primVE,
		      absVE, negVE, divVE, modVE, plusVE, minusVE, mulVE,
		      lessVE, greaterVE, lesseqVE, greatereqVE,
		      resetRegionsVE, forceResettingVE, DivVE, ModVE,
		      BindVE, MatchVE]
      in
	val initial = ENV {SE=SE.empty, TE=TE_initial, VE=VE_initial}
      end


      (* Support for recompilation *)

      fun equal_VarEnvRan (VE.LONGVAR s1, VE.LONGVAR s2) =
	    StatObject.TypeScheme.eq (s1,s2)
	| equal_VarEnvRan (VE.LONGCON s1, VE.LONGCON s2) =
	    StatObject.TypeScheme.eq (s1,s2)
	| equal_VarEnvRan (VE.LONGEXCON t1, VE.LONGEXCON t2) =
	    StatObject.Type.eq (t1,t2)
	| equal_VarEnvRan _ = false

      fun enrich_TyEnv (TE1,TE2) =
	    TE.Fold (fn (tycon2, TyStr2) => fn b => b andalso
		     case TE.lookup TE1 tycon2 of
		       Some TyStr1 => TyStr.eq (TyStr1,TyStr2)
		     | None => false) true TE2

      fun equal_TyEnv(TE1,TE2) =
	let val sorter = TyCon.<
	    val dom1 = ListSort.sort sorter (EqSet.list (TE.dom TE1))
	    val dom2 = ListSort.sort sorter (EqSet.list (TE.dom TE2))
	in dom1 = dom2 andalso enrich_TyEnv (TE1,TE2)
	end

      fun enrich_VarEnv(VE1,VE2) =
	    VE.Fold (fn (id2,r2) => fn b => b andalso
			  (case VE.lookup VE1 id2 of
			     Some r1 => equal_VarEnvRan(r1,r2)
			   | None => false)) true VE2

      fun equal_VarEnv(VE1,VE2) =
	let val sorter = Ident.<
	    val dom1 = ListSort.sort sorter (EqSet.list (VE.dom VE1))
	    val dom2 = ListSort.sort sorter (EqSet.list (VE.dom VE2))
	in dom1 = dom2 andalso enrich_VarEnv(VE1,VE2)
	end

      fun equal_Env(E1,E2) =
	let val (SE1,TE1,VE1) = un E1
	    val (SE2,TE2,VE2) = un E2
	in equal_StrEnv(SE1,SE2)
	  andalso equal_TyEnv(TE1,TE2)
	  andalso equal_VarEnv(VE1,VE2)
	end

      and enrich_StrEnv(SE1,SE2) = SE.Fold (fn (strid2,S2) => fn b => b andalso
					     case SE.lookup SE1 strid2 of
					       Some S1 => equal_Env(S1,S2)
					     | None => false) true SE2

      and equal_StrEnv(SE1,SE2) =
	let val sorter = StrId.<
	    val dom1 = ListSort.sort sorter (EqSet.list (SE.dom SE1))
	    val dom2 = ListSort.sort sorter (EqSet.list (SE.dom SE2))
	in dom1 = dom2 andalso enrich_StrEnv (SE1,SE2)
	end

      fun enrich_Env(E1,E2) =
	let val (SE1,TE1,VE1) = un E1
	    val (SE2,TE2,VE2) = un E2
	in enrich_StrEnv(SE1,SE2)
	  andalso enrich_TyEnv(TE1,TE2)
	  andalso enrich_VarEnv(VE1,VE2)
	end

      val enrich = enrich_Env
      val eq = equal_Env


      (* Restriction *)
      fun restrict(E,(ids,tycons,strids)) =
	let val (SE,TE,VE) = un E
	    val SE' = List.foldL
	                (fn strid => fn SEnew =>
			 let val E = (case SE.lookup SE strid of
					Some E => E
				      | None => impossible "restrictE: strid not in env.")
			 in SE.plus (SEnew, SE.singleton (strid,E))
			 end) SE.empty strids
	    val TE' = List.foldL
	                (fn tycon => fn TEnew =>
			 let val TyStr = (case TE.lookup TE tycon of
					    Some TyStr => TyStr
					  | None => impossible "restrictE: tycon not in env.")
			 in TE.plus (TEnew, TE.singleton (tycon,TyStr))
			 end) TE.empty tycons
	    val VE' = VE.restrict (VE,ids)
	in mk (SE',TE',VE')
	end

      (* Matching *)

	fun match_VE_range (VE.LONGVAR sigma1, VE.LONGVAR sigma2) =
	      StatObject.TypeScheme.match (sigma1, sigma2)
	  | match_VE_range (VE.LONGCON sigma1, VE.LONGCON sigma2) =
	      StatObject.TypeScheme.match (sigma1, sigma2)
	  | match_VE_range (VE.LONGEXCON tau1, VE.LONGEXCON tau2) =
	      StatObject.Type.match (tau1, tau2)
	  | match_VE_range _ = ()

	fun matchVE (VE,VE0) = VE.Fold (fn (id,r) => fn () =>
					   (case VE.lookup VE0 id of
					      Some r0 => match_VE_range (r,r0)
					    | None => ())) () VE

	fun matchTE (TE,TE0) = TE.Fold (fn (tycon, TyStr) => fn () =>
					   (case TE.lookup TE0 tycon of
					      Some TyStr0 => ()
					    (*C.match_TyStr(TyStr,TyStr0) *) (* MEMO *)
					    | None => ())) () TE

	fun match (E,E0) =
	      let val (SE, TE, VE)  = un E
		  val (SE0,TE0,VE0) = un E0
	      in
		matchSE (SE, SE0) ; 
		matchTE (TE, TE0) ;
		matchVE (VE, VE0)
	      end

	and matchSE (SE,SE0) = SE.Fold (fn (strid,E) => fn () =>
					   (case SE.lookup SE0 strid of
					      Some E0 => match (E,E0)
					    | None => ())) () SE
    end (*E*)




    structure C = struct
      (*ExplicitTyVarEnv*) 
      val U_empty = EXPLICITTYVARENV FinMap.empty
      fun plus_U (CONTEXT {T,U = EXPLICITTYVARENV U,E},
		  ExplicitTyVars : ExplicitTyVar list) : Context =
	    let val U' = List.foldL
			   (fn ExplicitTyVar => fn m => 
				 FinMap.add (ExplicitTyVar,
					     Level.current (),m))
			     FinMap.empty ExplicitTyVars
	    in
	      CONTEXT {T=T, U=EXPLICITTYVARENV (FinMap.plus (U,U')), E=E}
	    end
      fun to_U (CONTEXT {T, U=EXPLICITTYVARENV m, E}) : ExplicitTyVar list =
	    EqSet.list(FinMap.dom m)
      fun to_U' (CONTEXT {T,U,E}) : ExplicitTyVarEnv = U 
      fun ExplicitTyVarEnv_lookup (EXPLICITTYVARENV m) ExplicitTyVar =
	    (case FinMap.lookup m ExplicitTyVar of
	       None => Level.GENERIC
	     | Some l => l)
      fun from_T_and_E (T, E) = CONTEXT {T=T, U=U_empty, E=E}
      fun to_E (CONTEXT {T, U, E}) = E
      fun cplus_E (CONTEXT {T, U, E}, E') =
	    CONTEXT {T=TyName.Set.union T (E.tynames E'),
		     U=U, E=E.plus (E, E')}
      fun plus_E (CONTEXT {T, U, E}, E') = CONTEXT {T=T, U=U, E=E.plus (E, E')}
      fun cplus_TE (C, TE) = cplus_E (C, E.from_TE TE)
      fun plus_TE(C,TE) = plus_E(C,E.from_TE TE)
      fun cplus_VE_and_TE (C, (VE, TE)) = cplus_E (C, E.from_VE_and_TE (VE,TE))
      fun plus_VE (CONTEXT {T, U, E as ENV {SE, TE, VE}}, VE') =
	    CONTEXT {T=T, U=U,
		     E=ENV {SE=SE, TE=TE, VE=VE.plus (VE, VE')}}
      fun to_T (CONTEXT {T,U,E}) = T
      fun from_E E = CONTEXT {T=TyName.Set.empty, U=U_empty, E=E}
      fun on (S : Substitution, C as CONTEXT {T, U, E}) = C
      (* was:      CONTEXT {T = T, U = U, E = onE(S, E)} *)
	(*TODO 26/01/1997 21:57. tho.:  er det ^ ok?*)

      val lookup_longid              = E.lookup_longid              o to_E
      val lookup_fellow_constructors = E.lookup_fellow_constructors o to_E
      val lookup_longtycon           = E.lookup_longtycon           o to_E
      val lookup_longstrid           = E.lookup_longstrid           o to_E
      val lookup_tycon               = E.lookup_tycon               o to_E

      local open DecGrammar in
      fun dom_pat (C, pat : pat) : id list =
	    let
	      fun dom_patrow(patrow: patrow): id list =
		case patrow of
		  DOTDOTDOT _ =>
		    nil

		| PATROW(_, _, pat, None) =>
		    dom_pat' pat

		| PATROW(_, _, pat, Some patrow') =>
		    dom_pat' pat @ dom_patrow patrow'

	      and dom_atpat(atpat: atpat): id list =
		case atpat of
		  WILDCARDatpat _ => nil
		| SCONatpat(_, _) => nil
		| RECORDatpat(_, None) => nil
		| RECORDatpat(_, Some patrow) => dom_patrow patrow
		| PARatpat(_, pat) => dom_pat' pat

		| LONGIDatpat(i, OP_OPT(longid, _)) =>
		    (case lookup_longid C longid of
		       Some (VE.LONGCON _) => []
		     | Some (VE.LONGEXCON _) => []
		     | _ => (case Ident.decompose longid of
			       ([], id) => [id]
			     | _ => impossible "dom_atpat"))

	      and dom_pat'(pat: pat): id list =
		case pat of
		  ATPATpat(_, atpat) => dom_atpat atpat
		| CONSpat(_, _, atpat) => dom_atpat atpat
		| TYPEDpat(_, pat, _) => dom_pat' pat
		| LAYEREDpat(_, OP_OPT(id, _), _, pat) => id :: dom_pat' pat
		| UNRES_INFIXpat _ => impossible "dom_pat'(UNRES_INFIX)"
	    in
	      dom_pat' pat
	    end
      end (*local*)

      local
	exception BoundTwice of VarEnv
          (*raised if an identifier is bound twice in the valbind ---
	   this has been spotted earlier, and we choose to refrain
	   to close any of the typeschemes in the VE, and just
	   return VE unmodified *)
      in
      fun close (C : Context, valbind : valbind, VE : VarEnv) : VarEnv =
	let
	  val CONTEXT {E=ENV{SE=SE, VE=VARENV ve_map, ...}, 
		       U=EXPLICITTYVARENV U, ...} = C

	  open DecGrammar 

	  (*isExpansiveId valbind = a finmap which, for every
	   variable bound in a pattern in valbind, returns true
	   if the expression corresponding to the pattern is
	   expansive, otherwise false:*)

	  fun isExpansiveId (valbind : valbind) =
	    let 
	      fun makemap pat exp = 
		let
		  fun harmless_con longid =
			(case lookup_longid C longid of
			   Some(VE.LONGVAR _) => false
			 | Some(VE.LONGCON _) =>
			     #2 (Ident.decompose longid) <> Ident.id_REF
			 | Some(VE.LONGEXCON _) => true
			 | None => true)
		  (*why `true' and not `false' or `impossible ...'?  see my diary
		   19/12/1996 14:17. tho.*)
		  val b = DecGrammar.expansive harmless_con exp
		in
		  List.foldR
		    (fn id => fn m => FinMap.add (id, b, m))
		       FinMap.empty
		         (dom_pat (C,pat))
		end  
	    in
	      (case valbind of
		 PLAINvalbind (_, pat, exp, None) =>
		   makemap pat exp
	       | PLAINvalbind (_, pat, exp, Some valbind) =>
		   let val m1 = makemap pat exp
		       val m2 = isExpansiveId valbind
		   in
		     FinMap.mergeMap 
		       (fn (b1, b2) => raise BoundTwice VE)
		         m1 m2
		   end
	       | RECvalbind (i, valbind) =>
		   isExpansiveId valbind)
	    end

	  val isExpansiveId_map = isExpansiveId valbind

	  (* isVar is true iff id is a variable in dom VE *)		     
	  fun remake isVar id sigma =
	    let
	      val (_, ty) = TypeScheme.to_TyVars_and_Type sigma
	      val isExp = 
		(case FinMap.lookup isExpansiveId_map id of 
		   None => false
		 | Some b => 
		     (if !Flags.DEBUG_ELABDEC then
			 output (std_out, Ident.pr_id id ^ "'s exp is " 
				 ^ (if not b then "NOT " else "") ^ "expansive\n")
		      else () ;
		      b))
  	    in
	      TypeScheme.close (not (isExp andalso isVar)) sigma
	    end 

	  val VARENV m = VE
	in
	  VARENV (FinMap.ComposeMap
		    (fn (id, LONGVARpriv sigma) =>
		           LONGVARpriv (remake true id sigma)
		      | (id, LONGCONpriv(sigma, cons)) =>
			   LONGCONpriv (remake false id sigma, cons)
		      | (id, LONGEXCONpriv tau) => LONGEXCONpriv tau)
		        m)
	end handle BoundTwice VE => VE
      end
    end (*C*)
  


  
    (*constructor_map --- see comment in signature*)
      
    type constructor_map = (id, TypeScheme) FinMap.map

    structure constructor_map = struct
      val empty = FinMap.empty : constructor_map
      fun add id sigma constructor_map =
            FinMap.add (id, sigma, constructor_map)
      fun in_dom id constructor_map =
            EqSet.member id (FinMap.dom constructor_map)
      fun to_VE (constructor_map : constructor_map) : VarEnv =
	    let val fellow_constructors =
		      (ListSort.sort Ident.< o EqSet.list o FinMap.dom)
			constructor_map
	    in
	      FinMap.Fold
		(fn ((id,sigma), VE) =>
		      VE.add VE id (LONGCONpriv (sigma, fellow_constructors)))
		  VE.empty
		    constructor_map
	    end
    end (*constructor_map*)




    structure Realisation = struct
      val on_TyName                = Realisation.on_TyName
      val on_TyName_set            = Realisation.on_TyName_set
      val on_Type                  = Realisation.on_Type
      val on_TypeFcn               = Realisation.on_TypeFcn
      val on_TypeScheme            = Realisation.on_TypeScheme
      val Id                       = Realisation.Id
      val is_Id                    = Realisation.is_Id
      nonfix oo (*smlnj thinks oo is an infix*)
      val oo                       = Realisation.oo
      val singleton                = Realisation.singleton
      val from_T_and_theta         = Realisation.from_T_and_theta
      val restrict                 = Realisation.restrict

      fun on_VarEnv (phi : realisation) (VE : VarEnv) =
	    VE.map (fn LONGVARpriv sigma =>
		         LONGVARpriv (on_TypeScheme phi sigma)
	             | LONGCONpriv (sigma, cons) =>
			 LONGCONpriv (on_TypeScheme phi sigma, cons)
		     | LONGEXCONpriv tau =>
			 LONGEXCONpriv (on_Type phi tau))  VE

      fun on_TyStr (phi : realisation) (TYSTR {theta, VE}) =
	    TYSTR {theta = on_TypeFcn phi theta, VE = on_VarEnv phi VE}

      fun on_TyEnv (phi : realisation) (TE : TyEnv) =
	    TE.map (on_TyStr phi) TE

      fun on_Env phi (E : Env) =
	    if is_Id phi then E else
	      let val (SE, TE, VE) = E.un E
	      in
		E.mk (on_StrEnv phi SE, on_TyEnv phi TE, on_VarEnv phi VE)
	      end

      and on_StrEnv phi (SE : StrEnv) =
	    if is_Id phi then SE else SE.map (on_Env phi) SE

      (*renaming T = a realisation that maps each tyname in T to a fresh tyname*)
      fun renaming' (T : TyName.Set.Set) : TyName.Set.Set * realisation  =
	let
	  val new_tynames : (TyName * TyName) list =
		TyName.Set.fold
		  (fn n : TyName => fn names =>
		   let
		     val n' = TyName.freshTyName
				{tycon = TyName.tycon n, arity= TyName.arity n,
				 equality= TyName.equality n}
		   in
		     (n, n') :: names
		   end) [] T

	  fun mkphis (m, m') phi =
		oo (phi, singleton (m, TypeFcn.from_TyName m'))
	  fun buildphi [] = Id
	    | buildphi tynames = List.foldL mkphis Id tynames
	in
	  (TyName.Set.fromList(map #2 new_tynames),  buildphi new_tynames)

	  (*TODO 25/01/1997 22:06. tho.:
	   mon ikke man kan lave det samme med:

	   val ts = TyName.Set.list T
	   val phis = map
		       (fn t => mkphi(t, (TyName_in_TypeFcn o TyName.freshTyName)
						{tycon = TyName.tycon n, arity= TyName.arity n,
						 equality= TyName.equality n}))
			ts
	   val phi = foldL/R
		       (fn phi1 => fn phi2 => oo_phi(phi2,phi1))
		       id_phi phis
		       (*betyder rækkefølgen noget?*)
	   in
	     phi
	   *)
	end

      fun renaming (T: TyName.Set.Set) : realisation = #2 (renaming' T)
      val enrich = Realisation.enrich
      val layout = Realisation.layout

    end (*Realisation*)

    (*ABS and ABS'*)
    local
      open Realisation
      infix oo

      fun abstract_tystr (TYSTR {theta, ...}) =
	let val tyname = noSome (TypeFcn.to_TyName theta) "modifyTyStr"
	    val tyname_abs = TyName.freshTyName {tycon=TyName.tycon tyname,
						 arity=TyName.arity tyname,
						 equality=false}
	    val phi = singleton(tyname_abs, TypeFcn.from_TyName tyname)
	    val phi_inv = singleton(tyname, TypeFcn.from_TyName tyname_abs) 
	in (phi, phi_inv)
	end
	
      fun abstract (TE: TyEnv) =
	TE.fold (fn tystr => fn (phi, phi_inv) =>
		 let val (phi', phi_inv') = abstract_tystr tystr
		 in (phi oo phi', phi_inv oo phi_inv')
		 end) (Id,Id) TE

      fun strip (TE : TyEnv) : TyEnv =
	TE.map (fn (TYSTR {theta, ...}) => TYSTR {theta = theta, VE = VE.empty}) TE
    in
      fun ABS (TE : TyEnv, E : Env) : Env * realisation =
	    let val TE' = strip TE
	        val (phi, phi_inv) = abstract TE'
		val E' = E.plus (E.from_TE TE', E)
		val E' = on_Env phi_inv E'
	    in (E', phi)
	    end
    end (*local*)

    (*maximise_equality_in_VE_and_TE (VE, TE) = maximise equality in TE.
     VE is also modified so that it contains the correct equality
     attributes.  Only used by ElabDec, rule 19 and 20, and ElabTopdec,
     rule 71.  The side condition in rules 19 and 71 demands that TE
     maximises equality.  In the implementation, the maximisation is done
     after the elaboration of the datbind or the datdesc, and means that
     the equality attributes of tynames in TE are changed.  This is done
     with a realisation.  The result of elaborating a datbind or a
     datdesc is not only a TE, but also a VE, and therefore the
     realisation must also be applied to the VE:*)

    fun maximise_equality_in_VE_and_TE (VE : VarEnv, TE : TyEnv)
          : VarEnv * TyEnv =
          let val phi = TE.equality_maximising_realisation TE
	  in
	    (Realisation.on_VarEnv phi VE, Realisation.on_TyEnv phi TE)
	  end

  end; (*Environments*)



(*$TestEnvironments: DEC_GRAMMAR STRID IDENT TYCON TYNAME
                     STATOBJECT PRETTYPRINT SORTED_FINMAP FINMAP
                     TIMESTAMP REPORT FLAGS LIST_HACKS CRASH
                     ENVIRONMENTS ENVIRONMENTS *)

functor TestEnvironments(structure DecGrammar: DEC_GRAMMAR
			 structure StrId: STRID
			 structure Ident: IDENT
			   sharing type Ident.strid = StrId.strid
			       and type Ident.id = DecGrammar.id
		               and type Ident.longid = DecGrammar.longid
			 structure TyCon: TYCON
 	                   sharing type TyCon.strid = StrId.strid
			 structure TyName: TYNAME
			 structure StatObject: STATOBJECT
	                   sharing type StatObject.ExplicitTyVar = DecGrammar.tyvar
		               and type StatObject.TyName = TyName.TyName
			 structure PP: PRETTYPRINT
			   sharing type StatObject.StringTree = PP.StringTree
	                 structure SortedFinMap: SORTED_FINMAP
	                   sharing type SortedFinMap.StringTree = PP.StringTree
	                 structure FinMap: FINMAP
			   sharing type FinMap.StringTree = PP.StringTree
	                 structure Timestamp: TIMESTAMP
			 structure Report: REPORT
	                   sharing type SortedFinMap.Report = FinMap.Report = Report.Report
	                 structure Flags: FLAGS
	                 structure ListHacks: LIST_HACKS
			 structure Crash: CRASH ) 
  : sig end =
  struct
      structure Environments : ENVIRONMENTS = 
	Environments(structure DecGrammar = DecGrammar
		     structure Ident = Ident
		     structure TyCon = TyCon
		     structure StrId = StrId
		     structure StatObject = StatObject
		     structure TyName = TyName
		     structure PP = PP
		     structure SortedFinMap = SortedFinMap
		     structure FinMap = FinMap
		     structure Timestamp = Timestamp
		     structure Report = Report
		     structure Flags = Flags
		     structure ListHacks = ListHacks
		     structure Crash = Crash)
  end;
