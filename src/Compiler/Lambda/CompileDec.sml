(*$CompileDec : LAB IDENT CON SCON EXCON TYNAME TYVAR
        DEC_GRAMMAR STATOBJECT ENVIRONMENTS LVARS LAMBDA_EXP
        COMPILER_ENV MATCH_COMPILER ELAB_INFO FINMAP FINMAPEQ
        CRASH FLAGS STATE STATE_MONAD StateMonad PRETTYPRINT
        REPORT COMPILE_DEC*)

(* Notice that this functor refers directly to functor
 * ImperativeStateMonad (or FunctionalStateMonad) --- here we would have liked
 * to have Higher-order Functors!
 *)

(* At some point we should clean up this code.. Constructors are not
 * looked up in the environment anymore.. Also, - it would be nice if
 * primitives were declared in the initial environment (this makes the
 * prelude smaller and better code is generated when compiling
 * multible compilation units (inlining of primitives will happen
 * automatically)). (martin) *)

functor CompileDec(structure Lab: LAB   
		   structure Ident: IDENT
                   structure Con: CON   
                   structure SCon: SCON 
                   structure Excon: EXCON
                   structure TyVar : TYVAR

                   structure TyName: TYNAME

                   structure Grammar: DEC_GRAMMAR
                     sharing type Grammar.lab = Lab.lab
                         and type Grammar.scon = SCon.scon
                         and type Grammar.tyvar = TyVar.SyntaxTyVar
			 and type Grammar.id = Ident.id
			 and type Grammar.longid = Ident.longid

                   structure StatObject : STATOBJECT
                     sharing type StatObject.TyName = TyName.TyName

 		   structure Environments : ENVIRONMENTS
		     sharing type Environments.TypeFcn = StatObject.TypeFcn
		         and type Environments.TypeScheme = StatObject.TypeScheme
		         and type Environments.Type = StatObject.Type
			 and type Environments.TyVar = StatObject.TyVar
			 and type Environments.id = Ident.id

                   structure Lvars: LVARS

                   structure LambdaExp: LAMBDA_EXP
                     sharing type LambdaExp.lvar = Lvars.lvar
                         and type LambdaExp.con = Con.con
                         and type LambdaExp.excon = Excon.excon
                         and type LambdaExp.TyName = TyName.TyName

                   structure CompilerEnv: COMPILER_ENV
                     sharing type CompilerEnv.con = Con.con
                         and type CompilerEnv.excon = Excon.excon
                         and type CompilerEnv.lvar = Lvars.lvar
                         and type CompilerEnv.Type = LambdaExp.Type
			 and type CompilerEnv.id = Ident.id
 
                   structure MatchCompiler: MATCH_COMPILER
                     sharing type MatchCompiler.pat = Grammar.pat
                         and type MatchCompiler.id = Ident.id
                         and type MatchCompiler.lab = Lab.lab
                         and type MatchCompiler.scon = SCon.scon
                         and type MatchCompiler.longid = Ident.longid
                         and type MatchCompiler.lvar = Lvars.lvar
                         and type MatchCompiler.CEnv = CompilerEnv.CEnv

		   structure ElabInfo : ELAB_INFO
                     sharing type Grammar.info = ElabInfo.ElabInfo
                     sharing type MatchCompiler.TypeInfo = ElabInfo.TypeInfo.TypeInfo
                     sharing type ElabInfo.TypeInfo.lab = Lab.lab
                     sharing type ElabInfo.TypeInfo.longid = Ident.longid
                     sharing type ElabInfo.TypeInfo.TyEnv = Environments.TyEnv
                     sharing type ElabInfo.TypeInfo.TyVar = StatObject.TyVar
		     sharing type ElabInfo.TypeInfo.Type = StatObject.Type
                   structure FinMap: FINMAP
                     sharing type MatchCompiler.map = FinMap.map

	           structure FinMapEq: FINMAPEQ

                   structure Flags : FLAGS
                   structure PrettyPrint : PRETTYPRINT
                     sharing type PrettyPrint.StringTree = CompilerEnv.StringTree
		       = LambdaExp.StringTree = ElabInfo.StringTree
                   structure Report : REPORT
                     sharing type Report.Report = PrettyPrint.Report

                   structure Crash: CRASH

                  ): COMPILE_DEC =
  struct
    open MatchCompiler LambdaExp
    open Grammar

    (*import from StatObject:*)
    structure TyVar        = StatObject.TyVar
         type TyVar        = StatObject.TyVar
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure TypeFcn      = StatObject.TypeFcn

    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE

    (*import from otherwhere:*)
    structure OverloadingInfo = ElabInfo.OverloadingInfo
    structure TypeInfo = ElabInfo.TypeInfo
    structure CE = CompilerEnv
    structure TLE = LambdaExp

    infix plus
    val (op plus) = CE.plus

    fun die s = Crash.impossible ("CompileDec." ^ s)

    val region_profiling = Flags.lookup_flag_entry "region_profiling"
    (* ----------------------------------------
     * Environment functions
     * ---------------------------------------- *)

    fun declareExcon(id,excon,CE) = CE.declareExcon(id,excon,CE)
    fun declareCon(id,(con,it),CE) = CE.declareCon(id,(con,it),CE)
    fun lookupLongexcon CE longid =
      case Ident.decompose longid
	of ([], id) => (case CE.lookupId CE id
			  of CE.EXCON excon => excon
			   | _ => die "lookupLongexcon.not in env as EXCON")
	 | _ => die "lookupLongexcon.Modules not supported"
    fun lookupLongcon' CE longid =
      case Ident.decompose longid
	of ([], id) => CE.lookupId CE id
	 | _ => die "lookupLongcon.Modules not supported"
    fun lookupLongcon CE longid =
      case Ident.decompose longid
	of ([], id) => (case CE.lookupId CE id
			  of CE.CON(con,it) => (con,it) 
			   | _ => die "lookupLongcon.not in env as CON")
	 | _ => die "lookupLongcon.Modules not supported"
    fun lookupLongvar CE longid =
      case Ident.decompose longid
	of ([], id) => CE.lookupId CE id
	 | _ => die "lookupLongexcon.Modules not supported"

    fun lookupLongid CE longid =
      case Ident.decompose longid
	of ([], id) => CE.lookupId CE id
	 | _ => die "lookupLongid.Modules not supported"

    local 
      open TLE 
    in
      val TLEunit = PRIM(RECORDprim,[])

        (* raiseMatch and raiseBind: 
         *   depends on the way we compile exceptions --- for "_1", c.f. 
         * function new_excon below 
         * XXX UNFINISHED: Match and Bind should be built-in exception constructors
	 * Fixed now, - Match and Bind are initially in env (and in functor Excon)..
         *)

      fun raiseMatch tau' = RAISE (PRIM(EXCONprim Excon.ex_MATCH, []), Types [tau'])
      val raiseBind = RAISE (PRIM(EXCONprim Excon.ex_BIND, []), RaisedExnBind)

      fun monoLet((lv,tau,lamb1),lamb2) =
        LET{pat=[(lv,[],tau)],
            bind=lamb1,
            scope=lamb2}

      fun polyLet tyvars ((lv,tau,lamb1),lamb2) = 
        LET{pat=[(lv,tyvars,tau)],
            bind=lamb1,
            scope=lamb2}
    end

    (* ---------------------------------------------------------------------- *)
    (*               Monadic manipulations of State                           *)
    (* ---------------------------------------------------------------------- *)

    type datbind_list  = (TLE.tyvar list * TyName * (con * LambdaExp.Type Option) list) list
    type SemanticTyVarEnv = (TyVar * TLE.tyvar) list
      local
(*
 * XXX
 *
 * CHANGE to use FinMapEq
 *
 *)
        fun isin(x,[]) = false
          | isin(x,(x',y)::rest) = TyVar.eq (x,x') orelse isin(x,rest)
        fun ins (x, y, nil) = [(x, y)]
          | ins (x', y', (p as (x, y)) :: rest) = 
            if TyVar.eq (x,x') then (x', y') :: rest
            else p :: ins(x', y', rest)
        fun add (x, y, l) = 
          if isin(x,l) then ins(x,y,l)
          else (x,y)::l
        nonfix plus
        fun plus (l, []) = l
          | plus (l, (x, y) :: tl) = plus(add(x, y, l), tl)
        fun lookup [] x  = None
          | lookup ((x,y)::rest) x' =
            if TyVar.eq (x,x') then Some(y) else lookup rest x'
      in
        val SemanticTyVarEnvPlus = plus
        val lookupSemanticTyVarEnv = lookup
        val addSemanticTyVarEnv = add
        val emptySemanticTyVarEnv = []
      end
      
    (* 
     * Monads are used to structure the compilation code, in particular the 
     * propagation of state. SemanticTyVarEnv is part of the state since
     * the declaration of semantic type variables does not follow the scope rules;
     * consider, e.g. the expression (e1,e2); a type variable can be declared in 
     * e1 and must be available for e2. 
     * The monadic operations (signature STATE_MONAD) have been implemented 
     * in both purely functional and in imperative style. The two implemantations do
     * not have exactly the same external behaviour; in order to get the same 
     * behaviour it is at present necessary to use 'resetS'; c.f. 'val compileDec'
     * in the bottom of this module.
     * To use the functional implementation, use functor 'FunctionalStateMonad',
     * to use the imperative implementation, use functor 'ImperativeStateMonad'.
     *
     * -- Lars
     *)

    structure StateMonad = ImperativeStateMonad
      (structure State = struct
         type state = (datbind_list list * SemanticTyVarEnv)
	 val initialState = ([[]],[])
	 
	 (*re plus_datbindss: the state contains a list of lists of
	  datbinds: one datbind list from each datatype declaration.  When
	  adding a datbind list to this list of datbind lists, that datbind
	  list is thinned out removing all datbinds that define a tyname
	  that is already defined in the current state.  Why would the same
	  tyname occur twice?  Because of datatype replication:

	      datatype t = K of int
	      datatype s = datatype t

	  In this example, we only want a singleton datbind list list (I
	  think).  plus_datbindss is not associative.  Maybe that is not a
	  problem.  06/03/1997 14:49. tho.*)

	 local
	   fun datbind_to_tyname (tyvars, tyname, con_type_opt_s) = tyname
	   nonfix plus
	   val concat = List.foldL (General.curry op @) []
	   (*thin ts datbinds = remove all datbinds from `datbinds' that
	    define a tyname in ts:*)
	   fun thin ts datbinds =
	         List.dropAll
		    (fn datbind =>
		       List.exists
		         (fn t => TyName.eq (t, datbind_to_tyname datbind))
			    ts)
		       datbinds
	   fun plus_datbindss datbindss1 datbindss2 =
	         map ((thin o map datbind_to_tyname o concat) datbindss2) datbindss1
		 @ datbindss2
	 in
	   fun plus ((db,m),(db',m')) = (plus_datbindss db db',
					 SemanticTyVarEnvPlus(m,m'))
	 end
       end)
    open StateMonad
    infix bindS ooS 
    fun append_datbinds (db : datbind_list) : unit S = 
          plusS (unitS ([db], emptySemanticTyVarEnv))
    val get_datbinds = getS (unitS ())
    fun lookupSemanticTyVar tyvar : TLE.tyvar S = 
      (getS (unitS ())) bindS (fn s =>
      let 
        val (datbind_list,SemanticTyVarEnv) = s
      in
        case lookupSemanticTyVarEnv SemanticTyVarEnv tyvar of
          None => 
            let val lambdaTyVar = if TyVar.equality tyvar then TLE.fresh_eqtyvar()
				  else TLE.fresh_tyvar ()
            in
	       (plusS (unitS([], addSemanticTyVarEnv(tyvar,lambdaTyVar,emptySemanticTyVarEnv))))
	       bindS (fn () => unitS lambdaTyVar)
            end
        | Some lamdaTyVar => unitS lamdaTyVar
      end)

(*old: now in Grammar
    (* finding the string name of a topmost value identifier in a pattern, if any exists: *)

    fun find_topmost_id (ATPATpat(_, atpat)): string Option = find_in_atpat atpat
      | find_topmost_id (LAYEREDpat(_,OP_OPT(id, _),_,_)) = Some(Ident.pr_id id)
      | find_topmost_id _ = None

    and find_in_atpat (LONGIDatpat(_,OP_OPT(longid,_))) = Some(Ident.pr_longid longid)
      | find_in_atpat (PARatpat(_,pat)) = find_topmost_id pat
      | find_in_atpat _ = None
old*)


    (* ---------------------------------------------------------------------- *)
    (*           Extract type info from syntactic objects                     *)
    (* ---------------------------------------------------------------------- *)

    local 
      open TypeInfo
    in
      fun type_of_exp (exp: Grammar.exp) : StatObject.Type =
        case ElabInfo.to_TypeInfo (get_info_exp exp) of 
          Some(EXP_INFO{Type}) => Type
        | _ => die "type_of_exp"

      fun type_of_match (match: Grammar.match) : StatObject.Type =
        case ElabInfo.to_TypeInfo (get_info_match match) of 
          Some(MATCH_INFO{Type}) => Type
        | _ => die "type_of_match"
    end      

    (* ---------------------------------------------------------------------- *)
    (*                     Utilities                                          *)
    (* ---------------------------------------------------------------------- *)

    fun makeList (f: 'a -> 'a Option) (x: 'a) =
      x :: (case f x of Some y => makeList f y
                      | None => nil)

    fun NoSome errmsg x =
      case x of 
        None => die errmsg
      | Some y => y

    fun zip3(hd :: tl, hd' :: tl', hd'' :: tl'') =
          (hd, hd', hd'') :: zip3(tl, tl', tl'')
      | zip3(nil, nil, nil) = nil
      | zip3 _ = die "zip3"

    local
      fun unzip3'((x, y, z) :: rest, xs, ys, zs) =
            unzip3'(rest, x :: xs, y :: ys, z :: zs)
        | unzip3'(nil, xs, ys, zs) = (xs, ys, zs)
    in
      fun unzip3 triples = unzip3'(rev triples, nil, nil, nil)
    end

    fun zip4(hd :: tl, hd' :: tl', hd'' :: tl'', hd''' :: tl''') =
          (hd, hd', hd'', hd''') :: zip4(tl, tl', tl'', tl''')
      | zip4(nil, nil, nil, nil) = nil
      | zip4 _ = die "zip4"

    fun mk_env declare (xs,ys) =
      List.foldR (fn (x,y) => fn env => declare(x,y,env))
                 CE.emptyCEnv
                 (ListPair.zip(xs,ys))

    fun mk_env' declare (xs,ys,zs) =
      List.foldR (fn (x,y,z) => fn env => declare(x,y,z,env))
      CE.emptyCEnv (zip3(xs,ys,zs))

    fun pr_list (pr: 'a -> string) (l : 'a list) : string =
      let fun pr_l [] = ""
	    | pr_l (a::b::rest) = pr a ^ "," ^ pr_l (b::rest)
	    | pr_l [a] = pr a
      in "[" ^ pr_l l ^ "]"
      end


    (* ---------------------------------------------------------------------- *)
    (*           Utility functions used to compile constructors               *)
    (* ---------------------------------------------------------------------- *)

    fun compileExCon (id:id) : excon = Excon.mk_excon(Ident.pr_id id)

    fun compileCon (id:id) : con = Con.mk_con(Ident.pr_id id)


    (* ---------------------------------------------------------------------- *)
    (*         Compilation of the semantic objects Type and TypeScheme        *)
    (* ---------------------------------------------------------------------- *)

    (*compileTyName is necessary because char and word from
     CompileDec onwards are treated as int.*)

    fun compileTyName tyname =
          if TyName.eq (tyname, TyName.tyName_CHAR)
	  orelse TyName.eq (tyname, TyName.tyName_WORD) 
	  then TyName.tyName_INT
	  else tyname

    fun compileType (typ: StatObject.Type) : LambdaExp.Type S =
        case Type.to_RecType typ of
          None =>
            (case Type.to_ConsType typ of 
               None => 
                 (case Type.to_FunType typ of 
                    None => 
                      (case Type.to_TyVar typ of
                         None => die "compileType(1)"
                       | Some tyvar => 
			     (lookupSemanticTyVar tyvar) bindS (fn lambdaTyVar =>
			      ( (* print ("C[" ^ TyVar.string tyvar ^ "] = "
				          ^ TLE.pr_tyvar lambdaTyVar ^ "\n"); *)
			       unitS(TYVARtype lambdaTyVar))))
                  | Some funtype => 
                      let
                        val (ty1,ty2) = NoSome "compileType(2)" 
                                        (Type.un_FunType funtype)
                      in 
                        (compileType ty1) bindS (fn ty1' =>
                        (compileType ty2) bindS (fn ty2' => 
                        unitS(ARROWtype([ty1'],[ty2']))))
                      end)
             | Some constype =>
                 let
                   val (tys,tyname) = NoSome "compileType(3)" 
                                      (Type.un_ConsType constype)
                 in
                   (mapList compileType tys) bindS (fn tys' =>
                   unitS(CONStype(tys', compileTyName tyname)))
                 end)
        | Some rho =>
            let
              val labtys = Type.RecType.to_list rho
            in
              (mapList (compileType o #2) labtys) bindS (fn tys' =>
              unitS(RECORDtype tys'))
            end

      val compileType = fn typ =>
        (if !Flags.DEBUG_COMPILER andalso false then 
           output(std_out,"compileType: typ = " ^ Type.string typ ^ "\n")
         else ();
         compileType typ)

      val domType : StatObject.Type -> StatObject.Type =
        #1 o (NoSome "domType(2)") o Type.un_FunType  
        o (NoSome "domType(1)") o Type.to_FunType

      fun compileTypeScheme (tyvars : TyVar list, Type : StatObject.Type)
                  : (TLE.tyvar list * TLE.Type) S  =
          (mapList lookupSemanticTyVar tyvars) bindS (fn tyvars' =>
          (compileType Type) bindS (fn tau' => 
          unitS(tyvars',tau')))

      fun compileTyVar env tyvar : TLE.tyvar S =
        (lookupSemanticTyVar tyvar) bindS (fn lambdaTyVar =>
        unitS(lambdaTyVar))


    (* ---------------------------------------------------------------------- *)
    (*	       Compilation and normalization of constructor bindings          *)
    (* ---------------------------------------------------------------------- *)

    (* We normalize constructor bindings such that if
     *
     *      CE =  {A |-> \/ab.a -> (a,b) t,
     *             B |-> \/ba.a -> (b,a) t}
     * we get:
     *
     *      CE' = {A |-> \/ab.a -> (a,b) t,
     *             B |-> \/ab.b -> (a,b) t}
     * or:
     *
     *      ([a,b], t, [(A, Some a), (B, Some b)])
     *)

   type subst = (TLE.tyvar, TLE.tyvar) FinMap.map

   fun mk_S [] = FinMap.empty
     | mk_S ((a,b)::xs) = FinMap.add(a,b,mk_S xs)

   fun on_Type (S:subst) (Type:Type) : Type =
     let fun on (ARROWtype(taus1, taus2)) = ARROWtype(map on taus1, map on taus2)
	   | on (CONStype(taus,tyname)) = CONStype(map on taus, tyname)
	   | on (TYVARtype tv) = TYVARtype (case FinMap.lookup S tv
					      of Some tv' => tv'
					       | None => tv)
	   | on (RECORDtype taus) = RECORDtype(map on taus)
     in on Type
     end

   fun on_TypeOpt _ None = None
     | on_TypeOpt S (Some tau) = Some (on_Type S tau)

   fun unTyVarType (TYVARtype tv) = tv
     | unTyVarType _ = die "unTyVarType"

   fun compile_and_normalize_cb tyname tyvars0 (con, Type) (env, cons_TypeOpts) 
     : CE.CEnv * (con * Type Option) list =
     let val con' = compileCon con
       val (tyvars, tauOpt) =
	 case Type 
	   of ARROWtype([tau],[CONStype(taus,_)]) => (map unTyVarType taus, Some tau)
	    | CONStype(taus,_) => (map unTyVarType taus, None)
	    | _ => die "compile_and_normalize_cb.wrong type"
       val S = mk_S (ListPair.zip (tyvars, tyvars0)) handle ListPair.Zip => 
	              die "compile_and_normalize_cb.wrong number of tyvars"
       val tauOpt' = on_TypeOpt S tauOpt
       val it : CE.instance_transformer = CE.mk_it tyvars tyvars0
     in
       (declareCon(con,(con', it),env), (con', tauOpt') :: cons_TypeOpts)
     end

    (* ---------------------------------------------------------------------- *)
    (*	       Compilation of the semantic object (tyname, VE)                *)
    (*	       (i.e., almost a TyStr)                                         *)
    (* ---------------------------------------------------------------------- *)

    fun compile'TyStr' (tyname : TyName, VE : VarEnv) 
      : CE.CEnv * TLE.tyvar list * (con * Type Option) list =
      let
	val tyvars : TyVar list = 
	  let exception H of TyVar list
	  in (VE.CEfold (fn typescheme => fn cbs =>
			  let val (tyvars, _) = TypeScheme.to_TyVars_and_Type typescheme
			  in raise H tyvars
			  end) [] VE) handle H tyvars => tyvars
	  end
	val tyvars' = map (fn _ => TLE.fresh_tyvar ()) tyvars
	val semanticTyVarEnv =
	  let fun f [] [] = emptySemanticTyVarEnv
		| f (tv::tvs) (tv'::tvs') = addSemanticTyVarEnv(tv,tv',f tvs tvs')
		| f _ _ = die "compile'TyStr'.tyvar"
	  in f tyvars tyvars'
	  end
	val _ = plusS (unitS ([], semanticTyVarEnv))
	val cbs : (id * Type) list = 
	  VE.CEFold (fn (con, typescheme) => fn cbs =>
		      let val (_, tau) = TypeScheme.to_TyVars_and_Type typescheme
		      in (compileType tau) bindS (fn tau' =>
			 (con, tau') :: cbs)
		      end) [] VE 
	val (env, cons_TypeOpts) =
	  List.foldL (compile_and_normalize_cb tyname tyvars') (CE.emptyCEnv,[]) (rev cbs)
      in
	(env, tyvars', cons_TypeOpts)
      end


    (* ---------------------------------------------------------------------- *)
    (*	       Compilation of the semantic object TyEnv (TE)                  *)
    (* ---------------------------------------------------------------------- *)

       (* A datatype declaration is compiled by compiling the semantic
        * object TyEnv associated with it. The type environment
        * (TyEnv) has been glued (as type info) to the syntactic
        * construct of the datatype declaration during elaboration.
        * Notice that the syntactic restrictions (see p. 9, The Def.)
        * ensures that no constructor identifier is bound twice by the
        * same datbind. *)

    fun compileTyEnv (TyEnv : TyEnv) : CE.CEnv * datbind_list =
      let
	(* type structures in TyEnv with non-empty VE *)
	val tystr_list : (TyName * VarEnv) list = 
	      TE.fold (fn tystr => fn tystr_list => 
		      if VE.is_empty (TyStr.to_VE tystr) then tystr_list
		      else let val tyname = (NoSome "TypeFcn not simple" o 
					     TypeFcn.to_TyName o TyStr.to_theta) tystr
			       val VE = TyStr.to_VE tystr
			   in (compileTyName tyname, VE)::tystr_list
			   end) [] TyEnv
      in
	List.foldL (fn (tyname, VE) => fn (env', datbind_list) =>
		    let val (env'', tyvars, cbs) = compile'TyStr' (tyname, VE)
		    in (env' plus env'', (tyvars, tyname, cbs) :: datbind_list)
		    end) (CE.emptyCEnv, []) tystr_list
      end


   (* two kinds of failure from a compiled match: RAISEMATCH means raise the
      Match exception, and RAISESELF means raise the original packet. *)

    datatype FailType = RAISEMATCH of TLE.Type | RAISESELF of TLE.Type

   (* envOfDecTree - for `val <pat> = <exp>' bindings, we want the environment
      to pass out for use in the scope of the declaration. So, we guddle
      around the decision tree to look for it. This scheme will fail for
      general pattern matching tasks in things like `fn <match>' if a rule
      can be reached via two or more decision paths; but, we do the environment
      management for that differently anyway. *)

    fun envOfDecTree tree: CE.CEnv =
      let
        open MatchCompiler
        exception Gotcha of CE.CEnv

        fun envOfDecTree' tree: unit =  (* raise Gotcha(env) when found. *)
          case tree
            of LAB_DECOMPOSE{child, ...} => envOfDecTree' child
             | CON_DECOMPOSE{child, ...} => envOfDecTree' child
             | EXCON_DECOMPOSE{child, ...} => envOfDecTree' child

             | CON_SWITCH{selections, wildcard, ...} =>
                 (case wildcard
                    of None => ()
                     | Some w => envOfDecTree' w;

                  FinMap.fold
                    (fn ((_, t), _) => envOfDecTree' t) ()
                    (selections:
                       ((*eqtype*) id, (TypeInfo * DecisionTree))
                         FinMap.map
                    )
                 )

             | SCON_SWITCH{selections, wildcard, ...} =>
                 (envOfDecTree' wildcard;       (* most likely case... *)

                  FinMap.fold
                    (fn (t, _) => envOfDecTree' t) ()
                    (selections:
                       ((*eqtype*) SCon.scon, DecisionTree) FinMap.map
                    )
                 )

             | EXCON_SWITCH{selections, wildcard, ...} =>
                 (envOfDecTree' wildcard;       (* most likely case... *)
                  map (fn (_, t) => envOfDecTree' t) selections;
                  ()
                 )

             | END{environment, ...} =>
                 raise Gotcha environment

             | FAIL => ()
      in
        (envOfDecTree' tree; die "envOfDecTree")
        handle Gotcha e => e
      end


    (* ---------------------------------------------------------------------- *)
    (*                        Primitives                                      *)
    (* ---------------------------------------------------------------------- *)

    val dummyType = {instance=RECORDtype[]}
      (* Used to make the lookupPrim function well-typed, bit of a hack *)

    fun lookupPrim i =
          (case i of
	     0 => EQUALprim dummyType
	   | 1 => FLOORprim
	   | 2 => REALprim
	   | 3 => SQRTprim
	   | 4 => SINprim
	   | 5 => COSprim
	   | 6 => ARCTANprim
	   | 7 => EXPprim
	   | 8 => LNprim
	   | 9 => SIZEprim
	   | 10 => CHRprim
	   | 11 => ORDprim
	   | 12 => EXPLODEprim
	   | 13 => IMPLODEprim
	   | 14 => DIV_REALprim
	   | 15 => DIV_INTprim
	   | 16 => MODprim
	   | 17 => ASSIGNprim dummyType
	   | 18 => DEREFprim dummyType
	   | 19 => OPEN_INprim
	   | 20 => OPEN_OUTprim
	   | 21 => INPUTprim
	   | 22 => LOOKAHEADprim
	   | 23 => CLOSE_INprim
	   | 24 => END_OF_STREAMprim
	   | 25 => OUTPUTprim
	   | 26 => CLOSE_OUTprim
	   | 27 => USEprim
	   | 28 => FLUSH_OUTprim
	   | 29 => STD_INprim
	   | 30 => STD_OUTprim
	   | 31 => CCALLprim ("dummyString", dummyType)
	   | _ => die ("lookupPrim " ^ Int.string i))


    (* ---------------------------------------------------------------------- *)
    (*         Primitives for overloaded arithmetic operators                 *)
    (* ---------------------------------------------------------------------- *)

    local 
      fun int_or_real info (int, real) =
	    (case NoSome "int_or_real" (ElabInfo.to_OverloadingInfo info) of
	       OverloadingInfo.RESOLVED_INT => int
	     | OverloadingInfo.RESOLVED_REAL => real
	     | OverloadingInfo.RESOLVED_WORD => int
	     | OverloadingInfo.RESOLVED_CHAR => int
	     | OverloadingInfo.RESOLVED_STRING => die "int_or_real: string"
	     | OverloadingInfo.UNRESOLVED _ => die "int_or_real: unresolved")
      fun string_or_int_or_real info (int, real, string) =
	    (case NoSome "string_or_int_or_real" (ElabInfo.to_OverloadingInfo info) of
	       OverloadingInfo.RESOLVED_INT => int
	     | OverloadingInfo.RESOLVED_REAL => real
	     | OverloadingInfo.RESOLVED_WORD => int
	     | OverloadingInfo.RESOLVED_CHAR => int
	     | OverloadingInfo.RESOLVED_STRING => string
	     | OverloadingInfo.UNRESOLVED _ => die "string_or_int_or_real: unresolved")
      fun unoverload i CE.ABS = int_or_real i (ABS_INTprim, ABS_REALprim)
	| unoverload i CE.NEG = int_or_real i (NEG_INTprim, NEG_REALprim)
	| unoverload i CE.PLUS = int_or_real i (PLUS_INTprim, PLUS_REALprim)
	| unoverload i CE.MINUS = int_or_real i (MINUS_INTprim, MINUS_REALprim)
	| unoverload i CE.MUL = int_or_real i (MUL_INTprim, MUL_REALprim)
	| unoverload i CE.LESS =
	    string_or_int_or_real i
	      (LESS_INTprim, LESS_REALprim, LESS_STRINGprim)
	| unoverload i CE.GREATER=
	    string_or_int_or_real i
	      (GREATER_INTprim, GREATER_REALprim, GREATER_STRINGprim)
	| unoverload i CE.LESSEQ =
	    string_or_int_or_real i
	      (LESSEQ_INTprim, LESSEQ_REALprim, LESSEQ_STRINGprim)
	| unoverload i CE.GREATEREQ =
	    string_or_int_or_real i
	      (GREATEREQ_INTprim, GREATEREQ_REALprim, GREATEREQ_STRINGprim)
	| unoverload i _ = die "unoverload"
    in
      fun overloaded_prim info result (*e.g., CE.ABS*)
	    compilerAtexp compilerExp (arg: Grammar.atexp) takes_one_argument =
	    if takes_one_argument then
	      (compilerAtexp arg) bindS (fn arg' =>
	      unitS (PRIM (unoverload info result, [arg'])))
	    else
	      (case arg of 
		 RECORDatexp(_,
			     Some(EXPROW(_,_,exp1,
					 Some(EXPROW(_,_,exp2,None))))) =>
		 (compilerExp exp1) bindS (fn exp1' =>
                 (compilerExp exp2) bindS (fn exp2' =>
                 unitS (PRIM (unoverload info result,
			      [exp1',exp2']))))
	       | _ => die "overloaded_prim")

      fun overloaded_prim_fn info result (*e.g., CE.ABS*) takes_one_argument =
	    let val ty = int_or_real info (CONStype ([], TyName.tyName_INT),
					   CONStype ([], TyName.tyName_REAL))
	        val lvar1 = Lvars.newLvar ()
	    in
	      unitS (
	      if takes_one_argument then
		FN {pat=[(lvar1, ty)],
		    body=PRIM (unoverload info result,
			       [VAR {lvar=lvar1, instances=[]}])}
	      else (*takes two arguments*)
		FN {pat=[(lvar1, RECORDtype [ty, ty])],
		    body=PRIM (unoverload info result,
			       [PRIM (SELECTprim 0,
				      [VAR {lvar=lvar1, instances=[]}]),
				PRIM (SELECTprim 1,
				      [VAR {lvar=lvar1, instances=[]}])])})
	    end
    end (*local*)

   (* ---------------------------------------------------------------------- *)
   (*           Decomposition of applications of prim                        *)
   (* ---------------------------------------------------------------------- *)

   (* Decompose the absyn argument in a `prim(n, xxx)' expression. Notice
      that we're extremely intolerant of any deviation from this precise
      syntax. *)

    fun decomposePrimArg atexp: int * exp list =
      let 
        val (primno,arg) =
          case atexp of 
            RECORDatexp(_,
              Some(EXPROW(_, _, ATEXPexp(_, SCONatexp(_, SCon.INTEGER i)),
                          Some(EXPROW(_, _, exp2, None))))) => (i, exp2)
         | _ => die "decomposePrimArg(1)"

	fun decomposeArgs arg =
	  case arg 
	    of None => []
	     | Some(EXPROW(_,_,exp1, arg')) => exp1 :: (decomposeArgs arg')

	fun decomposeExp exp =
	  case exp
	    of ATEXPexp(_,RECORDatexp(_,expRow)) => decomposeArgs expRow
	     | _ => [exp]
      in
        (primno,
         if primno < 0 then die "decomposePrimArg(2)"
         else 
	   decomposeExp arg)
      end

    (* ----------------------------------------------------------------------- *)
    (*               Syntax directed compilation                               *)
    (* ----------------------------------------------------------------------- *)

    fun compileAtexp env atexp : TLE.LambdaExp S =
      case atexp
        of SCONatexp(_, SCon.INTEGER x) => unitS(INTEGER x)
         | SCONatexp(_, SCon.STRING x) => unitS(STRING x)
         | SCONatexp(_, SCon.REAL x) => unitS(REAL x)
         | SCONatexp(_, SCon.CHAR x) => unitS(INTEGER x)
         | SCONatexp(_, SCon.WORD x) => unitS(INTEGER x)

         | IDENTatexp(info, OP_OPT(longid, _)) =>
	  (case lookupLongid env longid
	     of CE.LVAR lv => 
	       let val instances =
		     case ElabInfo.to_TypeInfo info 
		       of Some(TypeInfo.VAR_INFO{instances}) => instances
			| _ => die ("compileAtexp(LVAR..): no type info for "
				    ^ Ident.pr_longid longid)
	       in
		 (mapList compileType instances) bindS 
		 (fn instances' =>
		  unitS(VAR {lvar=lv,instances=instances'}))
	       end
	      | CE.ABS =>       overloaded_prim_fn info CE.ABS       true 
	      | CE.NEG =>       overloaded_prim_fn info CE.NEG       true 
	      | CE.PLUS =>      overloaded_prim_fn info CE.PLUS      false
	      | CE.MINUS =>     overloaded_prim_fn info CE.MINUS     false
	      | CE.MUL =>       overloaded_prim_fn info CE.MUL       false
	      | CE.LESS =>      overloaded_prim_fn info CE.LESS      false
	      | CE.GREATER =>   overloaded_prim_fn info CE.GREATER   false
	      | CE.LESSEQ =>    overloaded_prim_fn info CE.LESSEQ    false
	      | CE.GREATEREQ => overloaded_prim_fn info CE.GREATEREQ false
	      | CE.CON(con,it) =>
	       let
		 val (functional,Type,instances) =
		   case ElabInfo.to_TypeInfo info 
		     of Some (TypeInfo.CON_INFO{Type,instances,...}) => 
		       (Type.is_Arrow Type,Type,CE.apply_it(it,instances))
		      | _ => die "compileAtexp(CON..): no type info"
	       in if functional then
		    let val lv = Lvars.newLvar()
		    in
		      (compileType (domType Type)) bindS (fn tau' =>
		       (mapList compileType instances) bindS 
		       (fn instances' =>
			unitS(FN{pat=[(lv,tau')],
				 body=PRIM(CONprim{con=con, instances=instances'},
					   [VAR{lvar=lv,instances=[]}])})))
		    end
		  else
		    (mapList compileType instances) bindS
		    (fn instances' =>
		     unitS(PRIM(CONprim {con=con, instances=instances'},
				[])))
	       end
	      | CE.REF =>
	       let val (Type,instances) =
		     case ElabInfo.to_TypeInfo info 
		       of Some (TypeInfo.CON_INFO{Type,instances,...}) => (Type,instances)
			| _ => die "compileAtexp(REF..): no type info"
		   val lv = Lvars.newLvar()
		   val instance = case instances 
				    of [x] => x 
				     | _ => die "compileAtexp(REF..): wrong ref instance"
	       in
		 (compileType (domType Type)) bindS (fn tau' =>
                  (compileType instance) bindS (fn instance' =>
                   unitS(FN{pat=[(lv,tau')],
			    body=PRIM(REFprim {instance=instance'},
				      [VAR{lvar=lv,instances=[]}])})))
	       end
	      | CE.EXCON excon =>
	       let
		 val (functional,Type) =
		   case ElabInfo.to_TypeInfo info of 
		     Some (TypeInfo.EXCON_INFO{Type,...}) => 
		       (Type.is_Arrow Type,Type)
		   | _ => die "compileAtexp(EXCON..): no type info"
	       in         
		 if functional then
		   let val lv = Lvars.newLvar()
		   in
		     (compileType (domType Type)) bindS (fn tau' =>
                      unitS(FN{pat=[(lv,tau')],
			       body=PRIM(EXCONprim excon,
					 [VAR{lvar=lv, instances=[]}])}))
		   end
		 else unitS(PRIM(EXCONprim excon,[]))
	       end
	      | _ => die "IDENTatexp.not implemented"
             )

        (* records: the fields must be evaluated in their textual order,
           but the resulting record object must have the fields in a
           canonical order (we adopt alphabetic ordering). Hmm. Tricky.
           Easiest way is to bind the record field expressions to lvars
           and then build a record of the (appropriately ordered) lvars. *)

	 (* Well, - if the labs are already sorted then we can in-line
	    the expressions in the record... 04/10/1996-Martin. *)

         | RECORDatexp(_, Some exprow) =>
             let
               val rows = makeList (fn EXPROW(_, _, _, e) => e) exprow
               val labs = map (fn EXPROW(_, l, _, _) => l) rows
	       val expsS = mapList (fn EXPROW(_, _, e, _) => compileExp env e) rows
	       fun is_sorted (l1::(labs as l2::_)) = Lab.<(l1,l2) andalso is_sorted labs
		 | is_sorted _ = true
	     in if is_sorted labs then
	         expsS bindS (fn exps =>
                 unitS(PRIM(RECORDprim, exps))) 
		else
		  let val tausS = mapList (compileType o type_of_exp 
					   o (fn EXPROW(_,_,e,_) => e)) rows
		      val lvars = map (fn _ => Lvars.newLvar()) rows
		      val scope =              (* The final record expression *)
			let
			  val sortedLvarsXlabs =
			    ListSort.sort
			    (fn (_, l1) => fn (_, l2) => Lab.<(l1, l2))
			    (ListPair.zip(lvars, labs))
			in
			  PRIM(RECORDprim,map (fn (lv, _) => VAR{lvar=lv,instances=[]})
			       sortedLvarsXlabs)
			end
		  in
		    tausS bindS (fn taus =>
		    expsS bindS (fn exps => 
		    unitS(List.foldR (fn (lv,exp,tau) => fn exp' => monoLet((lv,tau,exp),exp'))
                          scope
		          (zip3(lvars,exps,taus)))))
		  end
	     end

         | RECORDatexp(_, None) => unitS(PRIM(RECORDprim,[]))

         | LETatexp(_, dec, exp) =>
             (compileDec env (false,dec)) bindS (fn (env1,f) =>
             (compileExp (env plus env1) exp) bindS (fn exp' =>
             f exp'))


         | PARatexp(_, exp) => compileExp env exp

    and compileExp env exp =
      case exp
        of ATEXPexp(_, atexp) => compileAtexp env atexp

         | APPexp(_,
                  f as ATEXPexp(_, IDENTatexp(info, OP_OPT(longid, _))),
                  arg
                 ) =>
                        (* We have to spot direct application of "prim" - apart
                           from that, we don't have to bother with constructors
                           and the like. They'll compile to functions, but the
                           optimiser will spot them later. *)
	  
	  (case lookupLongvar env longid
	     of CE.LVAR lv =>        (* Not a primitive... *)
	       (compileAtexp env arg) bindS (fn arg' =>
		  (case ElabInfo.to_TypeInfo info 
		     of Some(TypeInfo.VAR_INFO{instances}) =>
		       (mapList compileType instances) bindS 
		       (fn instances' =>
			unitS(APP(VAR{lvar=lv,instances=instances'},arg')))
		      | _ => die "compileExp(APPexp..): wrong type info"))
	      | CE.RESET_REGIONS =>
	       (compileAtexp env arg) bindS (fn arg' =>
		  (case ElabInfo.to_TypeInfo info 
		     of Some(TypeInfo.VAR_INFO{instances = [tau]}) =>
		       (compileType tau) bindS 
		       (fn tau' =>
			unitS(PRIM(RESET_REGIONSprim{instance = tau'}, [arg'])))
		      | _ => die "compileExp(APPexp..): wrong type info"))
	      | CE.FORCE_RESET_REGIONS =>
	       (compileAtexp env arg) bindS (fn arg' =>
		  (case ElabInfo.to_TypeInfo info 
		     of Some(TypeInfo.VAR_INFO{instances = [tau]}) =>
		       (compileType tau) bindS 
		       (fn tau' =>
			unitS(PRIM(FORCE_RESET_REGIONSprim{instance = tau'}, [arg'])))
		      | _ => die "compileExp(APPexp..): wrong type info"))
	      | CE.ABS =>       overloaded_prim info CE.ABS       (compileAtexp env) (compileExp env) arg true 
	      | CE.NEG =>       overloaded_prim info CE.NEG       (compileAtexp env) (compileExp env) arg true 
	      | CE.PLUS =>      overloaded_prim info CE.PLUS      (compileAtexp env) (compileExp env) arg false
	      | CE.MINUS =>     overloaded_prim info CE.MINUS     (compileAtexp env) (compileExp env) arg false
	      | CE.MUL =>       overloaded_prim info CE.MUL       (compileAtexp env) (compileExp env) arg false
	      | CE.LESS =>      overloaded_prim info CE.LESS      (compileAtexp env) (compileExp env) arg false
	      | CE.GREATER =>   overloaded_prim info CE.GREATER   (compileAtexp env) (compileExp env) arg false
	      | CE.LESSEQ =>    overloaded_prim info CE.LESSEQ    (compileAtexp env) (compileExp env) arg false
	      | CE.GREATEREQ => overloaded_prim info CE.GREATEREQ (compileAtexp env) (compileExp env) arg false
	      | CE.PRIM =>   
                             (* Application of `prim'. We must now disassemble the 
                              * argument to get the prim number and the arguments 
                              * to the primitive operation *)
                             let
                               val (n, args) = decomposePrimArg arg
                               val prim = lookupPrim n
                               fun f prim =
                                 (mapList (compileExp env) args) bindS (fn args' =>
                                 (case ElabInfo.to_TypeInfo info of
                                    Some(TypeInfo.VAR_INFO{instances=[instanceRes,instance]}) => 
                                        (* XXX
                                         * This code depends on the order of the
                                         * instances recorded during elaboration.
                                         * We need the instance corresponding to 
                                         * the argument of prim, which in 
                                         * EfficientCoreElab version is the second
                                         *)
                                      (compileType instance) bindS (fn instance' =>
                                      unitS(TLE.PRIM(prim {instance=instance'},
                                                     args')))
                                  | _ => die 
                                      "compileExp(APPexp(PRIM..): wrong \
                                      \type info"))
                             in
                               case prim of
                                 DEREFprim _ => f DEREFprim
                                   (* ref (REFprim) is a constructor, so it does 
                                    * not show up here 
                                    *)
                               | ASSIGNprim _ => f ASSIGNprim
                               | EQUALprim _ => f EQUALprim
                                   (* <> (NOTEQUALprim) is declared in the prelude as an
                                    * ordinary variable (not a primitive), so it does
                                    * not show up here 
                                    *)
			       | CCALLprim _ => 
				   let
				     fun extractString exp =
				       let
					 val atexp = 
					   case exp 
					     of ATEXPexp(_,atexp) => atexp
					      | _ => die "CCALL.exp not atexp" 
					 val scon =
					   case atexp
					     of SCONatexp(_,scon) => scon
					      | IDENTatexp(_,_) => die "CCALL atexp is an identifier"
					      | RECORDatexp(_,_) => die "CCALL atexp is a record"
					      | LETatexp(_,_,_) => die "CCALL atexp is a let"
					      | PARatexp(_,_) => die "CCALL atexp is a par"
				       in 
					 case scon
					   of SCon.STRING s => s
					    | _ => die "CCALL.scon not string"
				       end 

				     val (s, args) = 
				       case args
					 of [] => die "No function name in CCALLprim"
					  | [s] => die "Only one function name in CCALLprim. Remember also function name for profiling."
					  | s::s_prof::xs =>
					     if !region_profiling then
					       (extractString s_prof, xs)
					     else
					       ((  (* Report.print(PP.reportStringTree (GrammarInfo.layoutPostElabGrammarInfo info)); *)
						 extractString s), xs)
				   in
				     (mapList (compileExp env) args) bindS (fn args' =>
				      (case ElabInfo.to_TypeInfo info of
					 Some(TypeInfo.VAR_INFO{instances=[instanceRes,instanceArg]}) => 
					   (* We use the result type of prim *)
					   (* SpreadExp generates reg. vars. from the result type. *)
					   (compileType instanceRes) bindS (fn instance' =>
					    unitS(TLE.PRIM(CCALLprim (s, {instance=instance'}), args')))
				       | _ => die 
					   "compileExp(APPexp(PRIM..): wrong \
					    \type info"))
				   end
                               | _ => 
                                   (mapList (compileExp env) args) bindS (fn args' =>
                                   unitS(TLE.PRIM(prim, args')))
                             end

	      | _ (*CON/EXCON*) => (compileExp env f) bindS (fn f' =>
		                    (compileAtexp env arg) bindS (fn arg' =>
		                     unitS(APP(f',arg'))))
	      )

         | APPexp(_, f, arg) =>         (* non-trivial function expression... *)
             (compileExp env f) bindS (fn f' =>
             (compileAtexp env arg) bindS (fn arg' =>
             unitS(APP(f',arg'))))

         | TYPEDexp(_, exp, _) => compileExp env exp

         | HANDLEexp(_, exp', match) =>
             (compileExp env exp') bindS (fn e1' =>
             (compileType (type_of_exp exp)) bindS (fn tau' =>
             (compileMatch env (match, false,RAISESELF tau')) bindS (fn e2' =>
             unitS(HANDLE(e1',e2')))))

         | RAISEexp(i, exp') => 
             (compileExp env exp') bindS (fn e' =>
             (compileType (type_of_exp exp)) bindS (fn tau' =>
             unitS(RAISE(e',Types [tau']))))

         | FNexp(_, match) => 
             (compileType (type_of_exp exp)) bindS 
              (fn TLE.ARROWtype(_,[tau']) => (* now extracts result type (was just tau'); mads, 30/12/94 *)
                   compileMatch env (match, true, RAISEMATCH tau')
                  | _ => die "compileExp: FNexp did not have (unary) arrow type"
              )

         | UNRES_INFIXexp _ =>  die "compileExp(UNRES_INFIX)"

   (* compileMatch - compiles a match into a FN expression; this is used
      for FNexp expressions and also for handlers. The failure argument
      indicates what to plant for non-matches; RAISEMATCH means plant a lambda
      which raises the Match exception, whereas RAISESELF means raise the
      original packet. `warn' is true if inexhaustiveness warnings are
      required (TRUE for case statement on excons, for example, but FALSE
      for the equivalent in a handler).
        compileMatch is a bit wasteful in that if a rule is reachable more
      than once through a decision tree, the RHS expression will get compiled
      each time. It's rather difficult to abstract this away, since different
      decision paths to a particular rule result in different bindings for
      the pattern variables. This isn't impossible to solve, but I don't
      want to bother doing it. -- Nick *)

    and compileMatch env (match, warn, failure) =
       let
         val matches = makeList (fn MATCH(_, _, m) => m) match
         val pats = map (fn MATCH(_, MRULE(_, pat, _), _) => pat) matches
         val exps = map (fn MATCH(_, MRULE(_, _, exp), _) => exp) matches
             (* We need to compile each exp into a lambda, for which we need 
              * the environment established by the corresponding pattern. 
              *)

         val root = case pats of
                      [pat] =>
                         (case Grammar.find_topmost_id_in_pat pat of
                            Some string => Lvars.new_named_lvar string
                          | _ => Lvars.newLvar())
                    | _ => Lvars.newLvar()

         val decTree =
           matchCompiler(root, pats,
                         {warnInexhaustive=warn, warnNoBindings=false}
                        )

         fun f(n: int, e: CE.CEnv): LambdaExp S =
           compileExp (env plus e) (List.nth (n-1) exps)

         val env' = CE.declareLvar(root,[],env)
         val expS =
            compileDecTree env' (decTree, f,
                                case failure
                                  of RAISEMATCH tau' => raiseMatch tau'
                                   | RAISESELF tau' => RAISE(VAR{lvar=root,instances=[]},
                                                             Types [tau']),
                                false) 
                (* last argument is false as the variables bound in the match
                   are to be FN bound in the lambda language (monomorphic) *)
       in
         expS bindS (fn exp =>
         (compileType (domType (type_of_match match))) bindS (fn tau =>
         unitS(FN{pat=[(root,tau)],
                  body=exp})))
       end

   (* compileDec - takes an enclosing environment and a declaration, and
      returns the environment *for this declaration only*, together with a
      function to apply to the declaration's scope to return the entire
      lambda term. The `topLevel' parameter is only needed because the
      match compiler is expected to report non-binding patterns for
      non top-level val bindings only. *)

    and compileDec env (topLevel, dec): (CE.CEnv * (LambdaExp -> LambdaExp S)) S =
      case dec
        of VALdec(_, tyvars, valbind) =>
             compileValbind env (topLevel, valbind)

         | UNRES_FUNdec _ =>
             die "compileDec(UNRES_FUN)"

         | TYPEdec _ => unitS(CE.emptyCEnv, fn x => unitS x)

         | DATATYPEdec(i,datbind) => 
             (compileDatbind env i) bindS (fn (env1,datbinds) =>
             (append_datbinds datbinds) bindS (fn () =>
             unitS(env1,fn x => unitS x)))

	 | DATATYPE_REPLICATIONdec (i, tycon, longtycon) => 
             (compileDatbind env i) bindS (fn (env1, datbinds) =>
             (append_datbinds datbinds) bindS (fn () =>
             unitS (env1, fn x => unitS x)))

         | ABSTYPEdec(i, datbind, dec) =>
             (compileDatbind env i) bindS (fn (env1,datbinds) =>
             (append_datbinds datbinds) bindS (fn () =>
             (compileDec (env plus env1) (false,dec)) bindS (fn (env2,f) =>
             unitS(env1 plus env2, f))))

         | EXCEPTIONdec(_, exbind) =>
             compileExbind env exbind

         | LOCALdec(_, dec1, dec2) =>
             (compileDec env (false,dec1)) bindS (fn (env1,f1) =>
             (compileDec (env plus env1) (false,dec2)) bindS (fn (env2, f2) =>
             unitS(env2, f1 ooS f2)))

         | OPENdec _ => Crash.unimplemented "compileDec(OPENdec..)"

         | SEQdec(_, dec1, dec2) =>
             (compileDec env (topLevel,dec1)) bindS (fn (env1,f1) =>
             (compileDec (env plus env1) (topLevel,dec2)) bindS (fn (env2,f2) =>
             unitS(env1 plus env2, f1 ooS f2)))

        (* INFIX/NONFIX declarations have no effect on execution. *)

         | INFIXdec _ => unitS(CE.emptyCEnv, fn x => unitS x)
         | INFIXRdec _ => unitS(CE.emptyCEnv, fn x => unitS x)
         | NONFIXdec _ => unitS(CE.emptyCEnv, fn x => unitS x)

         | EMPTYdec _ => unitS(CE.emptyCEnv, fn x => unitS x)

   (* compileValbind - although there may be `rec' prefixes nested anywhere
      in the valbind, the effect is a single non-recursive layer of
      binding, together with a second layer of several distinct recursive
      valbinds. *)

    and compileValbind env (topLevel, valbind)
        : (CE.CEnv * (LambdaExp -> LambdaExp S)) S =
      let
        fun flattenRecValbind vb: 
                       (pat * exp * (TyVar list * StatObject.Type)) list =
          case vb of 
            PLAINvalbind(i, pat, exp, vbOpt) =>
              (case ElabInfo.to_TypeInfo i of 
                 Some (TypeInfo.PLAINvalbind_INFO{Type,tyvars,...}) => 
                   (pat, exp,(tyvars,Type)) :: 
                   (case vbOpt of Some vb => flattenRecValbind vb
                                | None    => nil)

               | _ => die "flattenRecValbind: no type info")

          | RECvalbind(_, vb) =>  flattenRecValbind vb
      in
        case valbind
          of PLAINvalbind(i, pat, exp, None) =>
            (case ElabInfo.to_TypeInfo i of 
                 Some (TypeInfo.PLAINvalbind_INFO{tyvars,Type,...}) => 
                   compileBinding env (topLevel, pat, exp,(tyvars,Type))
               | _ => die "compileValbind: no type info")

           | PLAINvalbind(i, pat, exp, Some vb) =>
               (case ElabInfo.to_TypeInfo i of 
                  Some (TypeInfo.PLAINvalbind_INFO{tyvars,Type,...}) => 
                    (compileBinding env (topLevel, pat, exp,(tyvars,Type))) bindS 
                                                                      (fn (env1,f1) =>
                    (compileValbind env (topLevel,vb)) bindS (fn (envRest,f2) =>
                    unitS(env1 plus envRest, f1 ooS f2)))
                | _ => die "compileValbind: no type info")

           | RECvalbind(_, vb) =>
               let
                 val triples = flattenRecValbind vb
               in
                 compileREC env (unzip3 triples)
               end
      end

    and compileDatbind (env:CE.CEnv) i : (CE.CEnv * datbind_list) S  =
      let
	val (env', datbindlist) = case ElabInfo.to_TypeInfo i 
				    of Some(TypeInfo.DATBIND_INFO {TE}) => compileTyEnv TE 
				     | _ => die "No TyEnv type info for compiling datbind" 
      in
	unitS(env', datbindlist)
      end

    and compileExbind (env:CE.CEnv) exbind
        : (CE.CEnv * (LambdaExp -> LambdaExp S))  S =
      case exbind
        of EXBIND(i, OP_OPT(excon, _), _, rest) =>
             let
	       val tyOpt = case ElabInfo.to_TypeInfo i 
			     of Some(TypeInfo.EXBIND_INFO {TypeOpt}) => TypeOpt 
			      | _ => die "No typeOpt info for compiling exbind" 
               val (env1, f1) = compileNewExn env excon tyOpt
             in
               (case rest
                 of Some exbind' => compileExbind env exbind'
               | None => unitS(CE.emptyCEnv, fn x => unitS x))
                                           bindS (fn (envRest,f2) =>
               unitS(env1 plus envRest, f1 ooS f2))
             end

         | EXEQUAL(_, OP_OPT(excon1, _), OP_OPT(longexcon2, _), rest) =>
             let
               val excon2' = lookupLongexcon env longexcon2
               val env1 = declareExcon(excon1, excon2', CE.emptyCEnv)
               val f1 = fn x => unitS x (* No new code. *)
             in
               (case rest
                  of Some exbind' => compileExbind env exbind'
                   | None => unitS(CE.emptyCEnv, fn x => unitS x))
                                         bindS (fn (envRest,f2) =>
               unitS(env1 plus envRest, f1 ooS f2))
             end

    and compileNewExn env excon tyOpt =
      let
        val excon' = compileExCon excon
        val LambdaTypeOpt =
          case tyOpt of
            None => None 
          | Some tau => Some ((compileType tau) bindS (fn tau' => tau'))
        val env1 = declareExcon(excon,excon',CE.emptyCEnv)
        val f1 = fn x =>
          unitS(EXCEPTION(excon',LambdaTypeOpt, x))
      in
        (env1,f1)
      end

  (* compileBinding - we're compiling something like
        `let val pat = exp in scope end'.
      compileBinding returns the environment established by `pat',
      and a function: LambdaExp->LambdaExp, which is applied to the
      `scope' expression to yield the entire let expression. This
      generalises to parallel (`and'-linked) bindings. As long as we
      get the environment handling right (and don't make the thing
      `rec' by mistake), we can finally just cascade together the result
      functions returned by compileBinding.
      sigma is the type scheme of the exp, recorded during elaboration. *)

    and compileBinding env (topLevel, pat, exp, (tyvars,Type))
        : (CE.CEnv * (LambdaExp -> LambdaExp S)) S =
      let
        val root = (* Root of the pattern. *)
                   case Grammar.find_topmost_id_in_pat pat of
                     None => Lvars.newLvar()  
                   | Some string => Lvars.new_named_lvar string

        val decTree =
               matchCompiler(root, [pat],
                             {warnInexhaustive=false, warnNoBindings=not topLevel})
            (* Decision tree which takes the root apart according to the pattern. *)

        val env1 = envOfDecTree decTree
            (* The identifier environment generated by this (single) pattern. *)

(*
	fun fresh_lambda_type() = TLE.TYVARtype(TLE.fresh_tyvar())   (* this is a hack!! Expressions with this
								      * in it doesn't type-check!! *)
*)
      in
        unitS(env1, 
             (fn scope =>
                    (compileExp env exp) bindS (fn exp' =>
                    (mapList lookupSemanticTyVar tyvars) bindS (fn tyvars' =>
                    (compileType Type) bindS (fn tau' => 
                    (compileDecTree (CE.declareLvar(root,map TYVARtype tyvars',env))
                                    (decTree, fn _ => unitS(scope),raiseBind,true)) 
                         (* changed tau' to fresh_lambda_type() in previous line;
                            30/12/94, mads*)
                                        bindS (fn exp'' => 
                    unitS(LET{pat=[(root,tyvars',tau')],
                              bind=exp',
                              scope=exp''}
                          )))))))

        (* Given the final scope of this declaration, we can
         compile the declaration tree such that `scope'
         appears in the scope of the pattern. Weird side-
         effect that the compilation is passed out as a
         suspension. Note that the last argument to compileDecTree
         is true, since variables bound by the pattern pat
         should be LET bound in the lambda language to 
         allow polymorphism. *)
      end

   (* compileREC - compile a list of `rec' pattern/expression pairs. The
      patterns must all be variables (and aren't even allowed to be
      bracketted??), and the RHS's must all be lambdas. Type constraints
      are allowed, though. Returns the rec env only, plus `fn scope -> lexp'. *)

    and compileREC env (pats, exps, sigmas): (CE.CEnv * (LambdaExp -> LambdaExp S)) S =
      let
        fun varOfPat(TYPEDpat(_, pat, _)) = varOfPat pat
          | varOfPat(ATPATpat(_, LONGIDatpat(_, OP_OPT(longid, _)))) =
              (case Ident.decompose longid
                 of (nil, id) => id
                  | _ => die("compileREC.varOfPat(long: "
			     ^ Ident.pr_longid longid ^ ")"))
          | varOfPat _ = die "compileREC.varOfPat"

        val ids = map varOfPat pats
        val lvars = map (fn id => Lvars.new_named_lvar(Ident.pr_id id)) ids

        val recEnv: CE.CEnv =  mk_env CE.declareVar (ids,lvars)

        fun mk_bindings lvars sigmas' binds =
          map (fn (lvar,(tyvars,Type),bind) => {lvar = lvar,
						tyvars=tyvars,
						Type=Type,
						bind=bind})
          (zip3(lvars,sigmas',binds))

      in
        (mapList (compileExp (env plus recEnv)) exps) bindS (fn lexps =>
        (mapList compileTypeScheme sigmas) bindS (fn sigmas' =>
        unitS(recEnv, 
              fn scope => 
                unitS(FIX({functions=mk_bindings lvars sigmas' lexps,
                           scope=scope})))))
      end                                                            

    and compileSconSwitch env (arg: lvar,
                               selections: (scon, DecisionTree) map,
                               wildcard: DecisionTree,
                               compiler: (int * CE.CEnv) -> LambdaExp S,
                               failure: LambdaExp,
                               poly
                              ): LambdaExp S =
      let
        exception Next  (* wrong type of scon tried, try the next type. *)

        fun foldIntegerMap map =
          mapList 
          (fn (SCon.INTEGER x, t) =>
              (compileDecTree env (t, compiler,failure,poly)) bindS (fn e' =>
              unitS(x,e')) 
        | _ => raise Next) 
          (FinMap.list map)

        fun foldStringMap map =
          mapList 
          (fn (SCon.STRING x, t) =>
              (compileDecTree env (t, compiler,failure,poly)) bindS (fn e' =>
              unitS(x,e')) 
            | _ => raise Next) 
          (FinMap.list map)

        fun foldRealMap map =
          mapList 
          (fn (SCon.REAL x, t) =>
              (compileDecTree env (t, compiler,failure,poly)) bindS (fn e' =>
              unitS(x,e')) 
            | _ => raise Next) 
          (FinMap.list map)

        fun foldCharMap map =
          mapList 
          (fn (SCon.CHAR x, t) =>
              (compileDecTree env (t, compiler,failure,poly)) bindS (fn e' =>
              unitS(x,e')) 
            | _ => raise Next) 
          (FinMap.list map)

        fun foldWordMap map =
          mapList 
          (fn (SCon.WORD x, t) =>
              (compileDecTree env (t, compiler,failure,poly)) bindS (fn e' =>
              unitS(x,e')) 
            | _ => raise Next) 
          (FinMap.list map)
      in

        (foldIntegerMap selections) bindS (fn selections' =>
        (compileDecTree env (wildcard,compiler,failure,poly)) bindS (fn w' =>
        unitS(SWITCH_I(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
                              selections',Some w')))))

        handle Next =>

        (foldStringMap selections) bindS (fn selections' =>
        (compileDecTree env (wildcard,compiler,failure,poly)) bindS (fn w' =>
        unitS(SWITCH_S(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
                              selections',Some w')))))

        handle Next =>

        (foldRealMap selections) bindS (fn selections' =>
        (compileDecTree env (wildcard,compiler,failure,poly)) bindS (fn w' =>
        unitS(SWITCH_R(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
                              selections',Some w')))))

        handle Next =>

        (foldCharMap selections) bindS (fn selections' =>
        (compileDecTree env (wildcard,compiler,failure,poly)) bindS (fn w' =>
        unitS(SWITCH_I(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
                              selections',Some w')))))

        handle Next =>

        (foldWordMap selections) bindS (fn selections' =>
        (compileDecTree env (wildcard,compiler,failure,poly)) bindS (fn w' =>
        unitS(SWITCH_I(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
                              selections',Some w')))))

        handle Next => die "compileSconSwitch"
      end

   (* 
    * compileDecTree: 
    *    Note that compileDecTree doesn't take a list of abstract syntax
    * expressions as argument: we may not have one - compileDec isn't used
    * in that way at top-level. Nor does it take a list of compiled lambda
    * expressions: there is not necessarily a unique lambda for each RHS as
    * an RHS might be reachable several ways through a decision tree, so
    * the environment for generating the lambda isn't easily determined
    * elsewhere. So, it seems that the best bet is to pass `compileDecTree'
    * a function of type `(int * CE.CEnv) -> LambdaExp', which it can call at
    * each leaf point to generate a lambda for that rule with the decomposition
    * environment.
    *   poly signifies whether or not the variables that are bound in the decision
    * tree are allowed to be LET bound (poly true) or must be FN bound (poly false). 
    *   Whenever an lvar to be bound is encountered it is saved in the environment
    * together with the bound type variables corresponding to the lvar. This way
    * it is possible to obtain type instances of parents by looking up the variables
    * in the enviroment. The reason this holds is as follows. There will only be 
    * bound type variables when the decision tree has been generated for a pattern 
    * which is the left hand side pattern of a value binding (otherwise the 
    * decision tree is generated from a match in an FNexp or a HANDLEexp where we 
    * only have types and not type schemes).  We need the instances for a parent when
    * we refer to a parent (for instance when decomposing).  We simply assert that
    * the instances are equal to the bound variables of the parent, i.e. that the 
    * bound type variables are instantiated to themselves, which is OK as the 
    * bound type variables of a child of a parent are a subset of the bound type 
    * variables of the parent. Example: consider
    *    val (Id,Id') = (fn x => x, fn y => y)
    * This is compiled to something like
    *    LET v192: 
    *        FORALL(a17, a16).(('a16->'a16), ('a17->'a17)) = 
    *           PRIM(RECORDprim, [FN v201: 'a16. v201:(),FN v202: 'a17. v202:()])
    *    IN  LET v193: 
    *          FORALL'a16.('a16->'a16) = 
    *            PRIM(SELECTprim(0), [v192:('a17,'a16)])
    *        IN  LET v194: 
    *              FORALL'a17.('a17->'a17) = 
    *                PRIM(SELECTprim(1), [v192:('a17,'a16)])
    *            IN  ....
    * That is, first a root variable is bound to the expression and then the
    * pattern mathcing is performed (this code is generated by compileDecTree).
    * When we refer to the parent (the root v192 in this case) to generate the
    * code PRIM(SELECTprim(0), [v192:('a17,'a16)]), we set the instances of the 
    * parent ('a17,'a16) equal to the bound variables of the type scheme for the 
    * parent. This works since the type variables and the type recorded for 
    * label 1 (corresponding to Id) in the pattern of the value binding is
    * 'a16, respectively ('a16->'a16). So this code only works if the type information
    * is not alpha converted!
    *
    * -- Lars.
    *)

    and compileDecTree env (tree,
                            compiler: (int * CE.CEnv) -> LambdaExp S,
                            failure,
                            poly 
                           ): LambdaExp S =
      case tree
        of LAB_DECOMPOSE{bind, parent, lab, child, info} =>
          let 
            fun whichLab info: int =
              case info of
                TypeInfo.LAB_INFO{index,...} =>  index 
              | _ => die "whichLab"
            fun labType info : (TyVar list * StatObject.Type) =
              case info of
                TypeInfo.LAB_INFO{tyvars,Type,...} => (tyvars,Type)
              | _ => die "labType"
          in
            (compileTypeScheme (labType info)) bindS (fn (tyvars',tau') =>
            let 
              val env' = CE.declareLvar(bind,map TYVARtype tyvars',env) 
            in                                                    
              (compileDecTree env' (child,compiler,failure,poly)) bindS (fn exp' =>
              unitS((if poly then (polyLet tyvars') else monoLet)
                    ((bind,tau',PRIM(SELECTprim (whichLab info),
                                     [VAR{lvar=parent,
                                          instances=CE.lookupLvar env parent}])),
                     exp')))
            end)
          end
         | CON_DECOMPOSE{bind, parent, child,info} =>
	  let val (sigma, longcon, instances) =
                 case info 
		   of TypeInfo.CON_INFO{tyvars,Type,longid,instances,...} => 
		    ((tyvars,Type),longid,instances)
		    | _ => die "compileDecTree(CON_DECOMPOSE...)"
	      fun convert_sigma(tyvars,tau) =
		let fun mem [] tv = false
		      | mem (tv'::tvs) tv = TyVar.eq (tv,tv') orelse mem tvs tv 
		    fun NoSome (Some x) = x
		      | NoSome None = die "compileDecTree(CON_DECOMPOSE..): wrong type info 1"
		in
		  case (NoSome o Type.un_FunType o NoSome o Type.to_FunType) tau 
		    of (tau1,_) => (List.all (mem (Type.tyvars tau1)) tyvars, tau1)
		end
	  in case lookupLongcon' env longcon
	       of CE.CON(con,it) =>
		 let val instances = CE.apply_it(it,instances)
		     val (tyvars,tau) = convert_sigma sigma
		 in
		   (mapList compileType instances) bindS (fn instances' =>
                   (mapList (compileTyVar env) tyvars) bindS (fn tyvars =>
                   (compileType tau) bindS (fn tau =>
		   let val env' = CE.declareLvar(bind,map TYVARtype tyvars,env)
		   in                                           
		      (compileDecTree env' (child,compiler,failure,poly)) bindS (fn exp' =>
                      unitS((if poly then (polyLet tyvars) else monoLet)
                      ((bind,tau,PRIM(DECONprim{con=con,
						instances=instances'},
				      [VAR{lvar=parent,
					   instances=CE.lookupLvar env parent}])),
		       exp')))
		   end)))
		 end
		| CE.REF => 
		 let val (tyvars,tau) = convert_sigma sigma
		 in
		   (mapList compileType instances) bindS (fn instances' =>
                   (mapList (compileTyVar env) tyvars) bindS (fn tyvars =>
                   (compileType tau) bindS (fn tau =>
		   let val env' = CE.declareLvar(bind,map TYVARtype tyvars,env)
		   in                                           
		      (compileDecTree env' (child,compiler,failure,poly)) bindS (fn exp' =>
                      unitS((if poly then (polyLet tyvars) else monoLet)
                      ((case instances'
			     of [instance'] =>
			       (bind,tau,PRIM(DEREFprim{instance=instance'},
					      [VAR{lvar=parent,
						   instances=CE.lookupLvar env parent}]))
			      | _ => die "compileDecTree(ConDEREF..)"),
			  exp')))
		   end)))
		 end
		| _ => die "compileDecTree.CON_DECOMPOSE.not CON or REF"
	  end

         | EXCON_DECOMPOSE{bind, parent, child,info} =>
             let
               val (Type,longexcon) =
                 case info of
                   TypeInfo.EXCON_INFO{Type,longid} => (Type,longid)
                 | _ => die "compileDecTree(CON_DECOMPOSE...)"
               val tau = compileType (domType Type)
               val env' = CE.declareLvar(bind,[],env) (* added Lars 940621 *)
             in
               (compileType (domType Type)) bindS (fn tau =>
               (compileDecTree env' (child, compiler, failure,poly)) bindS (fn exp' =>
               unitS((if poly then (polyLet []) else monoLet) 
                       (* Could simply use monoLet, but we would like it to be the 
                        * case that all variables bound in value bindings in SML
                        * are compiled to LET-bindings in the lambda language
                        * (to keep it simple to remember)
                        *)
                     ((bind,tau,PRIM(DEEXCONprim (lookupLongexcon env longexcon),
                                     [VAR{lvar=parent,instances=[]}])),
                                         (* instances = [] since the type of the 
                                          * parent must be 'exn'
                                          *)
                      exp'))))
             end

         | CON_SWITCH{arg, selections, wildcard} =>
             (mapList
              (fn (con,(ti,tree)) => 
               (compileDecTree env (tree,compiler,failure,poly)) bindS (fn e' =>
               unitS(#1(lookupLongcon env (Ident.idToLongId con)), e')))
              (FinMap.list selections))
                                         bindS (fn selections' =>
              (compileDecTreeOpt env (wildcard,compiler,failure,poly)) 
                                         bindS (fn wildcardOpt' =>
              unitS (SWITCH_C(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
                                     selections',wildcardOpt')))))

         | SCON_SWITCH{arg, selections, wildcard} =>
             compileSconSwitch env
               (arg, selections, wildcard, compiler, failure,poly)

         | EXCON_SWITCH{arg, selections, wildcard} =>
             (mapList 
              (fn (longexcon,tree) =>
               (compileDecTree env (tree,compiler,failure,poly)) bindS (fn e' =>
                unitS((lookupLongexcon env longexcon,e'))))
              selections)
                                         bindS (fn selections' =>
              (compileDecTree env (wildcard,compiler,failure,poly)) 
                                         bindS (fn wildcard' =>
              unitS(SWITCH_E(SWITCH(VAR{lvar=arg,instances=[]},
                                    (* instances = [] since the type of arg must
                                     * be 'exn'
                                     *)
                                    selections',Some wildcard')))))

         | END{ruleNum, environment} => compiler(ruleNum, environment)
         | FAIL => unitS(failure)

    and compileDecTreeOpt env (None, _, _,_) = unitS None
      | compileDecTreeOpt env (Some t, compiler, failure,poly) =
        (compileDecTree env (t,compiler,failure,poly)) bindS (fn exp' =>
        unitS(Some exp'))                                               

  val compileDec =
    (* Note: if ImperativeStateMonad is used, and resetS is not applied, 
     * the state will persist between different calls to compileDec
     *)
    fn env => fn dec => 
    #1(showS initialState
    ((resetS (unitS ())) bindS (fn () =>
    ((compileDec env (true,dec)) bindS (fn (env1,f1) =>
    (getS (unitS ())) bindS (fn s =>
    unitS(env1,fn scope => 
               let val (res,s') = showS s (f1 scope)
               in PGM(DATBINDS (#1 s'), res)
               end)))))))

  fun reset () = () 

  end;
