(*$CompileDec : CON EXCON TYNAME TOPDEC_GRAMMAR STATOBJECT
        ENVIRONMENTS LVARS LAMBDA_EXP COMPILER_ENV MATCH_COMPILER
        ELAB_INFO FINMAP FINMAPEQ CRASH FLAGS PRETTYPRINT REPORT
        COMPILE_DEC*)

(* At some point we should clean up this code.. Constructors are not
 * looked up in the environment anymore.. Also, - it would be nice if
 * primitives were declared in the initial environment (this makes the
 * prelude smaller and better code is generated when compiling
 * multible compilation units (inlining of primitives will happen
 * automatically)). (martin) *)

functor CompileDec(structure Con: CON   
                   structure Excon: EXCON
                   structure TyName: TYNAME
                   structure Lvars: LVARS

		   structure TopdecGrammar: TOPDEC_GRAMMAR

                   structure StatObject : STATOBJECT
                     sharing type StatObject.TyName = TyName.TyName

 		   structure Environments : ENVIRONMENTS
		     sharing type Environments.TypeFcn = StatObject.TypeFcn
		         and type Environments.TypeScheme = StatObject.TypeScheme
		         and type Environments.Type = StatObject.Type
			 and type Environments.TyVar = StatObject.TyVar
			 and type Environments.id = TopdecGrammar.id
			 and type Environments.strid = TopdecGrammar.strid
			 and Environments.TyName = TyName

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
			 and type CompilerEnv.tyvar = LambdaExp.tyvar
			 and type CompilerEnv.id = TopdecGrammar.id
			 and type CompilerEnv.longid = TopdecGrammar.DecGrammar.longid
			 and type CompilerEnv.TypeScheme = StatObject.TypeScheme
			 and type CompilerEnv.strid = TopdecGrammar.strid
			 and type CompilerEnv.ElabEnv = Environments.Env
			 and type CompilerEnv.TyName = TyName.TyName
			 and type CompilerEnv.tycon = Environments.tycon

                   structure MatchCompiler: MATCH_COMPILER
                     sharing type MatchCompiler.pat = TopdecGrammar.DecGrammar.pat
                         and type MatchCompiler.id = TopdecGrammar.id
                         and type MatchCompiler.lab = TopdecGrammar.DecGrammar.lab
                         and type MatchCompiler.scon = TopdecGrammar.DecGrammar.scon
                         and type MatchCompiler.longid = TopdecGrammar.DecGrammar.longid
                         and type MatchCompiler.lvar = Lvars.lvar
                         and type MatchCompiler.CEnv = CompilerEnv.CEnv
		         and type MatchCompiler.LType = LambdaExp.Type
		         and type MatchCompiler.SType = StatObject.Type
			 and type MatchCompiler.TyVar = StatObject.TyVar
			 and type MatchCompiler.tyvar = LambdaExp.tyvar

		   structure ElabInfo : ELAB_INFO
                     sharing type ElabInfo.ElabInfo = TopdecGrammar.info
                         and type MatchCompiler.TypeInfo = ElabInfo.TypeInfo.TypeInfo
                         and type ElabInfo.TypeInfo.longid = TopdecGrammar.DecGrammar.longid
                         and type ElabInfo.TypeInfo.TyEnv = Environments.TyEnv
                         and type ElabInfo.TypeInfo.TyVar = StatObject.TyVar
		         and type ElabInfo.TypeInfo.Type = StatObject.Type
		         and type ElabInfo.TypeInfo.Env = Environments.Env
			 and type ElabInfo.TypeInfo.strid = TopdecGrammar.strid
			 and type ElabInfo.TypeInfo.tycon = TopdecGrammar.tycon
			 and type ElabInfo.TypeInfo.id = TopdecGrammar.id

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
    structure Grammar = TopdecGrammar.DecGrammar
    open Grammar

    (*import from StatObject:*)
    structure TyVar        = StatObject.TyVar            (* be careful - there are *)
         type TyVar        = StatObject.TyVar            (* two kinds of TyVar's around *)
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

    fun declareExcon(id,(excon,tau),CE) = CE.declareExcon(id,(excon,tau),CE)
    fun declareCon(id,(con,tyvars,tau,it),CE) = CE.declareCon(id,(con,tyvars,tau,it),CE)

    datatype lookup_info = NORMAL of ElabInfo.ElabInfo | OTHER of string
    fun lookup_error(kind: string, CE, longid, info:lookup_info) =
            let fun say s = (output(std_out, s );
                             output(!Flags.log, s);
                             NonStandard.flush_out(!Flags.log))
                fun sayst(st) = PrettyPrint.outputTree(say, st, !Flags.colwidth)
                fun say_compilerenv() = sayst(CE.layoutCEnv CE)
                fun say_info(NORMAL i) = sayst(ElabInfo.layout i)
                  | say_info(OTHER  s) = say s
            in
                say "\n.............";
                say_info(info);
                say ("Cannot find " ^ kind ^ " " ^  Ident.pr_longid longid
                     ^" in compiler environment:\n");
                say_compilerenv();
                die "lookup_error"
            end

    fun lookupLongexcon CE longid (info:lookup_info)= 
      case CE.lookup_longid CE longid
	of Some(CE.EXCON (excon,tau)) => (excon,tau)
         | _ => lookup_error("long exception constructor",CE,longid,info)

    fun lookupLongcon CE longid (info:lookup_info)=
      case CE.lookup_longid CE longid
	of Some(CE.CON (con,_,_,_,_)) => con
	 | _ => lookup_error("long value constructor",CE,longid,info)

    fun lookupLongid CE longid (info:lookup_info) = 
         case CE.lookup_longid CE longid of
           Some res => res
         | None  => lookup_error("long value variable",CE,longid,info)

    fun lookup_longstrid ce longstrid =
      let val (strids,strid) = StrId.explode_longstrid longstrid
	  fun lookup (ce, []) = CE.lookup_strid ce strid
	    | lookup (ce, strid::strids) = lookup(CE.lookup_strid ce strid, strids)
      in lookup(ce, strids)
      end

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


    (* -----------------------------------------------------
     * Container for collecting data type bindings
     * ----------------------------------------------------- *)

	 
	 (*MEMO: clean up this text.. re plus_datbindss: the state
	  contains a list of lists of datbinds: one datbind list from
	  each datatype declaration.  When adding a datbind list to
	  this list of datbind lists, that datbind list is thinned out
	  removing all datbinds that define a tyname that is already
	  defined in the current state.  Why would the same tyname
	  occur twice?  Because of datatype replication:

	      datatype t = K of int
	      datatype s = datatype t

	  In this example, we only want a singleton datbind list list
	  (I think).  plus_datbindss is not associative.  Maybe that
	  is not a problem.  06/03/1997 14:49. tho.*)


    type datbind_list  = (TLE.tyvar list * TyName * (con * LambdaExp.Type Option) list) list

    structure DatBinds : sig val add : datbind_list -> unit
			     val extract : unit -> datbind_list list
			     val reset : unit -> unit
			 end =
      struct
	val datbindss : datbind_list list ref = ref [] 
	fun add datbinds = datbindss := (datbinds :: (! datbindss))
	fun extract() = rev(!datbindss)
	fun reset() = datbindss := []
      end

    (* ----------------------------------------------
     * Compiling type variables
     * ---------------------------------------------- *)

   structure TV : sig val reset : unit -> unit
		      val lookup : TyVar -> TLE.tyvar
		  end =
      struct
	val env : (TyVar, TLE.tyvar) FinMapEq.map ref = ref FinMapEq.empty
	val look = FinMapEq.lookup TyVar.eq
	val add = FinMapEq.add TyVar.eq
	fun lookup tv =
	  case look (!env) tv
	    of Some tv' => tv'
	     | None => let val tv' = if TyVar.equality tv then TLE.fresh_eqtyvar()
				     else TLE.fresh_tyvar ()
		       in env := add(tv,tv',!env); tv'
		       end
	fun reset() = env := FinMapEq.empty
      end


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

    (* compileTyName is necessary because char and word from
     * CompileDec onwards are treated as int. I think we should
     * eventually support all base types in the backend (the fact that
     * there is not a one-to-one correspondence between type names
     * here and for elaboration makes me uneasy - Martin *)

    fun compileTyName tyname =
          if TyName.eq (tyname, TyName.tyName_CHAR)
	  orelse TyName.eq (tyname, TyName.tyName_WORD) 
	  orelse TyName.eq (tyname, TyName.tyName_WORD8) 
	  then TyName.tyName_INT
	  else tyname

    fun compileType (typ: StatObject.Type) : LambdaExp.Type =
        case Type.to_RecType typ of
          None =>
            (case Type.to_ConsType typ of 
               None => 
                 (case Type.to_FunType typ of 
                    None => 
                      (case Type.to_TyVar typ of
                         None => die "compileType(1)"
                       | Some tyvar => TYVARtype(TV.lookup tyvar))
                  | Some funtype => 
                      let val (ty1,ty2) = NoSome "compileType(2)" 
                                        (Type.un_FunType funtype)
			  val ty1' = compileType ty1
			  val ty2' = compileType ty2
                      in ARROWtype([ty1'],[ty2'])
                      end)
             | Some constype =>
                 let val (tys,tyname) = NoSome "compileType(3)" 
		                        (Type.un_ConsType constype)
		     val tys' = map compileType tys
                 in CONStype(tys', compileTyName tyname)
                 end)
        | Some rho =>
            let val labtys = Type.RecType.to_list rho
	        val tys' = map (compileType o #2) labtys
            in RECORDtype tys'
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
                  : (TLE.tyvar list * TLE.Type)  =
          let val tvs' = map TV.lookup tyvars
	      val tau' = compileType Type
	  in (tvs', tau')
	  end

      fun compileTyVar tyvar : TLE.tyvar = TV.lookup tyvar


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

   (* normalize_sigma tyvars0 Type:
      Type is the body of a type scheme of a constructor; 
      tyvars0 are the bound variables
      of the type scheme. 
      normalize_sigma tyvars0 Type normalizes Type into
      a type where the type arguments come in the same order
      as tyvars0
    *)

   fun normalize_sigma' (tyvars0: TLE.tyvar list, Type: Type): TLE.tyvar list * TLE.tyvar list * Type * Type Option=
     let
       val (tyvars, tauOpt, tyname) =
	 case Type 
	   of ARROWtype([tau],[CONStype(taus,tyname)]) => (map unTyVarType taus, Some tau, tyname)
	    | CONStype(taus,tyname) => (map unTyVarType taus, None, tyname)
	    | _ => die "compile_and_normalize_cb.wrong type"
       val S = mk_S (ListPair.zip (tyvars, tyvars0)) handle ListPair.Zip => 
	              die "normalize_sigma.wrong number of tyvars"
       val tauOpt' = on_TypeOpt S tauOpt
       val tau = case tauOpt'
		   of Some tau => ARROWtype([tau],[CONStype(map TYVARtype tyvars0, tyname)])
		    | None => CONStype(map TYVARtype tyvars0, tyname)
     in
       (tyvars,tyvars0, tau, tauOpt')
     end 

   fun normalize_sigma (tyvars0: TLE.tyvar list, Type: Type): TLE.tyvar list * Type=
     let val (_, tyvars0,tau', _) =  normalize_sigma' (tyvars0: TLE.tyvar list, Type: Type)
     in (tyvars0,tau') (* the normalized type scheme *)
     end

   fun compile_and_normalize_cb tyname tyvars0 (con, Type) (env, cons_TypeOpts) 
     : CE.CEnv * (con * Type Option) list =
     let val con' = compileCon con
       val (tyvars,_, tau, tauOpt') = normalize_sigma'(tyvars0, Type)
       val it : CE.instance_transformer = CE.mk_it tyvars tyvars0
     in
       (declareCon(con,(con',tyvars0,tau, it),env), (con', tauOpt') :: cons_TypeOpts)
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
	val tyvars' = map TV.lookup tyvars
	val cbs : (id * Type) list = 
	  VE.CEFold (fn (con, typescheme) => fn cbs =>
		      let val (_, tau) = TypeScheme.to_TyVars_and_Type typescheme
			  val tau' = compileType tau
		      in (con, tau') :: cbs
		      end) [] VE 
	val (env, cons_TypeOpts) =
	  List.foldL (compile_and_normalize_cb tyname tyvars') (CE.emptyCEnv,[]) (rev cbs)
      in
	(env, tyvars', cons_TypeOpts)
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
                       ((*eqtype*) longid, (TypeInfo * DecisionTree))
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
	     | _ => die "int_or_real: unresolved")
      fun string_or_int_or_real info (int, real, string) =
	    (case NoSome "string_or_int_or_real" (ElabInfo.to_OverloadingInfo info) of
	       OverloadingInfo.RESOLVED_INT => int
	     | OverloadingInfo.RESOLVED_REAL => real
	     | OverloadingInfo.RESOLVED_WORD => int
	     | OverloadingInfo.RESOLVED_CHAR => int
	     | OverloadingInfo.RESOLVED_STRING => string
	     | _ => die "string_or_int_or_real: unresolved")
      fun unoverload i CE.ABS = int_or_real i (ABS_INTprim, ABS_REALprim)
	| unoverload i CE.NEG = int_or_real i (NEG_INTprim, NEG_REALprim)
	| unoverload i CE.PLUS = int_or_real i (PLUS_INTprim, PLUS_REALprim)
	| unoverload i CE.MINUS = int_or_real i (MINUS_INTprim, MINUS_REALprim)
	| unoverload i CE.MUL = int_or_real i (MUL_INTprim, MUL_REALprim)
	| unoverload i CE.DIV = CCALLprim ("divInt", {instance=compileType Type.Int})
	| unoverload i CE.MOD = CCALLprim ("modInt", {instance=compileType Type.Int})
	| unoverload i CE.LESS =
	    string_or_int_or_real i
	      (LESS_INTprim, LESS_REALprim,
	       CCALLprim ("lessString", {instance=compileType Type.Bool}))
	| unoverload i CE.GREATER=
	    string_or_int_or_real i
	      (GREATER_INTprim, GREATER_REALprim,
	       CCALLprim ("greaterString", {instance=compileType Type.Bool}))
	| unoverload i CE.LESSEQ =
	    string_or_int_or_real i
	      (LESSEQ_INTprim, LESSEQ_REALprim,
	       CCALLprim ("lesseqString", {instance=compileType Type.Bool}))
	| unoverload i CE.GREATEREQ =
	    string_or_int_or_real i
	      (GREATEREQ_INTprim, GREATEREQ_REALprim,
	       CCALLprim ("greatereqString", {instance=compileType Type.Bool}))
	| unoverload i _ = die "unoverload"
    in
      fun overloaded_prim info result (*e.g., CE.ABS*)
	    compilerAtexp compilerExp (arg: Grammar.atexp)
	    takes_one_argument exn_args =
	    if takes_one_argument then
	      let val arg' = compilerAtexp arg
	      in PRIM (unoverload info result, [arg'])
	      end
	    else
	      (case arg of 
		 RECORDatexp(_,
			     Some(EXPROW(_,_,exp1,
					 Some(EXPROW(_,_,exp2,None))))) =>
		 let val exp1' = compilerExp exp1
		     val exp2' = compilerExp exp2
		 in PRIM (unoverload info result, [exp1',exp2'] @ exn_args)
		 end
	       | _ => die "overloaded_prim")

      fun overloaded_prim_fn info result (*e.g., CE.ABS*)
	    takes_one_argument exn_args =
	    let val ty = int_or_real info (CONStype ([], TyName.tyName_INT),
					   CONStype ([], TyName.tyName_REAL))
	        val lvar1 = Lvars.newLvar ()
	    in
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
				      [VAR {lvar=lvar1, instances=[]}])]
			       @ exn_args)}
	    end
      fun overloaded_prim_fn' info result = (*e.g., CE.LESS, ... *)
	    let val ty = CONStype ([], string_or_int_or_real info (TyName.tyName_INT,
								   TyName.tyName_REAL,
								   TyName.tyName_STRING))
	        val lvar1 = Lvars.newLvar ()
	    in (*takes two arguments*)
	      FN {pat=[(lvar1, RECORDtype [ty, ty])],
		  body=PRIM (unoverload info result,
			     [PRIM (SELECTprim 0,
				    [VAR {lvar=lvar1, instances=[]}]),
			      PRIM (SELECTprim 1,
				    [VAR {lvar=lvar1, instances=[]}])])}
	    end
    end (*local*)

   (* ---------------------------------------------------------------------- *)
   (*           Decomposition of applications of prim                        *)
   (* ---------------------------------------------------------------------- *)

   (* Decompose the absyn argument in a `prim(n, xxx)' expression. Notice
      that we're extremely intolerant of any deviation from this precise
      syntax. *)

    fun decomposePrimArg atexp: int * exp list =
      let val (primno,arg) =
	    (case atexp of 
	       RECORDatexp(_,
			   Some(EXPROW(_, _, ATEXPexp(_, SCONatexp(_, SCon.INTEGER i)),
				       Some(EXPROW(_, _, exp2, None))))) => (i, exp2)
	     | _ => die "prim syntax error")

	fun decomposeArgs None = []
	  | decomposeArgs (Some (EXPROW(_,_,exp1, arg'))) =
	      exp1 :: decomposeArgs arg'

	fun decomposeExp (ATEXPexp (_, RECORDatexp (_,expRow))) =
	      decomposeArgs expRow
	  | decomposeExp exp = [exp]
      in
        (primno,
         if primno < 0 then die ("prim number must be non-negative.  It is "
				 ^ Int.string primno)
         else decomposeExp arg)
      end

    (* ----------------------------------------------------------------------- *)
    (*               Syntax directed compilation                               *)
    (* ----------------------------------------------------------------------- *)

    fun compileAtexp env atexp : TLE.LambdaExp =
      case atexp
        of SCONatexp(_, SCon.INTEGER x) => INTEGER x
         | SCONatexp(_, SCon.STRING x) => STRING x
         | SCONatexp(_, SCon.REAL x) => REAL x
         | SCONatexp(_, SCon.CHAR x) => INTEGER x
         | SCONatexp(_, SCon.WORD x) => INTEGER x

         | IDENTatexp(info, OP_OPT(longid, _)) =>
	  (case lookupLongid env longid (NORMAL info)
	     of CE.LVAR (lv,tyvars,_,il) =>   (*see COMPILER_ENV*) 
	      (let val instances =
		     case ElabInfo.to_TypeInfo info 
		       of Some(TypeInfo.VAR_INFO{instances}) => instances
			| _ => die ("compileAtexp(LVAR..): no type info for "
				    ^ Ident.pr_longid longid)
		   val instances' = map compileType instances
		   val S = CE.mk_subst(tyvars, instances')
		   val il' = CE.on_il(S,il)
	       in VAR {lvar=lv,instances=il'}
	       end handle X => (print (" Exception raised in CompileDec.IDENTatexp.LVAR.longid = " ^ Ident.pr_longid longid ^ "\n");
				print " Reraising...\n"; raise X))
	      | CE.ABS =>       overloaded_prim_fn info CE.ABS       true  []
	      | CE.NEG =>       overloaded_prim_fn info CE.NEG       true  []
	      | CE.PLUS =>      overloaded_prim_fn info CE.PLUS      false []
	      | CE.MINUS =>     overloaded_prim_fn info CE.MINUS     false []
	      | CE.MUL =>       overloaded_prim_fn info CE.MUL       false []
	      | CE.DIV =>       overloaded_prim_fn info CE.DIV       false
	           [PRIM (EXCONprim Excon.ex_DIV, [])]
	      | CE.MOD =>       overloaded_prim_fn info CE.MOD       false
		   [PRIM (EXCONprim Excon.ex_MOD, [])]
	      | CE.LESS =>      overloaded_prim_fn' info CE.LESS
	      | CE.GREATER =>   overloaded_prim_fn' info CE.GREATER
	      | CE.LESSEQ =>    overloaded_prim_fn' info CE.LESSEQ
	      | CE.GREATEREQ => overloaded_prim_fn' info CE.GREATEREQ
	      | CE.CON(con,tyvars,_,il,it) => (*See COMPILER_ENV*)
	       let
		 val (functional,Type,instances) =
		   case ElabInfo.to_TypeInfo info 
		     of Some (TypeInfo.CON_INFO{Type,instances,...}) =>
		       (Type.is_Arrow Type, Type, instances)
		      | _ => die "compileAtexp(CON..): no type info"
	       in if functional then
		    let val lv = Lvars.newLvar()
		        val tau' = compileType (domType Type)
			val instances' = map compileType instances
			val S = CE.mk_subst(tyvars, instances')
			val il' = CE.on_il(S,il)
			val il'' = CE.apply_it(it,il')
		    in FN{pat=[(lv,tau')],
			  body=PRIM(CONprim{con=con, instances=il''},
				    [VAR{lvar=lv,instances=[]}])}
		    end
		  else
		    let val instances' = map compileType instances
		        val S = CE.mk_subst(tyvars, instances')
			val il' = CE.on_il(S,il)
			val il'' = CE.apply_it(it,il')
		    in PRIM(CONprim {con=con, instances=il''},[])
		    end
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
		   val tau' = compileType (domType Type)
		   val instance' = compileType instance
	       in FN{pat=[(lv,tau')],
		     body=PRIM(REFprim {instance=instance'},
			       [VAR{lvar=lv,instances=[]}])}
	       end
	      | CE.EXCON (excon,_) =>
	       let
		 val (functional,Type) =
		   case ElabInfo.to_TypeInfo info of 
		     Some (TypeInfo.EXCON_INFO{Type,...}) => 
		       (Type.is_Arrow Type,Type)
		   | _ => die "compileAtexp(EXCON..): no type info"
	       in         
		 if functional then
		   let val lv = Lvars.newLvar()
		       val tau' = compileType (domType Type)
		   in FN{pat=[(lv,tau')],
			 body=PRIM(EXCONprim excon,
				   [VAR{lvar=lv, instances=[]}])}
		   end
		 else PRIM(EXCONprim excon,[])
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
	       val exps = map (fn EXPROW(_, _, e, _) => compileExp env e) rows
	       fun is_sorted (l1::(labs as l2::_)) = Lab.<(l1,l2) andalso is_sorted labs
		 | is_sorted _ = true
	     in if is_sorted labs then PRIM(RECORDprim, exps) 
		else
		  let val taus = map (compileType o type_of_exp 
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
		    List.foldR (fn (lv,exp,tau) => fn exp' => monoLet((lv,tau,exp),exp'))
		    scope (zip3(lvars,exps,taus))
		  end
	     end

         | RECORDatexp(_, None) => PRIM(RECORDprim,[])

         | LETatexp(_, dec, exp) =>
	     let val (env1, f) = compileDec env (false,dec)
	         val exp' = compileExp (env plus env1) exp
	     in f exp'
	     end

         | PARatexp(_, exp) => compileExp env exp

    and compileExp env exp =
      (case exp of
	 ATEXPexp(_, atexp) => compileAtexp env atexp

       | APPexp(_,
		f as ATEXPexp(_, IDENTatexp(info, OP_OPT(longid, _))),
		arg) =>
                        (* We have to spot direct application of "prim" - apart
                           from that, we don't have to bother with constructors
                           and the like. They'll compile to functions, but the
                           optimiser will spot them later. *)
	  
	   (case lookupLongid env longid (NORMAL info) of
	      CE.LVAR (lv,tyvars,_,il) =>        (* Not a primitive... *)
		let val arg' = compileAtexp env arg
		    val instances' = case ElabInfo.to_TypeInfo info 
				       of Some(TypeInfo.VAR_INFO{instances}) =>
					 map compileType instances
					| _ => die "compileExp(APPexp..): wrong type info"
		    val S = CE.mk_subst(tyvars,instances')
		    val il' = CE.on_il(S, il)
		in APP(VAR{lvar=lv,instances=il'},arg')
		end

	    | CE.RESET_REGIONS =>
		let val arg' = compileAtexp env arg
		    val tau' = case ElabInfo.to_TypeInfo info 
				 of Some(TypeInfo.VAR_INFO{instances = [tau]}) =>
				   compileType tau
				  | _ => die "compileExp(APPexp..): wrong type info"
		in PRIM(RESET_REGIONSprim{instance = tau'}, [arg'])
		end

	    | CE.FORCE_RESET_REGIONS =>
		let val arg' = compileAtexp env arg
		    val tau' = case ElabInfo.to_TypeInfo info 
				 of Some(TypeInfo.VAR_INFO{instances = [tau]}) =>
				   compileType tau
				  | _ => die "compileExp(APPexp..): wrong type info"
		in PRIM(FORCE_RESET_REGIONSprim{instance = tau'}, [arg'])
		end

	    | CE.ABS =>       overloaded_prim info CE.ABS       (compileAtexp env) (compileExp env) arg true  []
	    | CE.NEG =>       overloaded_prim info CE.NEG       (compileAtexp env) (compileExp env) arg true  []
	    | CE.PLUS =>      overloaded_prim info CE.PLUS      (compileAtexp env) (compileExp env) arg false []
	    | CE.MINUS =>     overloaded_prim info CE.MINUS     (compileAtexp env) (compileExp env) arg false []
	    | CE.MUL =>       overloaded_prim info CE.MUL       (compileAtexp env) (compileExp env) arg false []
	    | CE.DIV =>       overloaded_prim info CE.DIV       (compileAtexp env) (compileExp env) arg false
		[PRIM (EXCONprim Excon.ex_DIV, [])]
	    | CE.MOD =>       overloaded_prim info CE.MOD       (compileAtexp env) (compileExp env) arg false 
		[PRIM (EXCONprim Excon.ex_MOD, [])]
	    | CE.LESS =>      overloaded_prim info CE.LESS      (compileAtexp env) (compileExp env) arg false []
	    | CE.GREATER =>   overloaded_prim info CE.GREATER   (compileAtexp env) (compileExp env) arg false []
	    | CE.LESSEQ =>    overloaded_prim info CE.LESSEQ    (compileAtexp env) (compileExp env) arg false []
	    | CE.GREATEREQ => overloaded_prim info CE.GREATEREQ (compileAtexp env) (compileExp env) arg false []
	    | CE.PRIM =>   
	       (* Application of `prim'. We must now disassemble the 
		* argument to get the prim number and the arguments 
	        * to the primitive operation *)
	       let
		 val (n, args) = decomposePrimArg arg
		 val prim = lookupPrim n
		 fun f prim =
		   let val args' = map (compileExp env) args
		       val instance' =
			 case ElabInfo.to_TypeInfo info 
			   of Some(TypeInfo.VAR_INFO{instances=[instanceRes,instance]}) => 
			     (* This code depends on the order of the
			      * instances recorded during elaboration.
			      * We need the instance corresponding to 
			      * the argument of prim, which in 
			      * EfficientCoreElab version is the second *)
			     compileType instance
			    | _ => die "compileExp(APPexp(PRIM..): wrong type info"
		   in TLE.PRIM(prim {instance=instance'}, args')
		   end
	       in
		 case prim of
		    DEREFprim _ => f DEREFprim (*ref (REFprim) is a constructor, so it does not show up here*)
		  | ASSIGNprim _ => f ASSIGNprim
		  | EQUALprim _ => f EQUALprim
		      (* <> (NOTEQUALprim) is declared in the prelude as an
		       * ordinary variable (not a primitive), so it does
		       * not show up here *)
		  | ORDprim => (case args of
				  [exp] => compileExp env exp
				| _ => die "compileExp(APPexp(PRIM... ORDprim...))")
		  | CCALLprim _ => 
		      let
			fun extractString exp =
			  let
			    val atexp = (case exp of
					   ATEXPexp(_,atexp) => atexp
					 | _ => die "CCALL.exp not atexp")
			    val scon = (case atexp of
					  SCONatexp(_,scon) => scon
					| IDENTatexp(_,_) => die "CCALL atexp is an identifier"
					| RECORDatexp(_,_) => die "CCALL atexp is a record"
					| LETatexp(_,_,_) => die "CCALL atexp is a let"
					| PARatexp(_,_) => die "CCALL atexp is a par")
			  in 
			    (case scon of
			       SCon.STRING s => s
			     | _ => die "CCALL.scon not string")
			  end 
			
			val (s, args) = 
			  (case args of
			     [] => die "No function name in CCALLprim"
			   | [s] => die "Only one function name in CCALLprim. \
                                         \Remember also function name for profiling."
			   | s::s_prof::xs =>
			       (extractString
				  (if !region_profiling then s_prof else s), xs))
			val args' = map (compileExp env) args
			val instance' =
			  case ElabInfo.to_TypeInfo info 
			    of Some(TypeInfo.VAR_INFO{instances=[instanceRes, instanceArg]}) => 
			      (* We use the result type of prim
			       * SpreadExp generates reg. vars.
			       * from the result type. *)
			      compileType instanceRes
			     | _ => die "compileExp(APPexp(PRIM..): wrong type info"
		      in TLE.PRIM(CCALLprim (s, {instance=instance'}), args')
		      end
		  | _ => let val args' = map (compileExp env) args
			 in TLE.PRIM(prim, args')
			 end
	       end
	     
	    | _ (*CON/EXCON*) => let val f' = compileExp env f
	                             val arg' = compileAtexp env arg
				 in APP(f',arg')
				 end
	  ) (*end lookup_longid*)

       | APPexp(_, f, arg) =>         (* non-trivial function expression... *)
	      let val f' = compileExp env f
		  val arg' = compileAtexp env arg
	      in APP(f',arg')
	      end

       | TYPEDexp(_, exp, _) => compileExp env exp

       | HANDLEexp(_, exp', match) =>
	      let val e1' = compileExp env exp'
		  val tau' = compileType (type_of_exp exp)
		  val e2' = compileMatch env (match, false,RAISESELF tau')
	      in HANDLE(e1',e2')
	      end

       | RAISEexp(i, exp') => 
	      let val e' = compileExp env exp'
		  val tau' = compileType (type_of_exp exp)
	      in RAISE(e',Types [tau'])
	      end

       | FNexp(_, match) => 
	      let val tau' = 
		    case compileType (type_of_exp exp)
		      of TLE.ARROWtype(_,[tau']) => tau'
		       | _ => die "compileExp: FNexp did not have (unary) arrow type"
	      in compileMatch env (match, true, RAISEMATCH tau')
	      end

       | UNRES_INFIXexp _ =>  die "compileExp(UNRES_INFIX)")

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

         val root = case pats
		      of [pat] =>
			(case Grammar.find_topmost_id_in_pat pat 
			   of Some string => Lvars.new_named_lvar string
			    | _ => Lvars.newLvar())
		       | _ => Lvars.newLvar()

         val decTree = matchCompiler compileTypeScheme 
	   (root, pats, {warnInexhaustive=warn, warnNoBindings=false})

         fun f(n: int, e: CE.CEnv): LambdaExp =
           compileExp (env plus e) (List.nth (n-1) exps)

         val env' = CE.declareLvar(root,[],env)
         val exp =
            compileDecTree env' (decTree, f,
                                case failure
                                  of RAISEMATCH tau' => raiseMatch tau'
                                   | RAISESELF tau' => RAISE(VAR{lvar=root,instances=[]},
                                                             Types [tau']),
                                false) 
                (* last argument is false as the variables bound in the match
		 * are to be FN bound in the lambda language (monomorphic) *)
	 val tau = compileType (domType (type_of_match match))
      in FN{pat=[(root,tau)], body=exp}
      end

   (* compileDec - takes an enclosing environment and a declaration, and
      returns the environment *for this declaration only*, together with a
      function to apply to the declaration's scope to return the entire
      lambda term. The `topLevel' parameter is only needed because the
      match compiler is expected to report non-binding patterns for
      non top-level val bindings only. *)

    and compileDec env (topLevel, dec): (CE.CEnv * (LambdaExp -> LambdaExp)) =
      case dec
        of VALdec(_, tyvars, valbind) =>
             compileValbind env (topLevel, valbind)

         | UNRES_FUNdec _ =>
             die "compileDec(UNRES_FUN)"

         | TYPEdec(i, typbind) => (compileTypbind i, fn x => x)

         | DATATYPEdec(i, _) => 
	     let val (env_ve, env_te, datbinds) = compileDatbind i
	         val _ = DatBinds.add datbinds
	     in (env_ve plus env_te, fn x => x)
	     end

	 | DATATYPE_REPLICATIONdec (i, tycon, longtycon) => 
	     let val env1 = compileDatrepl i
	     in (env1, fn x => x)
	     end

         | ABSTYPEdec(i, _, dec) =>
	     let val (env_ve, env_te, datbinds) = compileDatbind i
	         val _ = DatBinds.add datbinds
		 val (env2, f) = compileDec (env plus (env_ve plus env_te)) (false,dec)
	     in (env_te plus env2, f)
	     end

         | EXCEPTIONdec(_, exbind) => compileExbind env exbind

         | LOCALdec(_, dec1, dec2) =>
             let val (env1, f1) = compileDec env (false,dec1)
	         val (env2, f2) = compileDec (env plus env1) (false,dec2)
	     in (env2, f1 o f2)
	     end

         | OPENdec(i,longstrids_withinfos) =>
	     let val longstrids = map (fn WITH_INFO(_,longstrid) => longstrid) longstrids_withinfos
	         val envs = map (lookup_longstrid env) longstrids
		 val env' = List.foldL (fn env => fn env' => env plus env') CE.emptyCEnv envs
	     in (env', fn x => x)
	     end
 
         | SEQdec(_, dec1, dec2) =>
             let val (env1, f1) = compileDec env (topLevel,dec1)
	         val (env2, f2) = compileDec (env plus env1) (topLevel,dec2)
	     in (env1 plus env2, f1 o f2)
	     end

        (* INFIX/NONFIX declarations have no effect on execution. *)

         | INFIXdec _ => (CE.emptyCEnv, fn x => x)
         | INFIXRdec _ => (CE.emptyCEnv, fn x => x)
         | NONFIXdec _ => (CE.emptyCEnv, fn x => x)

         | EMPTYdec _ => (CE.emptyCEnv, fn x => x)

   (* compileValbind - although there may be `rec' prefixes nested anywhere
      in the valbind, the effect is a single non-recursive layer of
      binding, together with a second layer of several distinct recursive
      valbinds. *)

    and compileValbind env (topLevel, valbind)
        : (CE.CEnv * (LambdaExp -> LambdaExp)) =
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
            (case ElabInfo.to_TypeInfo i
	       of Some (TypeInfo.PLAINvalbind_INFO{tyvars,Type,...}) => 
		 compileBinding env (topLevel, pat, exp,(tyvars,Type))
		| _ => die "compileValbind: no type info")

           | PLAINvalbind(i, pat, exp, Some vb) =>
               (case ElabInfo.to_TypeInfo i
		  of Some (TypeInfo.PLAINvalbind_INFO{tyvars,Type,...}) =>
		    let val (env1, f1) = compileBinding env (topLevel, pat, exp,(tyvars,Type))
		        val (envRest, f2) = compileValbind env (topLevel,vb)
		    in (env1 plus envRest, f1 o f2)
		    end
		   | _ => die "compileValbind: no type info")

           | RECvalbind(_, vb) =>
               let val triples = flattenRecValbind vb
               in compileREC env (unzip3 triples)
               end
      end


       (* A datatype declaration is compiled by compiling the semantic
        * object TyEnv associated with it. The type environment
        * (TyEnv) has been glued (as type info) to the syntactic
        * construct of the datatype declaration during elaboration.
        * Notice that the syntactic restrictions (see p. 9, The Def.)
        * ensures that no constructor identifier is bound twice by the
        * same datbind. *)
                          (* VE        TE *) 
    and compileDatbind i : CE.CEnv * CE.CEnv * datbind_list =
      case ElabInfo.to_TypeInfo i 
	of Some(TypeInfo.TYENV_INFO TyEnv) => 
	  TE.Fold (fn (tycon,tystr) => fn (env_ve, env_te, dats) => 
	       if VE.is_empty (TyStr.to_VE tystr) then die "compileDatbind"
	       else let val tyname = (NoSome "TypeFcn not simple" o 
				      TypeFcn.to_TyName o TyStr.to_theta) tystr
			val VE = TyStr.to_VE tystr
			val (env_ve', tyvars, cbs) = compile'TyStr' (compileTyName tyname, VE) 
			val env_te' = CE.declare_tycon(tycon, [tyname], env_te)
		    in (env_ve plus env_ve', env_te', (tyvars,tyname,cbs)::dats)
		    end) (CE.emptyCEnv,CE.emptyCEnv,[]) TyEnv
	 | _ => die "No TyEnv type info for compiling datbind"

    and compileDatrepl i : CE.CEnv =
      case ElabInfo.to_TypeInfo i 
	of Some(TypeInfo.TYENV_INFO TyEnv) =>
	  (* A datatype replication may or may not introduce an empty VE component. *)
	  TE.Fold (fn (tycon, tystr) => fn env' => 
		   if VE.is_empty (TyStr.to_VE tystr) then
		     let val tns = TyName.Set.list(TyStr.tynames tystr)
		     in CE.declare_tycon(tycon,tns,env')
		     end
		   else let val tyname = (NoSome "TypeFcn not simple" o 
					  TypeFcn.to_TyName o TyStr.to_theta) tystr
			    val VE = TyStr.to_VE tystr
			    val (env'', tyvars, cbs) = compile'TyStr' (compileTyName tyname, VE) 
			in CE.declare_tycon(tycon,[tyname],env' plus env'')
			end) CE.emptyCEnv TyEnv
	 | _ => die "No TyEnv type info for compiling datatype replication/type declaration"

    and compileTypbind i : CE.CEnv = compileDatrepl i

    and compileExbind (env:CE.CEnv) exbind : (CE.CEnv * (LambdaExp -> LambdaExp)) =
      case exbind
        of EXBIND(i, OP_OPT(excon, _), _, rest) =>
             let val tyOpt = case ElabInfo.to_TypeInfo i 
			       of Some(TypeInfo.EXBIND_INFO {TypeOpt}) => TypeOpt 
				| _ => die "No typeOpt info for compiling exbind" 
		 val (env1, f1) = compileNewExn env excon tyOpt
		 val (envRest, f2) = case rest
				       of Some exbind' => compileExbind env exbind'
					| None => (CE.emptyCEnv, fn x => x)
	     in (env1 plus envRest, f1 o f2)
             end

         | EXEQUAL(info, OP_OPT(excon1, _), OP_OPT(longexcon2, _), rest) =>
             let val (excon2', tau) = lookupLongexcon env longexcon2 (NORMAL info)
                 val env1 = declareExcon(excon1, (excon2', tau), CE.emptyCEnv)
		 val (envRest, f2) = case rest
				       of Some exbind' => compileExbind env exbind'
					| None => (CE.emptyCEnv, fn x => x)
	     in (env1 plus envRest, f2) (*no new code*)
             end

    and compileNewExn env excon tyOpt =
      let val excon' = compileExCon excon
          val LambdaTypeOpt =
	    case tyOpt 
	      of None => None 
	       | Some tau => Some (compileType tau)
	  val tau = case LambdaTypeOpt
		      of Some tau => TLE.ARROWtype([tau],[TLE.exnType])
		       | None => TLE.exnType 
	  val env1 = declareExcon(excon,(excon',tau),CE.emptyCEnv)
	  val f1 = fn x => EXCEPTION(excon',LambdaTypeOpt, x)
      in (env1,f1)
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
        : (CE.CEnv * (LambdaExp -> LambdaExp)) =
      let
        val root = (* Root of the pattern. *)
                   case Grammar.find_topmost_id_in_pat pat of
                     None => Lvars.newLvar()  
                   | Some string => Lvars.new_named_lvar string

        val decTree = matchCompiler compileTypeScheme
	  (root, [pat], {warnInexhaustive=false, warnNoBindings=not topLevel})
	(* Decision tree which takes the root apart according to the pattern. *)

        val env1 = envOfDecTree decTree    (* The identifier environment generated 
					    * by this (single) pattern. *)
	val f = fn scope =>
	  let val exp' = compileExp env exp
	      val tyvars' = map TV.lookup tyvars
	      val tau' = compileType Type
	      val exp'' = compileDecTree (CE.declareLvar(root,map TYVARtype tyvars',env))
		          (decTree, fn _ => scope, raiseBind, true) 
	  in LET{pat=[(root,tyvars',tau')],
		 bind=exp',
		 scope=exp''}
	  end
      in (env1, f)

        (* Given the final scope of this declaration, we can compile
         * the declaration tree such that `scope' appears in the scope
         * of the pattern. Weird side- effect that the compilation is
         * passed out as a suspension. Note that the last argument to
         * compileDecTree is true, since variables bound by the
         * pattern pat should be LET bound in the lambda language to
         * allow polymorphism. *)

      end

   (* compileREC - compile a list of `rec' pattern/expression pairs. The
      patterns must all be variables (and aren't even allowed to be
      bracketted??), and the RHS's must all be lambdas. Type constraints
      are allowed, though. Returns the rec env only, plus `fn scope -> lexp'. *)

    and compileREC env (pats, exps, sigmas): (CE.CEnv * (LambdaExp -> LambdaExp)) =
      let
        fun varOfPat(TYPEDpat(_, pat, _)) = varOfPat pat
          | varOfPat(ATPATpat(_, LONGIDatpat(_, OP_OPT(longid, _)))) =
              (case Ident.decompose longid
                 of (nil, id) => id
                  | _ => die("compileREC.varOfPat(long: "
			     ^ Ident.pr_longid longid ^ ")"))
          | varOfPat _ = die "compileREC.varOfPat"

        val ids = map varOfPat pats
        val lvars_with_dummies = 
	  map (fn id => (Lvars.new_named_lvar(Ident.pr_id id),[],TYVARtype (fresh_tyvar()))) ids
	val lvars = map #1 lvars_with_dummies
        val recEnv: CE.CEnv =  mk_env CE.declareVar (ids,lvars_with_dummies)

	fun mk_scope_env lvars sigmas' = 
	  mk_env (fn (id,(lvar,(tyvars,Type)),ce) => CE.declareVar(id,(lvar,tyvars,Type),ce))
	  (ids, ListPair.zip(lvars,sigmas'))

        fun mk_bindings lvars sigmas' binds =
          map (fn (lvar,(tyvars,Type),bind) => {lvar = lvar, tyvars=tyvars,
						Type=Type, bind=bind})
          (zip3(lvars,sigmas',binds))

	val lexps = map (compileExp (env plus recEnv)) exps
	val sigmas' = map compileTypeScheme sigmas
	val env' = mk_scope_env lvars sigmas'
	val f' = fn scope => FIX {functions=mk_bindings lvars sigmas' lexps,
				  scope=scope}
      in (env', f')
      end                                                            

    and compileSconSwitch env (arg: lvar,
                               selections: (scon, DecisionTree) map,
                               wildcard: DecisionTree,
                               compiler: (int * CE.CEnv) -> LambdaExp,
                               failure: LambdaExp,
                               poly
                              ): LambdaExp =
      let
        exception Next  (* wrong type of scon tried, try the next type. *)

        fun foldIntegerMap m =
          map (fn (SCon.INTEGER x, t) => (x, compileDecTree env (t, compiler,failure,poly))
                | _ => raise Next) 
          (FinMap.list m)

        fun foldStringMap m =
          map (fn (SCon.STRING x, t) => (x, compileDecTree env (t, compiler,failure,poly))
	        | _ => raise Next) 
          (FinMap.list m)

        fun foldCharMap m =
          map (fn (SCon.CHAR x, t) => (x, compileDecTree env (t, compiler,failure,poly))
	        | _ => raise Next) 
          (FinMap.list m)

        fun foldWordMap m =
          map (fn (SCon.WORD x, t) => (x, compileDecTree env (t, compiler,failure,poly))
	        | _ => raise Next) 
          (FinMap.list m)
      in

        let val selections' = foldIntegerMap selections
	    val w' = compileDecTree env (wildcard,compiler,failure,poly)
	in SWITCH_I(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
			   selections',Some w'))
	end

        handle Next =>

	let val selections' = foldStringMap selections
	    val w' = compileDecTree env (wildcard,compiler,failure,poly)
	in SWITCH_S(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
			   selections',Some w'))
	end

        handle Next =>

        let val selections' = foldCharMap selections
	    val w' = compileDecTree env (wildcard,compiler,failure,poly)
	in SWITCH_I(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
			   selections',Some w'))
	end

        handle Next =>

	let val selections' = foldWordMap selections
	    val w' = compileDecTree env (wildcard,compiler,failure,poly)
	in SWITCH_I(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
			   selections',Some w'))
	end

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
    * pattern matching is performed (this code is generated by compileDecTree).
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
                            compiler: (int * CE.CEnv) -> LambdaExp,
                            failure,
                            poly 
			    ): LambdaExp =
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
	    val (tyvars',tau') = compileTypeScheme (labType info)
	    val env' = CE.declareLvar(bind,map TYVARtype tyvars',env) 
	    val exp' = compileDecTree env' (child,compiler,failure,poly)
	  in (if poly then (polyLet tyvars') else monoLet)
	     ((bind,tau',PRIM(SELECTprim (whichLab info),
			      [VAR{lvar=parent,
				   instances=CE.lookupLvar env parent}])),
	      exp')
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
	  in case lookupLongid env longcon (OTHER "CON_DECOMPOSE")
	       of CE.CON(con,tvs,_,il,it) =>
		 let val (tyvars,tau) = convert_sigma sigma
		     val instances' = map compileType instances
		     val tyvars = map compileTyVar tyvars
		     val tau = compileType tau
		     val env' = CE.declareLvar(bind,map TYVARtype tyvars,env)
		     val exp' =  compileDecTree env' (child,compiler,failure,poly)
		     val S = CE.mk_subst(tvs,instances')
		     val il' = CE.on_il(S, il)
		     val il'' = CE.apply_it(it,il')
		 in (if poly then (polyLet tyvars) else monoLet)
		    ((bind,tau,PRIM(DECONprim{con=con, instances=il''},
				    [VAR{lvar=parent,
					 instances=CE.lookupLvar env parent}])),
		     exp')
		 end
		| CE.REF => 
		 (case instances
		    of [instance] =>                            
		      let val (tyvars,tau) = convert_sigma sigma    (* For deref the instance is the *)
			  val instance' = compileType instance      (* instantiated argument type. *)
			  val tyvars = map compileTyVar tyvars
			  val tau = compileType tau
			  val env' = CE.declareLvar(bind,map TYVARtype tyvars,env)
			  val exp' = compileDecTree env' (child,compiler,failure,poly)
		      in (if poly then polyLet tyvars else monoLet)
			((bind,tau,PRIM(DEREFprim{instance=CONStype([instance'],TyName.tyName_REF)},
					[VAR{lvar=parent,
					     instances=CE.lookupLvar env parent}])),
			 exp')
		      end
		     | _ => die "compileDecTree.CON_DECOMPOSE.wrong number of instances")
		| _ => die "compileDecTree.CON_DECOMPOSE.not CON or REF"
	  end

         | EXCON_DECOMPOSE{bind, parent, child,info} =>
             let
               val (Type,longexcon) =
                 case info 
		   of TypeInfo.EXCON_INFO{Type,longid} => (Type,longid)
		    | _ => die "compileDecTree(CON_DECOMPOSE...)"
               val env' = CE.declareLvar(bind,[],env)
               val tau = compileType (domType Type)
	       val exp' = compileDecTree env' (child, compiler, failure,poly)
	     in (if poly then (polyLet []) else monoLet) 
                       (* Could simply use monoLet, but we would like it to be the 
                        * case that all variables bound in value bindings in SML
                        * are compiled to LET-bindings in the lambda language
                        * (to keep it simple to remember)
                        *)
	       ((bind,tau,PRIM(DEEXCONprim (#1(lookupLongexcon env longexcon (OTHER "compileDecTree.EXCON_DECOMPOSE"))),
			       [VAR{lvar=parent,instances=[]}])),
 	        	                 (* instances = [] since the type of the 
                                          * parent must be 'exn' *)
		exp')
             end

         | CON_SWITCH{arg, selections, wildcard} =>
             (case (FinMap.list selections, wildcard)
		of ([(longcon,(ti,tree))], None) => compileDecTree env (tree,compiler,failure,poly)

	         | (selections, _) =>
		  let val selections' = map (fn (longcon,(ti,tree)) => 
					     let val e' = compileDecTree env (tree,compiler,failure,poly)
					     in (lookupLongcon env longcon (OTHER "compileDecTree"), e')
					     end) selections
		      val wildcardOpt' = compileDecTreeOpt env (wildcard,compiler,failure,poly)
		  in SWITCH_C(SWITCH(VAR{lvar=arg,instances=CE.lookupLvar env arg},
				     selections',wildcardOpt'))
		  end)

         | SCON_SWITCH{arg, selections, wildcard} =>
             compileSconSwitch env (arg, selections, wildcard, compiler, failure,poly)

         | EXCON_SWITCH{arg, selections, wildcard} =>
             let val selections' = 
	           map (fn (longexcon,tree) =>
			let val e' = compileDecTree env (tree,compiler,failure,poly)
			in (#1(lookupLongexcon env longexcon (OTHER "CompileDecTree.EXCON_SWITCH")),e')
			end) selections
		 val wildcard' = compileDecTree env (wildcard,compiler,failure,poly)
	     in SWITCH_E(SWITCH(VAR{lvar=arg,instances=[]},
                                    (* instances = [] since the type of arg must
                                     * be 'exn'
                                     *)
				selections',Some wildcard'))
	     end

         | END{ruleNum, environment} => compiler(ruleNum, environment)
         | FAIL => failure

			      and compileDecTreeOpt env (None, _, _,_) = None
      | compileDecTreeOpt env (Some t, compiler, failure,poly) =
      Some(compileDecTree env (t,compiler,failure,poly))


  (* -----------------------------------------------------
   * Modules compilation
   * ----------------------------------------------------- *)

  local 
    open TopdecGrammar         
	   
    fun comp_strexp(ce,strexp) =
      case strexp
	of STRUCTstrexp(info,strdec) => comp_strdec(ce,strdec)
	 | LONGSTRIDstrexp(info,longstrid) => (lookup_longstrid ce longstrid, fn x => x)
	 | TRANSPARENT_CONSTRAINTstrexp(info, strexp, _) =>
	  let val (ce1,f) = comp_strexp(ce,strexp)
	      val E = case ElabInfo.to_TypeInfo info
			of Some (ElabInfo.TypeInfo.TRANS_CONSTRAINT_INFO E) => E
			 | _ => die "comp_strexp.TRANSPARENT_CONSTRAINTstrexp.no env info"
	      val ce2 = CE.constrain(ce1,E)
	  in (ce2, f)
	  end
	 | OPAQUE_CONSTRAINTstrexp(info, strexp, _) => die "OPAQUE_CONSTRAINTstrexp.not impl"
	 | APPstrexp _ => die "APPstrexp.not supported by compiler"
	 | LETstrexp(info, strdec, strexp) =>
	  let val (ce1, f1) = comp_strdec(ce,strdec)
	      val (ce2, f2) = comp_strexp(CE.plus(ce,ce1), strexp)
	  in (ce2, f1 o f2)
	  end

    and comp_strdec(ce: CE.CEnv, strdec: strdec) =
      case strdec
	of DECstrdec(info, dec) => compileDec ce (false,dec)  (* We  always want the warnings, since this is
							       * a compiler - not a top-level loop.. - Martin *)
	 | STRUCTUREstrdec(info, strbind) => comp_strbind(ce,strbind)
	 | LOCALstrdec(info, strdec1, strdec2) =>
	  let val (ce1, f1) = comp_strdec(ce,strdec1)
	      val (ce2, f2) = comp_strdec(CE.plus(ce,ce1), strdec2)
	  in (ce2, f1 o f2)
	  end
	 | EMPTYstrdec info => (CE.emptyCEnv, fn x => x)
	 | SEQstrdec(info, strdec1, strdec2) =>
	  let val (ce1, f1) = comp_strdec(ce,strdec1)
	      val (ce2, f2) = comp_strdec(CE.plus(ce,ce1), strdec2)
	  in (CE.plus(ce1,ce2), f1 o f2)
	  end

    and comp_strbind(ce,STRBIND(info,strid,strexp,strbind_opt)) =
      let val (ce1, f1) = comp_strexp(ce,strexp)
	  val ce1 = CE.declare_strid(strid,ce1,CE.emptyCEnv) 
      in case strbind_opt
	   of Some strbind' => 
	     let val (ce2, f2) = comp_strbind(ce,strbind')
	     in (CE.plus(ce1,ce2), f1 o f2)
	     end
	    | None => (ce1, f1)
      end 

  in (*local*)

    fun comp_strdecs(ce,[]) = (CE.emptyCEnv, fn x => x)
      | comp_strdecs(ce, strdec::strdecs) =
      let val (ce1,f1) = comp_strdec(ce,strdec)
	  val (ce2,f2) = comp_strdecs(CE.plus(ce,ce1),strdecs)
      in (CE.plus(ce1,ce2), f1 o f2)
      end

  end (*local*)


  (* -----------------------------------------------------
   * Main compilation function
   * ----------------------------------------------------- *)

  type strdec = TopdecGrammar.strdec
  fun compileStrdecs env strdecs = 
    let val _ = DatBinds.reset()
        val _ = TV.reset()
        val (env1, f1) = comp_strdecs(env, strdecs)

        (* Determine the scope of the declaration. Those lvars and
	 * excons which are declared by the declarations are those
	 * that appear in env1 but not in env. *)

	val typed_declared_lvars = 
              (* we associated the declared_lvars with dummy type schemes;
	       * the real type schemes are put in later; actually, now we 
	       * could put them in... *)
	  let val lvars_env = CE.lvarsOfCEnv env
	      val lvars_decl = 
		List.foldL (fn lv1 => fn lvs =>
			    if List.exists (fn lv => Lvars.eq(lv,lv1)) lvars_env then lvs
			    else lv1::lvs) [] (CE.lvarsOfCEnv env1)
	      val alpha = fresh_tyvar()
	  in map (fn lv => {lvar=lv,tyvars = [alpha],Type=TYVARtype alpha})    (* forall alpha. alpha *)
	     lvars_decl
	  end

       val declared_excons : (Excon.excon * Type Option) list =
	 let val excons_env = CompilerEnv.exconsOfCEnv env
	     val excons_decl = 
	       List.foldL (fn ex1 => fn exs =>
			   if List.exists (fn ex => Excon.eq(ex,ex1)) excons_env then exs
			   else ex1::exs) [] (CE.exconsOfCEnv env1)
	 in map (fn excon => (excon, None)) excons_decl  (*dummy-None*)
	 end

       val scope = FRAME{declared_lvars=typed_declared_lvars,
			 declared_excons=declared_excons}

       (* Build the lambda expression *)
       val lamb = f1 scope

       (* Then we can extract the datbinds *)
       val datbindss = DatBinds.extract()
       val pgm = PGM(DATBINDS datbindss, lamb)
    in (env1, pgm)
    end

  type TypeScheme = StatObject.TypeScheme
  type tyvar = LambdaExp.tyvar
  val compileTypeScheme : TypeScheme -> tyvar list * Type = fn sigma =>
    compileTypeScheme (TypeScheme.to_TyVars_and_Type sigma)

  val _ = CE.set_compileTypeScheme compileTypeScheme   (* MEGA HACK - Martin *)
  val _ = CE.set_normalize_sigma normalize_sigma   (* Another one; Mads *)

  fun reset () = () 

  end;
