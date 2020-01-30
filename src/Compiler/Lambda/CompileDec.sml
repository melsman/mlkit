
(* At some point we should clean up this code.. Constructors are not
 * looked up in the environment anymore.. Also, - it would be nice if
 * primitives were declared in the initial environment (this makes the
 * prelude smaller and better code is generated when compiling
 * multible compilation units (inlining of primitives will happen
 * automatically)). (martin) *)

structure CompileDec: COMPILE_DEC =
  struct

    structure TopdecGrammar = PostElabTopdecGrammar
    structure DecGrammar = TopdecGrammar.DecGrammar
    structure ElabInfo = AllInfo.ElabInfo

    val report_SourceInfo_in_ElabInfo =
        ElabInfo.ParseInfo.SourceInfo.report
	o ElabInfo.ParseInfo.to_SourceInfo
	o ElabInfo.to_ParseInfo

    val tag_values = Flags.is_on0 "tag_values"

    fun chat s = if !Flags.chat then print (s ^ "\n")
		 else ()

    open LambdaExp
    type function = {lvar : lvar, tyvars : tyvar list, Type : Type,
		     bind : LambdaExp}
    open DecGrammar

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
         type CEnv = CE.CEnv
    structure TLE = LambdaExp

    infix plus
    val (op plus) = CE.plus

    fun die s = Crash.impossible ("CompileDec." ^ s)
    fun pr (s : string) : unit = TextIO.output(TextIO.stdOut, s)
    fun pr_st st = (PrettyPrint.outputTree(print,st,100); print "\n")

    val region_profiling = Flags.lookup_flag_entry "region_profiling"

    val line = Report.line
    val // = Report.//
    infix //
    val report_SourceInfo =
            ElabInfo.ParseInfo.SourceInfo.report
	  o ElabInfo.ParseInfo.to_SourceInfo
	  o ElabInfo.to_ParseInfo

    (* ----------------------------------------
     * Environment functions
     * ---------------------------------------- *)

    fun declareExcon(id,(excon,tau),CE) = CE.declareExcon(id,(excon,tau),CE)
    fun declareCon(id,(con,tyvars,tau),CE) = CE.declareCon(id,(con,tyvars,tau),CE)

    datatype lookup_info = NORMAL of ElabInfo.ElabInfo | OTHER of string
    fun lookup_error(kind: string, CE, longid, info:lookup_info) =
            let fun say s = print s
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
	of SOME(CE.EXCON (excon,tau)) => (excon,tau)
         | _ => lookup_error("long exception constructor",CE,longid,info)

    fun lookupLongcon CE longid (info:lookup_info)=
      case CE.lookup_longid CE longid
	of SOME(CE.CON (con,_,_,_)) => con
	 | _ => lookup_error("long value constructor",CE,longid,info)

    fun lookupLongid CE longid (info:lookup_info) =
         case CE.lookup_longid CE longid of
           SOME res => res
         | NONE  => lookup_error("long value variable",CE,longid,info)

    fun lookup_longstrid ce longstrid = CE.lookup_longstrid ce longstrid

    local
      open TLE
    in
      val TLEunit = PRIM(RECORDprim,[])

      fun monoLet((lv,tau,lamb1),lamb2) =
        LET{pat=[(lv,[],tau)],
            bind=lamb1,
            scope=lamb2}
      fun If(e,e1,e2) =
          SWITCH_C(SWITCH(e,[((Con.con_TRUE,NONE),e1)],SOME e2))
    end

    fun new_lvar_from_string_opt NONE = Lvars.newLvar ()
      | new_lvar_from_string_opt (SOME string) = Lvars.new_named_lvar string
    val new_lvar_from_id = Lvars.new_named_lvar o Ident.pr_id
    val new_lvar_from_pat =
          new_lvar_from_string_opt o DecGrammar.find_topmost_id_in_pat
    fun new_lvar_from_pats [pat] = new_lvar_from_pat pat
      | new_lvar_from_pats _ = Lvars.newLvar ()


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


    type datbind_list  = (TLE.tyvar list * TyName * (con * LambdaExp.Type option) list) list

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

    (* --------------------- *)
    (* Unboxing of datatypes *)
    (* --------------------- *)

    local

      (* Return true if the type is potentially unboxed *)
      fun unboxed_ty tns ty =
	case ty
	  of CONStype(_,tn) => TyName.unboxed tn orelse List.exists (fn t => TyName.eq(t,tn)) tns
	   | TYVARtype _ => true
	   | RECORDtype [] => true (*unit*)
	   | RECORDtype _ => false
	   | ARROWtype _ => false
    in

      (* Either all datatypes are unboxed or no datatypes in datbinds are unboxed; restriction caused by
       * Spreading of Datatype bindings in file Regions/SpreadDataType.sml *)

      fun unbox_datbinds (datbinds : datbind_list) : unit =
	let
	  val bucket : TyName list ref = ref nil
	  fun unbox_tyname tn = if List.exists (fn t => TyName.eq(t,tn)) (!bucket) then ()
				else bucket := (tn :: (!bucket))
	  fun unbox_tn_enumeration (_,tn,cns) =
	    let fun nullary (_, NONE) = true
		  | nullary _ = false
	    in if List.all nullary cns then unbox_tyname tn
	       else ()
	    end
	  fun unbox_tn_single (_,tn,[(_,SOME _)]) = unbox_tyname tn
	    | unbox_tn_single _ = ()

	  val max_unary_unboxed = 1
	  fun unbox_tn_combi (_,tn,cns) =
	    let
	      (* under the assumption that tns are unboxed, which
	       * of the tns can we represent unboxed? *)
	      fun one_datbind tns n tn cns =
		case cns
		  of nil => unbox_tyname tn
		   | (_,NONE)::cns => one_datbind tns n tn cns   (*any number of nullary constructors*)
		   | (_,SOME ty)::cns =>
		    if not(unboxed_ty tns ty)
		      andalso n < max_unary_unboxed then one_datbind tns (n+1) tn cns
		    else ()
	    in one_datbind (map #2 datbinds) 0 tn cns
	    end
	in
	   (* Start by unboxing all datatypes consisting
	    * of one unary constructor and all datatypes
	    * consisting of only nullary constructors
	    * (enumerations) *)
(*	    app unbox_tn_single datbinds ; unsafe because of decon, which nullyfies the first two bits... *)
	    app unbox_tn_enumeration datbinds
	  ; app unbox_tn_combi datbinds
	  ; (if length (!bucket) = length datbinds then app TyName.setUnboxed (!bucket)
	     else ())
	end
    end

    (* ----------------------------------------------
     * Compiling type variables
     * ---------------------------------------------- *)

   structure TV : sig val reset : unit -> unit
		      val lookup : string -> TyVar -> TLE.tyvar
		  end =
      struct
	val env : (TyVar, TLE.tyvar) FinMapEq.map ref = ref FinMapEq.empty
	fun look a = FinMapEq.lookup TyVar.eq a
	fun add a = FinMapEq.add TyVar.eq a
	fun lookup s tv =
	      (case look (!env) tv of
		 SOME tv' => tv'
	       | NONE => let val tv' = if TyVar.equality tv then TLE.fresh_eqtyvar()
				       else TLE.fresh_tyvar ()
			 in env := add(tv,tv',!env); tv'
			 end)
		 handle ? => (TextIO.output (TextIO.stdOut, "  [TV.lookup " ^ s ^ "]  ") ;
			      raise ?)

	fun reset() = env := FinMapEq.empty
      end


    (* ---------------------------------------------------------------------- *)
    (*                     Utilities                                          *)
    (* ---------------------------------------------------------------------- *)

    fun to_TypeInfo i =
      case ElabInfo.to_TypeInfo i
	of SOME ti => SOME(TypeInfo.normalise ti)
	 | NONE => NONE

    fun makeList (f: 'a -> 'a option) (x: 'a) =
      x :: (case f x of SOME y => makeList f y
                      | NONE => nil)

    local
      open TypeInfo
    in
      fun type_of_exp (exp: DecGrammar.exp) : StatObject.Type =
        case to_TypeInfo (get_info_exp exp) of
          SOME(EXP_INFO{Type}) => Type
        | _ => die "type_of_exp"

      fun type_of_match (match: DecGrammar.match) : StatObject.Type =
        case to_TypeInfo (get_info_match match) of
          SOME(MATCH_INFO{Type}) => Type
        | _ => die "type_of_match"
    end

    fun map_opt f (SOME x) = SOME (f x)
      | map_opt f NONE = NONE

    fun app_opt f (SOME x) = (f x)
      | app_opt f NONE = ()

    fun NoSome errmsg x =
      case x of
        NONE => die errmsg
      | SOME y => y

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
      foldr (fn ((x,y), env) => declare(x,y,env))
                 CE.emptyCEnv
                 (ListPair.zip(xs,ys))

    fun pr_list (pr: 'a -> string) (l : 'a list) : string =
      let fun pr_l [] = ""
	    | pr_l (a::b::rest) = pr a ^ "," ^ pr_l (b::rest)
	    | pr_l [a] = pr a
      in "[" ^ pr_l l ^ "]"
      end

    fun hd s (x::xs) = x
      | hd s [] = die (s ^ ".hd")



    (* ---------------------------------------------------------------------- *)
    (*           Utility functions used to compile constructors               *)
    (* ---------------------------------------------------------------------- *)

    fun compileExCon (id:id) : excon = Excon.mk_excon(Ident.pr_id id)

    fun compileCon (id:id) : con = Con.mk_con(Ident.pr_id id)


    (* ---------------------------------------------------------------------- *)
    (*         Compilation of the semantic objects Type and TypeScheme        *)
    (* ---------------------------------------------------------------------- *)

    (* compileTyName compiles char and word8 to the default word-type *)

    fun compileTyName tyname =
      if TyName.eq (tyname, TyName.tyName_CHAR) orelse TyName.eq (tyname, TyName.tyName_WORD8) then
	TyName.tyName_WordDefault()
      else tyname

    fun compileType (typ: StatObject.Type) : LambdaExp.Type =
        case Type.to_RecType typ of
          NONE =>
            (case Type.to_ConsType typ of
               NONE =>
                 (case Type.to_FunType typ of
                    NONE =>
                      (case Type.to_TyVar typ of
                         NONE => die "compileType(1)"
                       | SOME tyvar => TYVARtype(TV.lookup "compileType" tyvar))
                  | SOME funtype =>
                      let val (ty1,ty2) = NoSome "compileType(2)"
                                        (Type.un_FunType funtype)
			  val ty1' = compileType ty1
			  val ty2' = compileType ty2
                      in ARROWtype([ty1'],[ty2'])
                      end)
             | SOME constype =>
                 let val (tys,tyname) = NoSome "compileType(3)"
		                        (Type.un_ConsType constype)
		     val tys' = map compileType tys
                 in CONStype(tys', compileTyName tyname)
                 end)
        | SOME rho =>
            let val labtys = Type.RecType.to_list rho
	        val tys' = map (compileType o #2) labtys
            in RECORDtype tys'
            end

      val domType : StatObject.Type -> StatObject.Type =
        #1 o (NoSome "domType(2)") o Type.un_FunType
        o (NoSome "domType(1)") o Type.to_FunType

      fun compileTypeScheme (tyvars : TyVar list, Type : StatObject.Type)
                  : (TLE.tyvar list * TLE.Type)  =
          let val tvs' = map (TV.lookup "compileTypeScheme") tyvars
	      val tau' = compileType Type
	  in (tvs', tau')
	  end handle ? => (print ("type scheme is all[" ^
				  concat (map (fn tv => TyVar.string' (fn ty => ("inst=" ^
										 Type.string ty)) tv ^
					       ", ") tyvars) ^
				  "]." ^ Type.string Type ^ "\n"); raise ?)

      fun compileTyVar tyvar : TLE.tyvar = TV.lookup "compileTyVar" tyvar


    (* ---------------------------------------------------------------------- *)
    (*	       Compilation of constructor bindings                            *)
    (* ---------------------------------------------------------------------- *)

    (* The compilation requires constructor bindings to be on a form where nullary
     * constructors have type scheme \/'a1,...,'an.('a1,...,'an)t and where
     * unary constructors have type \/'a1,...,'an.(tau -> ('a1,...,'an)t), for
     * any tau for which ftv(tau) \subseteq {'a1,...,'an}. The compiler is quite
     * picky on the exact order of the type variables, thus, alpha-conversion of
     * the type schemes are not allowed. *)

   fun on_Type S tau = LambdaBasics.on_Type S tau

   fun unTyVarType (TYVARtype tv) = tv
     | unTyVarType _ = die "unTyVarType"

   fun mk_subst a = LambdaBasics.mk_subst a
   fun on_il(S, il) = map (LambdaBasics.on_Type S) il

   fun pr_tvs tvs = pr_list LambdaExp.pr_tyvar tvs

   fun compile_cb tyvars0 ((con, Type), (env, cons_TypeOpts))
     : CE.CEnv * (con * Type option) list =
     let val con' = compileCon con
         val (tyvars, tauOpt) =
	   case Type
	     of ARROWtype([tau],[CONStype(taus,tyname)]) => (map unTyVarType taus, SOME tau)
	      | CONStype(taus,tyname) => (map unTyVarType taus, NONE)
	      | _ => die "compile_cb: wrong type"
	 (* tyvars should equal tyvars0 *)
     in
       (declareCon(con,(con',tyvars0,Type),env), (con', tauOpt) :: cons_TypeOpts)
     end


    (* ---------------------------------------------------------------------- *)
    (*	       Compilation of the semantic object (tyname, VE)                *)
    (*	       (i.e., almost a TyStr)                                         *)
    (* ---------------------------------------------------------------------- *)

   fun compile'TyStr' (tyname : TyName, VE : VarEnv)
	 : CE.CEnv * TLE.tyvar list * (con * Type option) list =
      let
	val tyvars : TyVar list =
	  let exception H of TyVar list
	  in (VE.CEfold (fn typescheme => fn cbs =>
			  let val (tyvars, _) = TypeScheme.to_TyVars_and_Type typescheme
			  in raise H tyvars
			  end) [] VE) handle H tyvars => tyvars
	  end
	val tyvars' = map (TV.lookup "compile'TyStr'") tyvars
	val cbs : (id * Type) list =
	  VE.CEFold (fn (con, typescheme) => fn cbs =>
		      let val (_, tau) = TypeScheme.to_TyVars_and_Type typescheme
			  val tau' = compileType tau
		      in (con, tau') :: cbs
		      end) [] VE
	val (env, cons_TypeOpts) =
	  foldl (compile_cb tyvars') (CE.emptyCEnv,[]) (rev cbs)
      in
	(env, tyvars', cons_TypeOpts)
      end


   (* Compile type information annotated at special constants; necessary for
    * dealing with overloading of integers and words. *)
   fun typeScon i : Type =
     case to_TypeInfo i
       of SOME(TypeInfo.MATCH_INFO {Type}) => compileType Type
	| SOME(TypeInfo.EXP_INFO {Type}) => compileType Type
	| _ => die "typeScon.wrong type info"


   (* Constructing intinfs *)
   fun digits (x:IntInf.int) : Int32.int list =
       if x = IntInf.fromInt 0 then nil
       else
	 let val maxdigit = IntInf.fromLarge (Int32.toLarge 1073741824)  (* 2^30 *)
	     val rest = IntInf.div(x,maxdigit)
	     val d = IntInf.mod(x,maxdigit)
	 in Int32.fromLarge (IntInf.toLarge d) :: digits rest
	 end

   fun buildIntInf (x : IntInf.int) =
       let val a = IntInf.abs x
  	   val digitsExp =   (* least significant bits first; see kit/basis/IntInf.sml *)
	     foldr (fn (d,C) => PRIM(CONprim{con=Con.con_CONS,instances=[int31Type]},
				     [PRIM(RECORDprim,[INTEGER(d,int31Type),
						       C])]))
		   (PRIM(CONprim{con=Con.con_NIL,instances=[int31Type]},
		         nil))
		   (digits a)

	   val negativeCon =
	       if IntInf.<(x,IntInf.fromInt 0) then Con.con_TRUE
	       else Con.con_FALSE
	   val negativeExp =
	       PRIM(CONprim{con=negativeCon,
			    instances=nil},nil)
       in
	 PRIM(CONprim{con=Con.con_INTINF,instances=nil},
	      [PRIM(RECORDprim,[digitsExp,negativeExp])])
       end



(* ---------------------------------------------------------------------- *)
(*         Pattern match compilation                                      *)
(* ---------------------------------------------------------------------- *)

local

(*The algorithm used in this pattern match compiler is described in
 PETER SESTOFT: ML pattern match compilation and partial evaluation.  In DANVY,
 GLÜCK & THIEMANN (eds): Dagstuhl Seminar on Partial Evaluation (= Lecture
 Notes in Computer Science (no.?)) 1996.
 ftp://ftp.dina.kvl.dk/pub/Staff/Peter.Sestoft/papers/match.ps.gz

 Consider this example input sml program to the kit

	    datatype t = A | B
	    fun f (A, A, _, _) = "0"
	      | f (_, _, A, A) = "1"
	      | f (A, B, A, B) = "last"

 Here is the unoptimised output from the pattern match compiler.  Every right
 hand side only appears once.  "1" has become a function, because two cases
 lead to "1".  The other nodes in the decdag ("rhs" nodes and "ifeq" nodes)
 have been inlined by the pattern match compiler, because it knew that they
 would only be jumped to from one place (i.e., the degree of those nodes in
 the decdag was 1).  "Fail" nodes are always inlined.

Report: UnOpt:

   [...]

   (fn <x>=>
    fix rhs1 = (fn <obj>=> "1")
    in  (case #0 x  of
           A =>
           (case #1 x  of
              A => "0"
            | _ =>
              (case #2 x  of
                 A => (case #3 x  of A => rhs1 () | _ => "last")
               | _ => raisePRIM(Match, []),Types(<string>)))
         | _ =>
           (case #2 x  of
              A =>
              (case #3 x  of A => rhs1 () | _ => raisePRIM(Match, []),Types(<string>))
            | _ => raisePRIM(Match, []),Types(<string>)))
    end)

 Naturally, the optimiser inlines the very small function body "1":

Report: Opt:

  [...]

   (fn <x'>=>
    (case #0 x'  of
       A =>
       (case #1 x'  of
          A => "0"
        | _ =>
          (case #2 x'  of
             A => (case #3 x'  of A => "1" | _ => "last")
           | _ => raisePRIM(Match, []),Types(<string>)))
     | _ =>
       (case #2 x'  of
          A => (case #3 x'  of A => "1" | _ => raisePRIM(Match, []),Types(<string>))
          | _ => raisePRIM(Match, []),Types(<string>))))

 If we make the pattern bigger, there will be more nodes in the decdag that
 have more than one in-edge, & thus there will be more functions in the fix
 created by the pattern matcher:

	  fun f (A, A, _, _, _, _) = "0"
	    | f (_, _, A, A, _, _) = "1"
	    | f (_, _, _, _, A, A) = "2"
	    | f (A, B, A, B, A, B) = "last"

Report: UnOpt:

 (fn <y>=>
  fix n5_A? =
        (fn <obj>=>
          (case #4 y of
             A => (case #5 y of A => rhs2 () | _ => raisePRIM(Match, []),Types(<string>))
           | _ => raisePRIM(Match, []),Types(<string>))),
      rhs2 = (fn <obj>=> "2"),
      rhs1 = (fn <obj>=> "1")
  in  (case #0 y of
         A =>
         (case #1 y of
            A => "0"
          | _ =>
            (case #2 y of
               A =>
               (case #3 y of
                  A => rhs1 ()
                | _ =>
                  (case #4 y of
                     A => (case #5 y of A => rhs2 () | _ => "last")
                   | _ => raisePRIM(Match, []),Types(<string>)))
             | _ => n5_A? ()))
       | _ =>
         (case #2 y of
            A => (case #3 y of A => rhs1 () | _ => n5_A? ())
          | _ => n5_A? ()))
  end)

Report: Opt:

 (fn <y'>=>
  fix n5_A? =
         (fn <obj>=>
          (case #4 y'  of
             A => (case #5 y' of A => "2" | _ => raisePRIM(Match, []),Types(<string>))
           | _ => raisePRIM(Match, []),Types(<string>)))
  in  (case #0 y' of
         A =>
         (case #1 y' of
            A => "0"
          | _ =>
            (case #2 y' of
               A =>
               (case #3 y' of
                  A => "1"
                | _ =>
                  (case #4 y' of
                     A => (case #5 y' of A => "2" | _ => "last")
                   | _ => raisePRIM(Match, []),Types(<string>)))
             | _ => n5_A? ()))
       | _ =>
         (case
            #2 y' of
            A => (case #3 y' of A => "1" | _ => n5_A? ()) | _ => n5_A? ()))
  end)
 *)

  abstype span = Infinite | Finite of int
  with
    val span_from_int = Finite
    val span_infinite = Infinite
    val span_256 = Finite 256
    val span_1 = Finite 1
    fun span_eq Infinite Infinite = true
      | span_eq (Finite i1) (Finite i2) = i1 = i2
      | span_eq _ _ = false
    fun span_eq_int Infinite i = false
      | span_eq_int (Finite i1) i2 = i1 = i2
  end (*abstype*)

  datatype con = Con of {longid : longid,
			 span : span,
			 nullary : bool,
			 info : ElabInfo.ElabInfo}
               | Scon of SCon.scon * Type
	       | Excon of {longid : longid, nullary : bool}
               | Tuple of {arity : int}
  (*nullary=true when the (exception) constructor takes zero arguments;
   otherwise it takes one argument.*)

  fun string_from_con0 (Con {longid, ...}) = "Con {" ^ Ident.pr_longid longid ^ ", ...}"
    | string_from_con0 (Scon (scon,_)) = "Scon " ^ SCon.pr_scon scon
    | string_from_con0 (Excon {longid, ...}) = "Excon {" ^ Ident.pr_longid longid ^ ", ...}"
    | string_from_con0 (Tuple {arity}) = "Tuple {" ^ Int.toString arity ^ "}"

  (*the type cmp, and the _cmp functions scattered around are used to define
   function ifeq_cmp which is used to define an orderfinmap that keeps track
   of what IfEq nodes have been created.  This is necessary because we want
   to build a dag, not a tree.*)

  datatype cmp = Lt | Gt | Eq

  fun cmp_from_lt lt (x1, x2) = if lt (x1, x2) then Lt
				else if lt (x2, x1) then Gt else Eq
  fun eq_from_cmp x_cmp (x1, x2) = (case x_cmp (x1, x2) of Eq => true | _ => false)
  fun lt_from_cmp x_cmp (x1, x2) = (case x_cmp (x1, x2) of
				      Lt => true
				    | Gt => false
				    | Eq => false)

  val int_cmp = cmp_from_lt (op < : int * int -> bool)
  val lvar_cmp = cmp_from_lt Lvars.lt
  val scon_cmp = cmp_from_lt SCon.lt
  (*of course, longid_cmp ought to be in IDENT.sml & this is hacky, but then
  I would have to define datatype cmp somewhere else:*)
  fun longid_lt (longid1, longid2) =
	Ident.pr_longid longid1 < Ident.pr_longid longid2
  val longid_cmp = cmp_from_lt longid_lt

  fun con_ord (Con _) = 0
    | con_ord (Scon _) = 1
    | con_ord (Excon _) = 2
    | con_ord (Tuple _) = 3
  fun con_cmp (Con {longid=longid1, span=span1, nullary=nullary1, ...},
	       Con {longid=longid2, span=span2, nullary=nullary2, ...}) =
	longid_cmp (longid1, longid2) (*I think it must suffice to compare the longids?*)
    | con_cmp (Scon (scon1,_), Scon (scon2,_)) = scon_cmp (scon1, scon2)  (* same type when compared, thus
									   * we don't need to compare infos *)
    | con_cmp (Tuple {arity=arity1}, Tuple {arity=arity2}) = int_cmp (arity1, arity2)
    | con_cmp (Excon {longid=longid1, ...}, Excon {longid=longid2, ...}) =
	longid_cmp (longid1, longid2)
    | con_cmp (con1, con2) = int_cmp (con_ord con1, con_ord con2)

  val con_eq = eq_from_cmp con_cmp

  fun arity (Con {nullary, ...}) = if nullary then 0 else 1
    | arity (Scon scon) = 0
    | arity (Excon {nullary, ...}) = if nullary then 0 else 1
    | arity (Tuple {arity}) = arity

  fun span (Con {span, ...}) = span
    | span (Scon (SCon.INTEGER x, _)) = span_infinite
    | span (Scon (SCon.STRING x, _)) = span_infinite
    | span (Scon (SCon.REAL x, _)) = die "span: real scon in pattern"
    | span (Scon (SCon.CHAR x, _)) = span_256
    | span (Scon (SCon.WORD x, _)) = span_infinite
    | span (Excon _) = span_infinite
    | span (Tuple {arity}) = span_1

  datatype path = Access of int * con * path | Obj
    (*a path is a description of how to access a component value of the value
     that is being pattern matched.  For instance, if the value being matched
     against is `(0, Bah (0,1,v,4))', the path

        Access (2, Tuple foo,
	        Access (0, Con {longid=Bah, ...},
		        Access (1, Tuple hoo,
			        Obj)))

     points out v, i.e., the first element in the path is the "innermost"
     part of the path.*)


  local

      (* Path environments are used by the pattern match compiler. There
       are two environments in play - one for sharing code for
       extracting components from the top-node and one for allowing
       pretty-printing to print cases nicely, without use of DECON
       constructs. We keep these two environments apart by prepending
       different numbers onto the paths for each env... mael 2002-11-04
       *)

      type spath = int list
      fun to_spath i p : spath =
	  let fun pa (Obj,acc) = i :: acc
		| pa (Access(i,c,p),acc) = pa (p,i::con_ord c::acc)
	  in pa(p,nil)
	  end
  in
      fun lookupLvarDecon e p =
	  case CE.lookupPath e (to_spath 1 p) of
	      SOME (lv,_) => SOME lv
	    | NONE => NONE
      fun declareLvarDecon (p, lv, e) = CE.declarePath (to_spath 1 p, lv, RECORDtype nil, e)

      fun lookupPath e p = CE.lookupPath e (to_spath 0 p)
      fun declarePath (p, lv, tau, e) = CE.declarePath (to_spath 0 p, lv, tau, e)
  end

  fun string_from_path (Access (i, con, path)) =
	"Access ("
	^ Int.toString i ^ ", "
	^ string_from_con0 con ^ ", "
	^ string_from_path path ^ ")"
    | string_from_path Obj = "Obj"

  fun path_cmp (Obj, Obj) = Eq
    | path_cmp (Obj, Access _) = Lt
    | path_cmp (Access _, Obj) = Gt
    | path_cmp (Access (i1, pcon1, path1), Access (i2, pcon2, path2)) =
	(case int_cmp (i1, i2) of
	   Eq => (case con_cmp (pcon1, pcon2) of
		    Eq => path_cmp (path1, path2)
		  | cmp => cmp)
	 | cmp => cmp)

  val path_eq = eq_from_cmp path_cmp

  type declaration_to_be_made = id * (lvar * LambdaExp.tyvar list * Type) * path
  type declarations_to_be_made = declaration_to_be_made list
  type rhs = int
  type rhs' = declarations_to_be_made * rhs

  val string_from_rhs = Int.toString
  fun string_from_rhs' (_, rhs) = string_from_rhs rhs

  structure conset : KIT_MONO_SET = OrderSet
      (struct
	   type T = con
	   fun lt con1 con2 = lt_from_cmp con_cmp (con1, con2)
       end) (*structure Order*)

  structure negset = struct
    type negset = int (*number of cons in the set*) * conset.Set (*the cons*)
    val empty = (0, conset.empty)
    fun member con (i, conset) = conset.member con conset
    fun insert con (i, conset) = (i + 1, conset.insert con conset)
    fun size (i, _) = i
  end (*structure negset*)


  datatype termd = Pos of con * termd list | Neg of negset.negset
  type context = (con * termd list) list

  fun addneg (Neg negset, con) = Neg (negset.insert con negset)
    | addneg (dsc,_) = dsc

  fun augment ([], termd) = []
    | augment ((con, args) :: rest, termd)  = (con, termd :: args) :: rest

  fun norm ((con, args) :: rest) = augment (rest, Pos (con, rev args))
    | norm _ = die "norm"

  fun buildtermd ([], termd, []) = termd
    | buildtermd ((con,args) :: rest, termd, (_, _, dargs) :: work) =
	buildtermd (rest, Pos (con, rev args @ (termd :: dargs)), work)
    | buildtermd _ = die "buildtermd"

  datatype matchresult = Yes | No | Maybe

  (* See Peter Sestoft's paper "ML pattern match compilation and
     partial evaluation", pages 6-7 for a description of the cases (a)
     to (e) below. *)

  fun staticmatch (con : con, termd : termd) : matchresult =
      let fun maybefy (Con {longid,...}) m =
	      if Ident.id_REF = #2(Ident.decompose longid) then m
	      else Maybe
	    | maybefy _ m = m
      in case termd of
	  Pos (pcon, _) =>
	      (case con_cmp (con, pcon) of
		   Eq => Yes   (* case(a) *)
		 | _ => (case pcon of
			     Excon _ => Maybe (* Different excons may have same name *)
			   | _ => No)) (* case(b) *)
	| Neg negset  =>
		   if negset.member con negset then
		       No          (* case(c) *)
		   else
		       if span_eq_int (span con) (negset.size negset + 1) then
			   Yes     (* case(d) *)
		       else Maybe  (* case(e) *)
      end

  datatype kind = Success of rhs' | IfEq of path * con * node option * node option
  withtype node = {lvar : lvar, kind : kind, refs : int ref, visited : bool ref}

  type edge = node option
  type ifeq = path * con * edge * edge
  type decdag = edge


  (*A  d e c i s i o n  d a g  is a dag (mas o menos) with edges of type t &
   nodes of type t'.  An edge either points to nothing (i. e., it is NONE :
   t) and means "fail", or it points to (is) a node & means "jump to that
   node".  A node is either a right hand side (of type rhs, i. e., a Success
   node) or a test with two branches, i.e., two edges (an IfEq node).  Each
   node will be compiled into a LambdaExp function.  (well, compile_match
   does that; compile_binding is a bit different).  The lvar of the node will
   be the name of that function and also serves as unique identification of
   the node.  Correspondingly, edges to nodes will be compiled to function
   calls.  Failure edges (i.e., NONE : t) will be compiled into "raise
   something".

   The "refs" field in a node is the number of edges to the node. When a node
   is created, it is set to 0, because we do not know at that point, whether
   anyone will ever jump to the node.  It is only when we make an edge to the
   node that we bump this counter.

   Invariant: "visited" fields of all nodes are always false except within
   calls to reachable and compile_decdag.*)

  fun shared ({refs, ...} : node) = !refs > 1

  fun node_cmp ({lvar=lvar1, ...} : node, {lvar=lvar2, ...} : node) =
	lvar_cmp (lvar1, lvar2)
  fun opt_cmp x_cmp (NONE, NONE) = Eq
    | opt_cmp x_cmp (NONE, SOME _) = Lt
    | opt_cmp x_cmp (SOME _, NONE) = Gt
    | opt_cmp x_cmp (SOME x1, SOME x2) = x_cmp (x1, x2)
  val edge_cmp = opt_cmp node_cmp
  val edge_eq = eq_from_cmp edge_cmp

  fun ifeq_cmp ((path1 : path, con1 : con, then1 : edge, else1 : edge),
		(path2 : path, con2 : con, then2 : edge, else2 : edge)) =
	(case edge_cmp (else1, else2) of
	   Eq => (case edge_cmp (then1, then2) of
		    Eq => (case con_cmp (con1, con2) of
			     Eq => path_cmp (path1, path2)
			   | cmp => cmp)
		  | cmp => cmp)
	 | cmp => cmp)

  local
    structure map = OrderFinMap
      (struct
	 type T = ifeq
	 (*lt ifeq1 ifeq2 = lexicographic ordering of the components of the
	  tuples.  As long as it is a linear order, I can choose any
	  ordering, so I compare the components in an order such that in the
	  frequent cases it is determined as quickly as possible whether
	  ifeq1 < ifeq2.*)
	 fun lt ifeq1 ifeq2 = lt_from_cmp ifeq_cmp (ifeq1, ifeq2)
       end)

    type mapr = node map.map ref
    val mapr : mapr = ref map.empty
    fun find_ifeq_node_like_this ifeq : node option = map.lookup (!mapr) ifeq
    val counter = ref 0
    fun next () = (counter := 1 + !counter ; !counter)
    fun string_from_con (Con {longid, ...}) = Ident.pr_longid longid
      | string_from_con (Scon (SCon.INTEGER i, _)) = IntInf.toString i
      | string_from_con (Scon (SCon.STRING s, _)) = "a_string"
      | string_from_con (Scon (SCon.REAL r, _)) = "a_real"
      | string_from_con (Scon (SCon.CHAR c, _)) = "a_char"
      | string_from_con (Scon (SCon.WORD w, _)) = Word32.toString w
      | string_from_con (Excon {longid, ...}) = Ident.pr_longid longid
      | string_from_con (Tuple {arity}) = "a_tuple"

(*TODO 06/01/1998 10:55. tho.  kan man bruge indkanttallet ("refs") til
noget?  F. eks. til at se, om koden er død (fordi der er redundante regler)?
Problemet er, hvis der kan forekomme en ifeq-knude med indgrad 0.  Så er den
"død", & det er dens børn måske også, men det kan man ikke se af deres
indkanttal, for det taltes op, da den "døde" ifeq-knude blev skabt ...
Det finder du nok aldrig ud af.*)

  in
    val edge_bump = app_opt (fn {refs, ...} : node => refs := !refs + 1)

    fun mk_node kind s = {kind = kind, refs = ref 0, visited = ref false,
			  lvar = Lvars.new_named_lvar s}
    fun mk_ifeq_node (ifeq as (path, con, edge1, edge2)) : node =
	  let val node = mk_node (IfEq ifeq) (string_from_con con ^ "_n" ^ Int.toString (next ()))
	  in
	    mapr := map.add (ifeq, node, !mapr) ;
	    edge_bump edge1 ;
	    edge_bump edge2 ;
	    node
	  end

    fun reset () = (mapr := map.empty ; counter := 0)
    fun ifeq_node ifeq : node =
	  (case find_ifeq_node_like_this ifeq of
	     SOME node => node
	   | NONE => mk_ifeq_node ifeq)

    val fail_edge = NONE
  end (*local*)


  type work = pat list * path list * termd list
  type rule = pat * node

  fun fail (termd : termd, []) = fail_edge
    | fail (termd, (pat1, rhs1) :: rulerest) =
	match_pat (pat1, Obj, termd, [], [], rhs1, rulerest)
  and succeed (ctx : context, [] : work list, rhs : node, rules : rule list) =
	SOME rhs
    | succeed (ctx, work1::workr, rhs, rules) =
	(case work1 of
	   ([],[],[]) => succeed (norm ctx, workr, rhs, rules)
	 | (pat1::patr, path1::pathr, termd1::termdr) =>
	     match_pat (pat1, path1, termd1, ctx,
			(patr, pathr, termdr)::workr, rhs, rules)
	 | _ => die "succeed")
  and match_pat (pat : pat, path : path, termd : termd, ctx : context,
		 work : work list, rhs : node, rules : rule list) =
	(case pat of
	   ATPATpat (info, atpat) => match_atpat (atpat, path, termd, ctx, work, rhs, rules)
	 | CONSpat (info, longid_op_opt, atpat) =>
	     (case to_TypeInfo info of
		SOME (TypeInfo.CON_INFO {numCons:int, longid:longid, ...}) =>
		  match_con (Con {longid=longid, span=span_from_int numCons,
				  info=info,
		                  (*because it appears in a CONSpat:*)nullary=false})
		    [(0, ATPATpat (DecGrammar.bogus_info, atpat))]
		      (path, termd, ctx, work, rhs, rules)
	      | SOME (TypeInfo.EXCON_INFO {longid, ...}) =>
		  match_con (Excon {longid=longid,
				    (*because it appears in a CONSpat:*)nullary=false})
		    [(0, ATPATpat (DecGrammar.bogus_info, atpat))]
		      (path, termd, ctx, work, rhs, rules)
	       | _ => die "match_pat (CONSpat ...)")
	 | TYPEDpat (info, pat, ty) =>
	     match_pat (pat, path, termd, ctx, work, rhs, rules)
	 | LAYEREDpat (info, OP_OPT (id, _), ty_opt, pat) =>
	     match_pat (pat, path, termd, ctx, work, rhs, rules)
	 | UNRES_INFIXpat _ => die "match_pat (UNRES_INFIXpat ...)")

  and match_atpat (atpat, path, termd, ctx, work, rhs, rules) =
	(case atpat of
	   WILDCARDatpat info => succeed (augment (ctx, termd), work, rhs, rules)
	     (*WILDCARDatpat is treated almost like variable patterns*)
	 | SCONatpat (info, scon) =>
           let val tau = typeScon info
           in (* if typeIsIntInf tau then
                match_intinf (Scon (scon, tau)) [] (path, termd, ctx, work, rhs, rules)
              else *)
                match_con (Scon (scon, tau)) [] (path, termd, ctx, work, rhs, rules)
           end
	 | LONGIDatpat (info, OP_OPT (longid, _), _) =>
	     (case to_TypeInfo info of
		SOME (TypeInfo.VAR_PAT_INFO _) =>
		  (*it is a variable, not a nullary constructor*)
		  succeed (augment (ctx, termd), work, rhs, rules)
	      | SOME (TypeInfo.CON_INFO {numCons   : int,
					 longid    : longid, ...}) =>
		  match_con (Con {longid=longid, span=span_from_int numCons, info=info,
		                  (*because it appears in a LONGIDatpat:*)nullary=true})
		    [] (path, termd, ctx, work, rhs, rules)
	      | SOME (TypeInfo.EXCON_INFO {longid, ...}) =>
		  match_con (Excon {longid=longid,
				    (*because it appears in a LONGIDatpat:*)nullary=true})
		    [] (path, termd, ctx, work, rhs, rules)
	      | _ => die "match_atpat (LONGIDatpat ...)")
	 | RECORDatpat (info, patrow_opt) =>
	     let val patrows = (case patrow_opt of
		       NONE => []
		     | SOME patrow => makeList
			 (fn PATROW (_, _, _, patrow_opt) => patrow_opt
		           | DOTDOTDOT _ => die "match_atpat: DOTDOTDOT")
			 patrow)
	       val argpats =
		     map (fn PATROW (info, _, pat, _) =>
			  (case to_TypeInfo info of
			     SOME (TypeInfo.LAB_INFO {index, ...}) => (index, pat)
			   | _ => die "match_atpat: RECORDatpat info")
		           | _ => die "match_atpat: RECORDatpat patrow") patrows
	       val argpats = ListSort.sort (fn (a,_) => fn (b,_) => a < b) argpats
	     in
	       match_con (Tuple {arity=List.length argpats}) argpats
	         (path, termd, ctx, work, rhs, rules)
	     end
	 | PARatpat (info, pat) => match_pat (pat, path, termd, ctx, work, rhs, rules))
(*
  and match_intinf pcon (argpats : (int * pat) list)
	(path, termd, ctx, work, rhs, rules) =
	let val pcon = Tuple {arity=2}
        in match_con pcon (path, termd, ctx, work, rhs, rules)
        end
*)

  (*match_con pcon argpats (path, termd, ctx, work, rhs, rules) = `pcon' is
   the compile-time description of the constructor that we want to generate
   code to match.  `argpats' are the argument patterns to `pcon', each paired
   with its "Access number", the argument to the constructor Access that will
   access the component corresponding to that pattern.  The rest of the
   arguments of match_con describe the state of the pattern match compiler as
   they do in the other functions (`succeed', `match_pat' etc.).*)

  and match_con pcon (argpats : (int * pat) list)
	(path, termd, ctx, work, rhs, rules) =
	let
	  fun getdargs (Neg _) = map (fn _ => Neg negset.empty) argpats
	    | getdargs (Pos (con, dargs)) = dargs
	  fun getoargs () = map (fn (i, pat) => Access (i, pcon, path)) argpats
	  fun succeed' () = succeed
		((pcon, []) :: ctx,
		 (map #2 argpats, getoargs (), getdargs termd) :: work,
		 rhs,
		 rules)
	  fun fail' newtermd = fail (buildtermd (ctx, newtermd, work), rules)
	in
	  (case staticmatch (pcon, termd) of
	     Yes => succeed' ()
	   | No => fail' termd
	   | Maybe => let val left = succeed' ()
			  val right = fail' (addneg (termd, pcon))
		      in if edge_eq (left, right) then left
			 else SOME (ifeq_node (path, pcon, left, right))
		      end)
	end

  fun env_from (declarations_to_be_made : declarations_to_be_made) =
	foldl
	(*foldr could also have been used, as no id appears twice in
	 declarations_to_be_made because of syntactic restrictions enforced
	 by ElabDec*)
	  (fn ((id, (lvar, tyvars, tau), path), env_sofar) =>
	   CE.declareVar (id, (lvar, tyvars, tau), env_sofar))
	  CE.emptyCEnv declarations_to_be_made

  fun pr_tau tau = (PrettyPrint.flatten1 o LambdaExp.layoutType) tau
  fun pr_il il = pr_list pr_tau il

  fun compile_path env (obj:LambdaExp*Type) path
      : (LambdaExp -> LambdaExp) * LambdaExp * Type * CE.CEnv =
      case lookupPath env path of
	  SOME (lvar,tau) => (fn x => x, VAR {lvar=lvar,instances=nil}, tau, CE.emptyCEnv)
	| NONE => compile_path0 env obj path

  and compile_path0 env (obj,tau) Obj = (fn x => x, obj, tau, CE.emptyCEnv)
    | compile_path0 env (obj as (obj_e,_)) (path0 as Access (0, Con {info, ...}, path)) =
	(case to_TypeInfo info of
	   SOME (TypeInfo.CON_INFO {longid, instances, ...}) =>
	     (case lookupLongid env longid (NORMAL info) of
		CE.CON (con, tyvars, Type, il) =>  (* because the con occurs in the pattern, we
						    * have {instances'/tyvars}il = il'. *)
		  let
		      (* To improve pretty-printing of DECON's, we lookup the
		       * lvar bound to the unary constructor in the case
		       * construct. This is an optional lvar because there
		       * are cases where pattern matching introduces
		       * functions, which take the obj root lvar as the
		       * single argument; these functions are hoisted, thus
		       * in the body of the functions the lvar is not in
		       * scope... The pretty-printer solves the problem in
		       * this case by introducing a dummy case. *)
		      val lv_opt = lookupLvarDecon env path

		      val il' = map compileType instances
		      val (f,e,tau',env') = compile_path env obj path
		      val tau =
			  case Type of
			      ARROWtype ([tau],_) =>
				  let val S = mk_subst (fn _ => "compile_path0.con")
				      (tyvars,il')
				  in on_Type S tau
				  end
			    | _ => die "Unary constructor does not have well-formed arrow-type"
(*
		      val _ = (print ("compile_path " ^ Con.pr_con con ^ "\n");
			       print ("  Type = " ^ pr_tau Type ^ "\n");
			       print ("  tau  = " ^ pr_tau tau ^ "\n");
			       print ("  tau' = " ^ pr_tau tau' ^ "\n"))
*)
		      val decon = PRIM (DECONprim {con=con, instances=il',
						   lv_opt=lv_opt}, [e])
		  in
		      case obj_e of
			  VAR{instances= _ :: _ , ...} => (f, decon, tau, env')
			| _ =>
			let
			    val lvar = Lvars.newLvar()
			    val f' = fn x => LET{pat=[(lvar,nil,tau)],
						 bind=decon,
						 scope= x}
			    val env'' = declarePath(path0, lvar, tau, CE.emptyCEnv)
			in
			    (f o f',
			     VAR{lvar=lvar,instances=nil},
			     tau,
			     CE.plus(env',env''))
			end
		  end

	      | CE.REF =>
		  (case instances of [instance] =>
		       (*for deref, the instance is the instantiated argument type:*)
		       let
			   val (f,e,_,env') = compile_path env obj path
			   val tau = compileType instance
		       in
			   (f, PRIM (DEREFprim {instance=CONStype ([tau],
								   TyName.tyName_REF)},
				     [e]),
			    tau,
			    env')
		       end
		   | _ => die "compile_path: ref instances")
	      | _ => die "compile_path: Con, longid")
	 | _ => die "compile_path: Con, info")
    | compile_path0 env obj (Access (_, Con _, path)) = die "compile_path: _, Con _"
    | compile_path0 env obj (Access (i, Excon {longid, ...}, path)) =
	     let
		 val (f,e,_,env') = compile_path env obj path
		 val (excon,tau) = lookupLongexcon env longid (OTHER "compile_path")
		 val tau =
		     case tau of
			 ARROWtype ([tau],_) => tau
		       | _ => die "Unary exconstructor does not have well-formed arrow-type"
	     in (f, PRIM (DEEXCONprim excon, [e]),
		 tau,
		 env')
	     end
    | compile_path0 env (obj as (obj_e,_)) (path0 as Access (i, Tuple {arity}, path)) =
	if 0 <= i andalso i < arity
	then
	    let
		val (f,e,tau,env') = compile_path env obj path
		val tau =
		    case tau of
			RECORDtype ts =>
			    if length ts = arity then List.nth (ts,i)
			    else die "Wrong record arity"
		      | _ => die ("compile_path0.RECORDtype expected. Type is "
				  ^ PrettyPrint.flatten1 (layoutType tau))
		val select = PRIM (SELECTprim i, [e])
	    in
		case obj_e of
		    VAR {instances= _ :: _ , ...} => (f, select, tau, env')
		  | _ =>
		  let
		      val lvar = Lvars.newLvar()
		      val f' = fn x => LET{pat=[(lvar,nil,tau)],
					   bind=select,
					   scope = x}
		      val env'' = declarePath(path0,lvar,tau,CE.emptyCEnv)
		  in (f o f',
		      VAR{lvar=lvar,instances=nil},
		      tau,
		      CE.plus(env',env''))
		  end
	    end
	else die "compile_path: i's range"
    | compile_path0 env obj (Access (_, Scon _, path)) = die "compile_path: scon"


  (*compile_rhs = TODO

   How? In

       fn ((42, (x, _)), (_, {y, ...})) => e1
        | ((_, (_, z)), (_, {y, ...})) => e2

   we will want to compile e1 to

      LET x = <code to access x>
      IN LET y = <code to access y>
      IN <e1 compiled in an environment that says something about x & y>

   The rhs for the first mrule will say something about x & y.  First, we use
   this information to build the abovementioned environment.  Then we compile
   e1 in it.  Then we add the surrounding LET's, again using the
   aforementioned information.*)

  fun compile_rhs' compile_no obj env rhs'  : LambdaExp =
	let
	  val (declarations_to_be_made : declarations_to_be_made, rhs) = rhs'
	  val env_rhs = CE.plus (env, env_from declarations_to_be_made)
	  val lexp = compile_no (rhs, env_rhs)
	in mk_declarations_to_be_made declarations_to_be_made obj lexp env
	end

  and mk_declarations_to_be_made declarations_to_be_made obj e env
	: LambdaExp =
      let val (f, e, env) =
	    foldl  (*again, foldr could also have been used*)
	    (fn ((id, (lvar, tyvars, tau), path), (f,e,env)) =>
	     let val obj =
                     case obj of
                       (VAR{lvar=lv,instances},tau) =>
                       let
                         fun member tv = List.exists (fn tv' => tv = tv') tyvars
                         fun f (t as TYVARtype tv) =
                             if member tv then t
                             else intDefaultType()   (* see compilation of test/pat.sml *)
                           | f t = t
                       in (VAR{lvar=lv,instances=map f instances}, tau)  (* MEMO: maybe instantiate tau properly? *)
                       end
                     | _ => die "mk_declarations_to_be_made.skip0"
                 val (f',e',_,env') = compile_path env obj path
	     in
		 case tyvars of
		     nil => (f o f', LET {pat = [(lvar, tyvars, tau)],
					  bind = e',
					  scope = e},
			     CE.plus(env, env'))
		   | _ => (f, LET {pat = [(lvar, tyvars, tau)],
				   bind = f' e',
				   scope = e},
			   env)
	     end)
	    (fn x=>x,e,env) declarations_to_be_made
      in f e
      end

in

  fun compile_jump_to ({lvar, ...} : node) =
	APP (VAR {lvar = lvar, instances = []}, PRIM (RECORDprim, []), NONE)
	  (*instances=[] because the var can never be polymorphic
	   because it is the name of a non-polymorphic function.*)

  fun switchify (path0, con0, edge1, edge2) =
	let fun switchify0 (s as (cases, SOME {kind=IfEq (path, con, edge1, edge2), refs, ...})) =
	          if path_eq (path0, path) andalso !refs <= 1
		  then switchify0 ((con, edge1) :: cases, edge2)
		  else s
	      | switchify0 s = s
	    val (cases, otherwise) = switchify0 ([(con0, edge1)], edge2)
	in  case con0 of
	      Con {span,info,...} =>
		  if span_eq_int span (length cases) then
		      (* no need for the otherwise branch *)
		      (cases, NONE)
		  else (cases, SOME otherwise)
	      | _ => (cases, SOME otherwise)
	end

  fun compile_node compile_no obj raise_something tau_return_opt env
	({visited, kind, ...} : node)    : function list * LambdaExp =
	(if !visited then die "compile_node: already visited" else () ;
	 visited := true ;
	 (case kind of
	   IfEq (path, con, edge1, edge2) =>
	     let
	       val (cases, def) = switchify (path, con, edge1, edge2)
	       fun switch (switch_x : 'x Switch -> LambdaExp,
			   compile_x : con * CE.CEnv -> 'x * CE.CEnv) : function list * LambdaExp =
		   let
		       val (f,e,_,env') = compile_path env obj path
		       val env = CE.plus(env,env')
		       val (functions, cases') =
		         foldl (fn ((x, edge), (functions, cases')) =>
				let val (x',env') = compile_x (x,env)
				    val (functions', lexp') =
					compile_edge compile_no obj raise_something
					tau_return_opt env' edge
				in (functions' @ functions, (x', lexp') :: cases')
				end)
			 ([], []) cases
		       val (functions', def') =
			   case def of
			       SOME def =>
				   let val (functions', def') =
				         compile_edge compile_no obj raise_something
					 tau_return_opt env def
				   in (functions', SOME def')
				   end
			     | NONE => (nil, NONE)
		   in (functions' @ functions,
		       f (switch_x (SWITCH (e, cases', def'))))
		   end
               fun switch_ii (SWITCH (e:LambdaExp, cases:(IntInf.int*LambdaExp)list, def:LambdaExp option)) : LambdaExp =
                   let val lv = Lvars.newLvar ()
                       fun convertCases ([(_,exp:LambdaExp)],NONE) :LambdaExp = exp
                         | convertCases (nil,SOME a)   = a
                         | convertCases ((x,b)::cases,def:LambdaExp option) =
                           If(PRIM(EQUALprim {instance=RECORDtype[intinfType,intinfType]},
                                   [VAR{lvar=lv,instances=nil},
                                    buildIntInf x]),
                              b,
                              convertCases(cases,def))
                         | convertCases (nil,NONE) = die "Switch.intinf - no cases"
                   in monoLet((lv,intinfType,e),
                              convertCases(cases,def))
                   end
	       fun precision (t: Type) : int =
		 let val tn = case t
				of CONStype(nil, tn) => tn
				 | _ => die "precision.not CONStype"
		     open TyName
		 in if eq (tn, tyName_INTINF) then ~1
		    else if eq (tn, tyName_INT31) orelse eq (tn, tyName_WORD31) then 31
		    else if eq (tn, tyName_INT32) orelse eq (tn, tyName_WORD32) then 32
		    else die ("precision. tn = " ^ pr_TyName tn ^ " not expected")
		 end
	     in
	       (case con of
		  Con _ => switch
		    (SWITCH_C,
		     fn (Con {longid, nullary, ...},env) =>
		     if nullary then ((lookupLongcon env longid (OTHER "compile_node, Con, nullary"), NONE), env)
		     else
			 let val lv' = Lvars.newLvar()
			 in ((lookupLongcon env longid (OTHER "compile_node, Con"), SOME lv'),
			     declareLvarDecon (path, lv', env))
			 end
		      | _ => die "compile_node: fn Con =>")
		| Scon (SCon.INTEGER _, tau) =>
                  (case precision tau of
                     ~1 => (* intinf *)
                     switch (switch_ii,
                             fn (Scon (SCon.INTEGER i,_),env) => (i,env)
		              | _ => die "compile_node: fn Scon (SCon.INTEGER i) =>")
                   | p =>
                     switch
		         (fn sw => SWITCH_I{switch=sw,precision=p},
		          fn (Scon (SCon.INTEGER i,_),env) =>
		             ((Int32.fromLarge(IntInf.toLarge i),env)
			      handle _ => die "IntInf in patterns not implemented")
		           | _ => die "compile_node: fn Scon (SCon.INTEGER i) =>"))
		| Scon (SCon.CHAR _, tau) => switch
		    (fn sw => SWITCH_W {switch=sw, precision=precision tau},
		     fn (Scon (SCon.CHAR i,_),env) => (Word32.fromInt i,env)
		      | _ => die "compile_node: fn Scon (SCon.CHAR i) =>")
		| Scon (SCon.WORD _, tau) => switch
		    (fn sw => SWITCH_W{switch=sw, precision=precision tau},
		     fn (Scon (SCon.WORD w,_),env) => (w,env)
		      | _ => die "compile_node: fn Scon (SCon.WORD w) =>")
		| Scon (SCon.STRING _,_) => switch
		    (SWITCH_S,
		     fn (Scon (SCon.STRING s,_),env) => (s,env)
		      | _ => die "compile_node: fn Scon (SCon.STRING s) =>")
		| Scon (SCon.REAL _,_) => die "compile_node: real"
		| Excon _ => switch
		    (SWITCH_E,
		     fn (Excon {longid, ...},env) => ((#1 (lookupLongexcon env longid (OTHER "compile_node, Excon")), NONE),env)
		      | _ => die "compile_node: fn Excon {longid, ...} =>")
		| Tuple _ => die "compile_node: Tuple")
	     end
	 | Success rhs' => ([], compile_rhs' compile_no obj env rhs')))

  and compile_edge  compile_no obj raise_something tau_return_opt env edge
	: function list * LambdaExp =
	(case edge of
	   NONE => ([], raise_something (#1 obj))
	 | SOME node =>
	     if shared node then
	       (if ! (#visited node) then [] else
		let val (functions, lexp) =
		  compile_node compile_no obj raise_something tau_return_opt (CE.clearPathEnv env) node
		    val Type = ARROWtype ([unitType],
					  [NoSome "compile_edge" tau_return_opt])
		    val function = {lvar= #lvar node, tyvars=[], Type=Type,
				    bind=FN {pat = [(Lvars.new_named_lvar "obj",
						     unitType)],
					     body = lexp}}
	             (*tyvars=[] because an rhs can never be polymorphic: FN
		      & HANDLE exps have no polymorphism, and
		      valbinds---which do have polymorphism---will always be
		      compiled to one big exp and no functions (see
		      compile_binding).*)
		in function :: functions
		end,
	        compile_jump_to node)
	     else compile_node compile_no obj raise_something tau_return_opt env node)

  val compile_decdag = compile_edge

  (*reachable edge = the nodes that are reachable from the edge `edge'; so
   `reachable decdag' gives you the nodes of the decdag `decdag'*)

  fun reachable (decdag : edge)   : node list =
	let val nodes = reachable0 decdag
	in List.app (fn node => #visited node := false) nodes ;
	  nodes
	end

  (*invariant: visited fields of all nodes are always false except within
   calls to reachable*)

  and reachable0 NONE = []
    | reachable0 (SOME (node : node as {visited, ...})) =
	if !visited then [] else node :: visit_node node
  and visit_node node =
	(#visited node := true ;
	 (case #kind node of
	    IfEq (path, con, edge1, edge2) => reachable0 edge1 @ reachable0 edge2
	  | Success rhs' => []))

  fun exhaustive (nodes : node list)  : bool =
	(*Presumably `nodes' is the complete list of nodes in some decdag.
	 The decdag is exhaustive if there is no failures in it, i.e., if all
	 edges go to some node, i.e., have the form `SOME _'*)
	not (List.exists (fn {kind=IfEq (path, pcon, NONE, _), ...} => true
                           | {kind=IfEq (path, pcon, _, NONE), ...} => true
			   | {kind=_, ...} => false)
	        nodes)

  (*redundant_rules nodes = a list of numbers of rhs's that will never be
   evaluated.  Presumably `nodes' is the complete list of nodes in some
   decdag.  In the case of

       fn (SOME _) => true | NONE => false | SOME x => x | _ => true

   you get the list [2,3] or [3,2], because the last two mrules are
   redundant.  rev the list, if you don't like the order.*)

  fun redundant_rules (nodes : node list)   : int list =
	foldl (fn (node, is) =>
		    (case node of
		       {kind=Success (_, i), refs=ref 0, ...} => i :: is
		     | _ => is))
	  [] nodes


(*TODO 06/01/1998 11:25. tho.:

  fun string_from_decdag (edge : edge) =
	"(" ^ string_from_edge edge ^ ",\n[" ^ string_from_nodes reachable edge ^ "])\n"
  and string_from_edge NONE = "NONE"
    | string_from_edge (SOME (lvar, decision1)) =
	"SOME (" ^ Lvars.pr_lvar lvar ^ ", " ^ string_from_decison1 decision1 ^ ")"
  and string_from_nodes nodes = foldr (fn (node, s) =>
					string_from_node node ^ ",\n" ^ s) "" nodes
  and string_from_node (lvar, decision1) =
	"  (" ^ Lvars.pr_lvar lvar ^ "-- " ^ string_from_decison1 decision1 ^ ")"
  and string_from_decison1 (IfEq (path, con, t1, t2)) =
	"IfEq (" ^ string_from_path path ^ ", "
	^ string_from_con0 con ^ ", "
	^ short_string_from_edge t1 ^ ", "
	^ short_string_from_edge t2 ^ ")"
    | string_from_decison1 (Success rhs) = "Success " ^ string_from_rhs rhs
  and short_string_from_edge NONE = "NONE"
    | short_string_from_edge (SOME (lvar, _)) = "SOME (" ^ Lvars.pr_lvar lvar ^ ", ...)"

  val pr_decdag = pr o string_from_decdag
*)

  fun string_from_edge (NONE) = "NONE"
    | string_from_edge (SOME node) = "SOME(" ^ string_from_node node ^ ")"
  and string_from_kind (Success rhs') = "Success(" ^ string_from_rhs' rhs' ^ ")"
    | string_from_kind (IfEq(path,con,edge1,edge2)) =
    "IfEq{path=" ^ string_from_path path ^ ", con=" ^ string_from_con0 con ^
    ", edge1=" ^ string_from_edge edge1 ^ ", edge2=" ^ string_from_edge edge2 ^ ")"
  and string_from_node {kind, refs, visited, lvar} =
    "Node{lvar=" ^ Lvars.pr_lvar lvar ^ ", kind=" ^ string_from_kind kind ^
    ", refs=" ^ Int.toString (!refs) ^ ", visited="^ Bool.toString(!visited) ^ "}"

  local
    fun declarations_to_be_made_for_id (id : id) (info : ElabInfo.ElabInfo) (path : path) =
	  (case to_TypeInfo info of
	     SOME (TypeInfo.VAR_PAT_INFO {tyvars, Type}) =>
	       let val alphas = map compileTyVar tyvars
		   val tau = compileType Type
		   val lvar = new_lvar_from_id id
	       in
		 (id, (lvar, alphas, tau), path)
	       end
	   | _ => die "declarations_to_be_made_for_id")

    (*declared_by_pat (pat, Obj) = the list of declarations (i.e.,
     declarations_to_be_made) that pat will make.  For instance, the pat in

      `fun f (a, b as (1, c)) = e'

     declares a, b & c.*)

    fun declared_by_pat (pat : pat, path : path) =
	  (case pat of
	     ATPATpat (info, atpat) => declared_by_atpat (atpat, path)
	   | CONSpat (info, longid_op_opt, atpat) =>
	       (case to_TypeInfo info of
		  SOME (TypeInfo.CON_INFO {numCons, longid, ...}) =>
		    declared_by_application (Con {longid=longid, span=span_from_int numCons,
				    info=info,
				    (*because it appears in a CONSpat:*)nullary=false})
		      [(0, ATPATpat (DecGrammar.bogus_info, atpat))] (path)
		| SOME (TypeInfo.EXCON_INFO {longid, ...}) =>
		    declared_by_application (Excon {longid=longid,
				      (*because it appears in a CONSpat:*)nullary=false})
		      [(0, ATPATpat (DecGrammar.bogus_info, atpat))] (path)
		 | _ => die "declared_by_pat (CONSpat ...)")
	   | TYPEDpat (info, pat, ty) => declared_by_pat (pat, path)
	   | LAYEREDpat (info, OP_OPT (id, _), ty_opt, pat) =>
	       declarations_to_be_made_for_id id info path :: declared_by_pat (pat, path)
	   | UNRES_INFIXpat _ => die "declared_by_pat (UNRES_INFIXpat ...)")

    and declared_by_atpat (atpat, path) =
	  (case atpat of
	     WILDCARDatpat info => []
	   | SCONatpat (info, scon) => []
	   | LONGIDatpat (info, OP_OPT (longid, _), _) =>
	       (case to_TypeInfo info of
		  SOME (TypeInfo.VAR_PAT_INFO _) =>
		    (*it is a variable, not a nullary constructor*)
		    [declarations_to_be_made_for_id (Ident.decompose0 longid) info path]
		| SOME (TypeInfo.CON_INFO _) =>
		    (*because it appears in a LONGIDatpat, the constructor is nullary,
		     & thus has no arguments:*) []
		| SOME (TypeInfo.EXCON_INFO _) => []
		| _ => die "declared_by_atpat (LONGIDatpat ...)")
	   | RECORDatpat (info, patrow_opt) =>
	       let val patrows = (case patrow_opt of
			 NONE => []
		       | SOME patrow => makeList
			   (fn PATROW (_, _, _, patrow_opt) => patrow_opt
			     | DOTDOTDOT _ => die "declared_by_atpat: DOTDOTDOT")
			   patrow)
		 val argpats =
		       map (fn PATROW (info, _, pat, _) =>
			    (case to_TypeInfo info of
			       SOME (TypeInfo.LAB_INFO {index, ...}) => (index, pat)
			     | _ => die "declared_by_atpat: RECORDatpat info")
			     | _ => die "declared_by_atpat: RECORDatpat patrow") patrows
	       in
		 declared_by_application (Tuple {arity=List.length argpats}) argpats (path)
	       end
	   | PARatpat (info, pat) => declared_by_pat (pat, path))

    (*declared_by_application pcon argpats path = `pcon' is the compile-time
     description of the constructor that we want to generate code to match.
     `argpats' are the argument patterns to `pcon', each paired with its
     "Access number", the argument to the constructor Access that will access
     the component corresponding to that pattern.*)

    and declared_by_application pcon (argpats : (int * pat) list) path =
	  foldr (fn ((i, pat), declarations_to_be_made) =>
		      declared_by_pat (pat, Access (i, pcon, path))
		      @ declarations_to_be_made) [] argpats
  in
    fun mk_success_node (pat : pat, rhs : rhs) : node =
	  let val rhs' = (declared_by_pat (pat, Obj), rhs)
	  in
	    mk_node (Success rhs') ("rhs" ^ string_from_rhs' rhs')
	  end

    fun simple_pat (ATPATpat (_, atpat)) = simple_atpat atpat
      | simple_pat (TYPEDpat (_, pat, _)) = simple_pat pat
      | simple_pat _ = NONE
    and simple_atpat (LONGIDatpat (info, OP_OPT (longid, _), _)) =
      (case to_TypeInfo info
	 of SOME (TypeInfo.VAR_PAT_INFO _) => (*it is a variable, not a nullary constructor*)
	   SOME(Ident.decompose0 longid)
	  | _ => NONE)
      | simple_atpat (PARatpat (_, pat)) = simple_pat pat
      | simple_atpat _ = NONE

    fun is_wild_pat (ATPATpat (_, WILDCARDatpat _)) = true
      | is_wild_pat (TYPEDpat (_, pat, _)) = is_wild_pat pat
      | is_wild_pat _ = false

  end(*local*)


  fun mk_decdag (rules : rule list)   : edge =
	(reset () ;
	 let val edge = fail (Neg negset.empty, rules)
	 in
	   edge_bump edge ;
	   edge
	 end)

  (*env_from_decdag is only used by compile_binding.  The decdag will always
   be from a pattern that only has one rhs, and env_from_decdag extracts from
   the decdag the environment in which this rhs expression must be
   compiled.*)

  fun env_from_decdag decdag = (*because the pattern is from a let-binding
				there will always be exactly one Success node.
				we grab the environment from that node:*)
	(case List.filter (fn {kind=Success _, ...} => true | _ => false)
	        (reachable decdag) of
	   [{kind=Success (declarations_to_be_made, _), ...}] =>
	     env_from declarations_to_be_made
	 | _ => die "env_from_decdag: not exactly a Success")

end; (*match compiler local*)


    (* ---------------------------------------------------------------------- *)
    (*         Compilation of CCall names                                     *)
    (* ---------------------------------------------------------------------- *)

    (* The flag "tag_values" determines whether 32-bit integer
       values and 32-bit word values are implemented boxed or
       unboxed. When "tag_values" is enabled, 32-bit integers and
       32-bit words are represented boxed and the default integer type
       (int) is defined, internally, to be int31 and the default word
       type is defined to be word31. Contrary, when "tag_values" is
       disabled, 32-bit integers and 32-bit words are represented
       unboxed-untagged and the default integer type (int) is defined,
       internally, to be int32 and the default word type is defined to
       be word32.

       The function compileCName transforms 32-bit primitives into
       primitives on either boxed or unboxed representations dependent
       on the value of the "tag_values" flag. The function also
       transforms operations on integers and words into operations on
       either 32-bit representations or 31-bit representations.

       Overloading is dealt with independently, but whether 32-bit
       primitives work on boxed or unboxed representations is resolved
       here. *)

    local
	structure CNameMap = OrderFinMap(struct
					     type T = string
					     val lt : T -> T -> bool = fn a => fn b => a < b
					 end)

      (* 32-bit primitives are resolved to primitives working on either
       * boxed or unboxed representations *)
      fun resolve_32bit_prim p = (p, (p ^ "b", p ^ "ub"))

      (* primitives on integers and words are resolved to primitives working
       * on either 31-bit or 32-bit unboxed representations. *)
      fun resolve_default p = (p, (p ^ "31", p ^ "32ub"))

      val M = CNameMap.fromList
       (map resolve_32bit_prim
	["__shift_left_word32", "__shift_right_signed_word32",
	 "__shift_right_unsigned_word32", "__andb_word32", "__orb_word32",
	 "__xorb_word32", "__quot_int32", "__rem_int32", "__max_int32",	"__min_int32",
	 "__int31_to_int32", "__word31_to_word32",

	 "__plus_int32", "__plus_word32", "__minus_int32", "__minus_word32",  (* overloaded primitives *)
	 "__mul_int32", "__mul_word32", "__div_int32", "__div_word32", "__mod_int32", "__mod_word32",
	 "__less_int32", "__less_word32", "__greater_int32", "__greater_word32",
	 "__lesseq_int32", "__lesseq_word32", "__greatereq_int32", "__greatereq_word32",
	 "__neg_int32", "__abs_int32",

	 "__equal_int32", "__equal_word32"
	 ]
	@ map resolve_default
	["__quot_int", "__rem_int", "__max_int", "__min_int", "__equal_word",
	 "__shift_left_word", "__shift_right_signed_word", "__shift_right_unsigned_word",
	 "__orb_word", "__andb_word", "__xorb_word"]
	@
	[("__int_to_int32", ("__int31_to_int32b", "id")),
	 ("__int31_to_int", ("id", "__int31_to_int32ub")),
	 ("__int32_to_int", ("__int32b_to_int31", "id")),
	 ("__int32_to_word", ("__int32b_to_word31", "id")),
	 ("__int32_to_word32", ("__int32b_to_word32b", "id")),
	 ("__int32_to_int31", ("__int32b_to_int31", "__int32ub_to_int31")),
	 ("__int_to_int31", ("id", "__int32ub_to_int31")),
	 ("__word_to_word32", ("__word31_to_word32b", "id")),
	 ("__word_to_word32_X", ("__word31_to_word32b_X", "id")),
	 ("__word31_to_word", ("id", "__word31_to_word32ub")),
	 ("__word32_to_word", ("__word32b_to_word31", "id")),
	 ("__word32_to_word31", ("__word32b_to_word31", "__word32ub_to_word31")),
	 ("__word_to_word31", ("id", "__word32ub_to_word31")),
	 ("__word31_to_word_X", ("id", "__word31_to_word32ub_X")),
	 ("__word31_to_word32_X", ("__word31_to_word32b_X", "__word31_to_word32ub_X")),
	 ("__word32_to_int32", ("__word32b_to_int32b", "__word32ub_to_int32ub")),
	 ("__word32_to_int32_X", ("__word32b_to_int32b_X", "id")),
	 ("__word32_to_int", ("__word32b_to_int31", "__word32ub_to_int32ub")),
	 ("__word32_to_int_X", ("__word32b_to_int31_X", "id"))
	 ])
    in
      fun compileCName name =
	case CNameMap.lookup M name
	  of SOME (tagged, untagged) =>
	    if tag_values() then tagged
	    else untagged
	   | NONE => name
    end


    (* ---------------------------------------------------------------------- *)
    (*         Primitives for overloaded arithmetic operators                 *)
    (* ---------------------------------------------------------------------- *)

    fun ccall name argtypes restype =
      CCALLprim {name = compileCName name, instances = [], tyvars = [],
		 Type = ARROWtype (argtypes, [restype])}


    local

      fun resolve err_str i args {int31, int32, intinf, word8, word31, word32, real, string} =
	let fun no s (SOME e) = e args
	      | no s NONE = die (err_str ^ ": " ^ s)
	   (* int resolved to int31 or int32 and word resolved to
	    * word31 or word32 in ElabDec. *)
	in case NoSome err_str (ElabInfo.to_OverloadingInfo i)
	     of OverloadingInfo.RESOLVED_INT31 => no "int31" int31
	      | OverloadingInfo.RESOLVED_INT32 => no "int32" int32
	      | OverloadingInfo.RESOLVED_INTINF => no "intinf" intinf
	      | OverloadingInfo.RESOLVED_REAL => no "real" real
	      | OverloadingInfo.RESOLVED_WORD8 => no "word8" word8
	      | OverloadingInfo.RESOLVED_WORD31 => no "word31" word31
	      | OverloadingInfo.RESOLVED_WORD32 => no "word32" word32
	      | OverloadingInfo.RESOLVED_CHAR => no "char" word8
	      | OverloadingInfo.RESOLVED_STRING => no "string" string
	      | _ => die (err_str ^ ": unresolved")
	end

      fun int_or_real i args {int31, int32, intinf, real} =
	resolve "int_or_word" i args
	{int31=SOME int31, int32=SOME int32, intinf=SOME intinf, word8=NONE, word31=NONE,
	 word32=NONE, real=SOME real, string=NONE}

      fun int_or_word i args {int31, int32, intinf, word8, word31, word32} =
	resolve "int_or_word" i args
	{int31=SOME int31, int32=SOME int32, intinf=SOME intinf, word8=SOME word8, word31=SOME word31,
	 word32=SOME word32, real=NONE, string=NONE}

      fun int_or_word_or_real i args {int31, int32, intinf, word8, word31, word32, real} =
	resolve "int_or_word_or_real" i args
	{int31=SOME int31, int32=SOME int32, intinf=SOME intinf, word8=SOME word8, word31=SOME word31,
	 word32=SOME word32, real=SOME real, string=NONE}

      fun string_or_int_or_word_or_real i args {string, int31, int32, intinf, word8, word31, word32, real} =
	resolve "string_or_int_or_word_or_real" i args
	{int31=SOME int31, int32=SOME int32, intinf=SOME intinf, word8=SOME word8, word31=SOME word31,
	 word32=SOME word32, real=SOME real, string=SOME string}

      fun binary_ccall t n args =
	let val c = ccall n [t,t] t
	in PRIM(c, args)
	end

      fun binary_ccall_exn t n args =
	let val c = ccall n [t,t,exnType] t
	in PRIM(c, args)
	end

      fun unary_ccall t n args =
	let val c = ccall n [t] t
	in PRIM(c, args)
	end

      fun cmp_ccall t n args =
	let val c = ccall n [t,t] boolType
	in PRIM(c, args)
	end

      (* Operations on Words (word8, word31, word32) *)

      fun norm31 e =
	binary_ccall word31Type "__andb_word31"
	[WORD(0wxFF: Word32.word, word31Type), e]

      fun norm32 e =
	binary_ccall word32Type "__andb_word32"
	[WORD(0wxFF: Word32.word, word32Type), e]

      val plus_word31 = binary_ccall word31Type "__plus_word31"
      val plus_word32 = binary_ccall word32Type "__plus_word32"
      fun plus_word8 args = if tag_values() then norm31 (plus_word31 args)
			    else norm32 (plus_word32 args)

      val minus_word31 = binary_ccall word31Type "__minus_word31"
      val minus_word32 = binary_ccall word32Type "__minus_word32"
      fun minus_word8 args = if tag_values() then norm31 (minus_word31 args)
			     else norm32 (minus_word32 args)

      val mul_word31 = binary_ccall word31Type "__mul_word31"
      val mul_word32 = binary_ccall word32Type "__mul_word32"
      fun mul_word8 args = if tag_values() then norm31 (mul_word31 args)
			   else norm32 (mul_word32 args)

      val div_word31 = binary_ccall_exn word31Type "__div_word31"
      val div_word32 = binary_ccall_exn word32Type "__div_word32"
      fun div_word8 args = if tag_values() then div_word31 args
			   else div_word32 args

      val mod_word31 = binary_ccall_exn word31Type "__mod_word31"
      val mod_word32 = binary_ccall_exn word32Type "__mod_word32"
      fun mod_word8 args = if tag_values() then mod_word31 args
			   else mod_word32 args

      val less_word31 = cmp_ccall word31Type "__less_word31"
      val less_word32 = cmp_ccall word32Type "__less_word32"
      fun less_word8 args = if tag_values() then less_word31 args
			    else less_word32 args
      val greater_word31 = cmp_ccall word31Type "__greater_word31"
      val greater_word32 = cmp_ccall word32Type "__greater_word32"
      fun greater_word8 args = if tag_values() then greater_word31 args
			       else greater_word32 args
      val lesseq_word31 = cmp_ccall word31Type "__lesseq_word31"
      val lesseq_word32 = cmp_ccall word32Type "__lesseq_word32"
      fun lesseq_word8 args = if tag_values() then lesseq_word31 args
			      else lesseq_word32 args
      val greatereq_word31 = cmp_ccall word31Type "__greatereq_word31"
      val greatereq_word32 = cmp_ccall word32Type "__greatereq_word32"
      fun greatereq_word8 args = if tag_values() then greatereq_word31 args
				 else greatereq_word32 args

      (* Operations on Integers (int31, int32) *)
      val plus_int31 = binary_ccall int31Type "__plus_int31"
      val plus_int32 = binary_ccall int32Type "__plus_int32"
      val minus_int31 = binary_ccall int31Type "__minus_int31"
      val minus_int32 = binary_ccall int32Type "__minus_int32"
      val mul_int31 = binary_ccall int31Type "__mul_int31"
      val mul_int32 = binary_ccall int32Type "__mul_int32"
      val div_int31 = binary_ccall_exn int31Type "__div_int31"
      val div_int32 = binary_ccall_exn int32Type "__div_int32"
      val mod_int31 = binary_ccall_exn int31Type "__mod_int31"
      val mod_int32 = binary_ccall_exn int32Type "__mod_int32"
      val less_int31 = cmp_ccall int31Type "__less_int31"
      val less_int32 = cmp_ccall int32Type "__less_int32"
      val greater_int31 = cmp_ccall int31Type "__greater_int31"
      val greater_int32 = cmp_ccall int32Type "__greater_int32"
      val lesseq_int31 = cmp_ccall int31Type "__lesseq_int31"
      val lesseq_int32 = cmp_ccall int32Type "__lesseq_int32"
      val greatereq_int31 = cmp_ccall int31Type "__greatereq_int31"
      val greatereq_int32 = cmp_ccall int32Type "__greatereq_int32"

      (* Operations on Strings *)
      val less_string = cmp_ccall stringType "lessStringML"
      val greater_string = cmp_ccall stringType "greaterStringML"
      val lesseq_string = cmp_ccall stringType "lesseqStringML"
      val greatereq_string = cmp_ccall stringType "greatereqStringML"

      (* Unary Operations *)
      val abs_int31 = unary_ccall int31Type "__abs_int31"
      val abs_int32 = unary_ccall int32Type "__abs_int32"
      val abs_real = unary_ccall realType "__abs_real"
      val neg_int31 = unary_ccall int31Type "__neg_int31"
      val neg_int32 = unary_ccall int32Type "__neg_int32"
      val neg_real = unary_ccall realType "__neg_real"

      (* Real operations *)
      val plus_real = binary_ccall realType "__plus_real"
      val minus_real = binary_ccall realType "__minus_real"
      val mul_real = binary_ccall realType "__mul_real"
      val less_real = cmp_ccall realType "__less_real"
      val greater_real = cmp_ccall realType "__greater_real"
      val lesseq_real = cmp_ccall realType "__lesseq_real"
      val greatereq_real = cmp_ccall realType "__greatereq_real"

      (* IntInf operations *)
      fun intInfOp opr e args =
	  let val intInfLongId = Ident.mk_LongId ["IntInfRep",opr]
	      val arg = (case args of
			     [a] => a
			   | args => PRIM(RECORDprim,args))
	  in case CE.lookup_longid e intInfLongId of
	      SOME(CE.LVAR (lv,tvs,t,ts)) => APP(VAR{lvar=lv,instances=nil},arg,NONE)
	    | _ => die ("intinfOp: " ^ opr)
	  end

      val plus_intinf = intInfOp "+"
      val minus_intinf = intInfOp "-"
      val mul_intinf = intInfOp "*"
      val div_intinf = intInfOp "div"
      val mod_intinf = intInfOp "mod"
      val abs_intinf = intInfOp "abs"
      val neg_intinf = intInfOp "~"
      val less_intinf = intInfOp "<"
      val greater_intinf = intInfOp ">"
      val lesseq_intinf = intInfOp "<="
      val greatereq_intinf = intInfOp ">="

      fun unoverload env i p args =
	case p
	  of CE.ABS => int_or_real i args {int31=abs_int31, int32=abs_int32, intinf=abs_intinf env, real=abs_real}
	   | CE.NEG => int_or_real i args {int31=neg_int31, int32=neg_int32, intinf=neg_intinf env, real=neg_real}
	   | CE.PLUS => int_or_word_or_real i args {int31=plus_int31, int32=plus_int32, intinf=plus_intinf env,
						    word8=plus_word8, word31=plus_word31,
						    word32=plus_word32, real=plus_real}
	   | CE.MINUS => int_or_word_or_real i args {int31=minus_int31, int32=minus_int32, intinf=minus_intinf env,
						     word8=minus_word8, word31=minus_word31,
						     word32=minus_word32, real=minus_real}
	   | CE.MUL => int_or_word_or_real i args {int31=mul_int31, int32=mul_int32, intinf=mul_intinf env,
						   word8=mul_word8, word31=mul_word31,
						   word32=mul_word32, real=mul_real}
	   | CE.DIV => int_or_word i args {int31=div_int31, int32=div_int32, intinf=div_intinf env,
					   word8=div_word8, word31=div_word31, word32=div_word32}
	   | CE.MOD => int_or_word i args {int31=mod_int31, int32=mod_int32, intinf=mod_intinf env,
					   word8=mod_word8, word31=mod_word31, word32=mod_word32}
	   | CE.LESS => string_or_int_or_word_or_real i args
	    {string=less_string, int31=less_int31, int32=less_int32, intinf=less_intinf env,
	     word8=less_word8, word31=less_word31, word32=less_word32, real=less_real}
	   | CE.GREATER => string_or_int_or_word_or_real i args
	    {string=greater_string, int31=greater_int31, int32=greater_int32, intinf=greater_intinf env,
	     word8=greater_word8, word31=greater_word31, word32=greater_word32, real=greater_real}
	   | CE.LESSEQ => string_or_int_or_word_or_real i args
	    {string=lesseq_string, int31=lesseq_int31, int32=lesseq_int32, intinf=lesseq_intinf env,
	     word8=lesseq_word8, word31=lesseq_word31, word32=lesseq_word32, real=lesseq_real}
	   | CE.GREATEREQ => string_or_int_or_word_or_real i args
	    {string=greatereq_string, int31=greatereq_int31, int32=greatereq_int32, intinf=greatereq_intinf env,
	     word8=greatereq_word8, word31=greatereq_word31, word32=greatereq_word32, real=greatereq_real}
	   | _ => die "unoverload"
    in
      fun overloaded_prim env info result (*e.g., CE.ABS*)
	compilerAtexp compilerExp (arg: DecGrammar.atexp)
	takes_one_argument exn_args =
	    if takes_one_argument then
	      let val arg' = compilerAtexp arg
	      in unoverload env info result [arg']
	      end
	    else
              let val exn_args = case ElabInfo.to_OverloadingInfo info of
	                            NONE => die "overloaded_prim.no overloading info"
                                  | SOME OverloadingInfo.RESOLVED_INTINF => nil
                                  | SOME _ => exn_args
              in case arg of
		 RECORDatexp(_,
			     SOME(EXPROW(_,_,exp1,
					 SOME(EXPROW(_,_,exp2,NONE)))),
                            _) =>
		 let val exp1' = compilerExp exp1
		     val exp2' = compilerExp exp2
		 in unoverload env info result (exp1' :: exp2' :: exn_args)
		 end
	       | _ => die "overloaded_prim"
              end

      fun overloaded_prim_fn env info result (*e.g., CE.ABS*)
	takes_one_argument exn_args =
	    let
	      val ty = int_or_word_or_real info ()
		{int31=fn() => int31Type, int32=fn() => int32Type,
		 intinf=fn() => intinfType,
		 word8=wordDefaultType, word31=fn() => word31Type,
		 word32=fn() => word32Type, real=fn() => realType}
	      val exn_args = if LambdaBasics.eq_Type(LambdaExp.intinfType,ty) then nil
	                     else exn_args
	      val lvar1 = Lvars.newLvar ()
	    in
	      if takes_one_argument then
		FN {pat=[(lvar1, ty)],
		    body=unoverload env info result [VAR{lvar=lvar1, instances=[]}]}
	      else (*takes two arguments*)
		FN {pat=[(lvar1, RECORDtype [ty, ty])],
		    body=unoverload env info result
		    ([PRIM (SELECTprim 0,
			    [VAR {lvar=lvar1, instances=[]}]),
		      PRIM (SELECTprim 1,
			    [VAR {lvar=lvar1, instances=[]}])]
		     @ exn_args)}
	    end
      fun overloaded_prim_fn' env info result = (*e.g., CE.LESS, ... *)
	    let val ty = CONStype ([], string_or_int_or_word_or_real info ()
				   {string=fn()=>TyName.tyName_STRING,
				    int31=fn()=>TyName.tyName_INT31,
				    int32=fn()=>TyName.tyName_INT32,
				    intinf=fn()=>TyName.tyName_INTINF,
				    word8=TyName.tyName_WordDefault,
				    word31=fn()=>TyName.tyName_WORD31,
				    word32=fn()=>TyName.tyName_WORD32,
				    real=fn()=>TyName.tyName_REAL})
	        val lvar1 = Lvars.newLvar ()
	    in (*takes two arguments*)
	      FN {pat=[(lvar1, RECORDtype [ty, ty])],
		  body=unoverload env info result
		  [PRIM (SELECTprim 0,
			 [VAR {lvar=lvar1, instances=[]}]),
		   PRIM (SELECTprim 1,
			 [VAR {lvar=lvar1, instances=[]}])]}
	    end
    end (*local*)


    (* Compilation of equality operations *)
    local
      fun equal t n = ccall n [t,t] boolType
    in
      fun equal_int31() = equal int31Type "__equal_int31"
      fun equal_int32() = equal int32Type "__equal_int32"
      fun equal_word8() = equal (wordDefaultType()) "__equal_word"
      fun equal_word31() = equal word31Type "__equal_word31"
      fun equal_word32() = equal word32Type "__equal_word32"
    end

    (* ----------------------------------------------------------------------- *)
    (*               Syntax directed compilation                               *)
    (* ----------------------------------------------------------------------- *)

    fun typeIsIntInf (CONStype(nil,tn)) =
	TyName.eq(tn,TyName.tyName_INTINF)
      | typeIsIntInf _ = false

    fun attach_loc_info NONE = ()
      | attach_loc_info (SOME(i,r)) =
        RegVar.attach_location_report r (fn () => report_SourceInfo_in_ElabInfo i)

    fun compileAtexp env atexp : TLE.LambdaExp =
          (case atexp of
	     SCONatexp(info, SCon.INTEGER x, rv_opt) =>
	       let
(*		 val t = case NoSome "compileAtexp.SCON.INT.NONE" (ElabInfo.to_OverloadingInfo info)
			   of OverloadingInfo.RESOLVED_INT31 => int31Type
			    | OverloadingInfo.RESOLVED_INT32 => int32Type
			    | _ => die "compileAtexp.SCON.INT.unresolved"
*)
		 val t = typeScon info
	       in if typeIsIntInf t then buildIntInf x
		  else INTEGER (Int32.fromLarge (IntInf.toLarge x), t)
	       end
           | SCONatexp(_, SCon.STRING x, rv_opt) => (attach_loc_info rv_opt;
                                                     STRING (x, Option.map #2 rv_opt))
	   | SCONatexp(_, SCon.REAL x, rv_opt) => (attach_loc_info rv_opt;
                                                   REAL (x, Option.map #2 rv_opt))
	   | SCONatexp(info, SCon.CHAR x, rv_opt) =>
	       if x < 0 orelse x > 255 then die "compileAtexp.CHAR"
	       else WORD(Word32.fromInt x, typeScon info)
	   | SCONatexp(info, SCon.WORD x, rv_opt) =>
		 let
(*
		   val t = case NoSome "compileAtexp.SCON.WORD.NONE" (ElabInfo.to_OverloadingInfo info)
			     of OverloadingInfo.RESOLVED_WORD8 => wordDefaultType()
			      | OverloadingInfo.RESOLVED_WORD31 => word31Type
			      | OverloadingInfo.RESOLVED_WORD32 => word32Type
			      | _ => die "compileAtexp.SCON.WORD.unresolved"
*)
		   val t = typeScon info
		 in WORD (x, t)
		 end
	   | IDENTatexp(info, OP_OPT(longid, _), regvars_opt) =>
	       compile_ident env info longid (lookupLongid env longid (NORMAL info))

	   (* records: the fields must be evaluated in their textual order,
	    but the resulting record object must have the fields in a
	    canonical order (we adopt alphabetic ordering). Hmm. Tricky.
	    Easiest way is to bind the record field expressions to lvars and
	    then build a record of the (appropriately ordered) lvars. *)

	   (* Well, - if the labs are already sorted then we can in-line
	    the expressions in the record... 04/10/1996-Martin. *)

	   | RECORDatexp(_, SOME exprow, rv_opt) =>
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
			let val sortedLvarsXlabs =
			          ListSort.sort
				  (fn (_, l1) => fn (_, l2) => Lab.<(l1, l2))
				  (ListPair.zip(lvars, labs))
			in
			  PRIM(RECORDprim,map (fn (lv, _) => VAR{lvar=lv,instances=[]})
			       sortedLvarsXlabs)
			end
		  in
		    foldr (fn ((lv,exp,tau), exp') => monoLet((lv,tau,exp),exp'))
		    scope (zip3(lvars,exps,taus))
		  end
	     end

	   | RECORDatexp(_, NONE, _) => PRIM(RECORDprim,[])

	   | LETatexp(_, dec, exp) =>
	       let val (env1, f) = compileDec env (false,dec)
	           val exp' = compileExp (env plus env1) exp
	       in f exp'
	       end

	   | PARatexp(_, exp) => compileExp env exp)

    and compile_ident env info longid result =
          (case result of
	     CE.LVAR (lv,tyvars,_,il) =>   (*see COMPILER_ENV*)
	       (let val instances =
		      (case to_TypeInfo info of
			 SOME(TypeInfo.VAR_INFO{instances}) => instances
		       | _ => die ("compileAtexp(LVAR..): no type info for "
				   ^ Ident.pr_longid longid))
		    val instances' = map compileType instances
		    val S = mk_subst (fn () => "CompileDec.IDENTatexp") (tyvars, instances')
		    val il' = on_il(S,il)
		in VAR {lvar=lv,instances=il'}
		end handle X => (print (" Exception raised in CompileDec.IDENTatexp.LVAR.longid = "
					^ Ident.pr_longid longid ^ "\n");
		                 print " Reraising...\n"; raise X))
	   | CE.ABS =>       overloaded_prim_fn env info CE.ABS       true  []
	   | CE.NEG =>       overloaded_prim_fn env info CE.NEG       true  []
	   | CE.PLUS =>      overloaded_prim_fn env info CE.PLUS      false []
	   | CE.MINUS =>     overloaded_prim_fn env info CE.MINUS     false []
	   | CE.MUL =>       overloaded_prim_fn env info CE.MUL       false []
	   | CE.DIV =>       overloaded_prim_fn env info CE.DIV       false
		  [PRIM (EXCONprim Excon.ex_DIV, [])]
	   | CE.MOD =>       overloaded_prim_fn env info CE.MOD       false
		  [PRIM (EXCONprim Excon.ex_DIV, [])]
	   | CE.LESS =>      overloaded_prim_fn' env info CE.LESS
	   | CE.GREATER =>   overloaded_prim_fn' env info CE.GREATER
	   | CE.LESSEQ =>    overloaded_prim_fn' env info CE.LESSEQ
	   | CE.GREATEREQ => overloaded_prim_fn' env info CE.GREATEREQ
	   | CE.CON(con,tyvars,tau0,il) => (*See COMPILER_ENV*)
	       let
		 val instances =
		   case to_TypeInfo info
		     of SOME (TypeInfo.CON_INFO{instances,...}) => instances
		      | SOME (TypeInfo.VAR_INFO{instances}) => instances
		      | _ => die "compileAtexp(CON..): no type info"
		 val instances' = map compileType instances
		 val S = mk_subst (fn () => "CompileDec.CON") (tyvars, instances')
		 val tau0 = on_Type S tau0
		 val il' = on_il(S,il)
	       in case tau0
		    of ARROWtype ([tau'],_) =>
		      let val lv = Lvars.newLvar()
		      in FN{pat=[(lv,tau')],
			    body=PRIM(CONprim{con=con, instances=il'},
				      [VAR{lvar=lv,instances=[]}])}
		      end
		     | CONStype _ => PRIM(CONprim {con=con, instances=il'},[])
		     | _ => die "CE.CON.tau0 malformed"
	       end
	   | CE.REF =>
	       let val instance =
		     case to_TypeInfo info
		       of SOME (TypeInfo.CON_INFO{instances=[instance],...}) => instance
			| _ => die "compileAtexp(REF..): wrong type info"
		   val lv = Lvars.newLvar()
		   val instance' = compileType instance
	       in FN{pat=[(lv,instance')],
		     body=PRIM(REFprim {instance=instance'},
			       [VAR{lvar=lv,instances=[]}])}
	       end
	   | CE.EXCON (excon,_) =>
	       let
		 val (functional,Type) =
		   case to_TypeInfo info of
		     SOME (TypeInfo.EXCON_INFO{Type,...}) =>
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
	   | CE.RESET_REGIONS => die "compile_ident: CE.RESET_REGIONS"
	   | CE.FORCE_RESET_REGIONS => die "compile_ident: CE.FORCE_RESET_REGIONS"
	   | CE.PRIM => die "compile_ident: CE.PRIM"
	   | CE.EXPORT => die "compile_ident: CE.EXPORT")

    and compileExp env exp =
          (case exp of
	     ATEXPexp(_, atexp) => compileAtexp env atexp

	   | APPexp(_, f as ATEXPexp(_, IDENTatexp(info, OP_OPT(longid, _), regvars_opt)), arg) =>
	       compile_application_of_ident env f info longid arg

	   | APPexp(_, f, arg) => (*application of non-identifier*)
	       let val f' = compileExp env f
		 val arg' = compileAtexp env arg
	       in APP(f',arg',NONE)
	       end

	   | TYPEDexp(_, exp, _) => compileExp env exp

	   | HANDLEexp (info, exp', match) =>
	       let val e1' = compileExp env exp'
		   val tau_return = compileType (type_of_exp exp)
		   val e2' = compile_match env
		                (info,
				 match,
				 (*no inexhaustiveness warnings:*)false,
				 SOME tau_return,
				(*when no mrule matches, the raised exception
				 (obj, which is not known here)
				 must be reraised:*)
				fn obj => RAISE (obj, Types [tau_return]))
	       in HANDLE (e1',e2')
	       end

	   | RAISEexp(i, exp') =>
	       let val e' = compileExp env exp'
		   val tau' = compileType (type_of_exp exp)
	       in RAISE(e',Types [tau'])
	       end

	   | FNexp (info, match) =>
	       let val tau_return =
		         (case compileType (type_of_exp exp) of
			    ARROWtype (_, [tau_return]) => tau_return
			  | _ => die "compileExp: FNexp did not have (unary) arrow type")
	       in compile_match env (info,
				     match,
				     (*inexhaustiveness warnings, please:*)true,
				     SOME tau_return,
				    (*when no mrule matches, exception Match must
				     be raised: (abstracted over obj because
				     of HANDLEexp above)*)
				    fn obj =>
				    RAISE (PRIM (EXCONprim Excon.ex_MATCH, []),
					   Types [tau_return]))
	       end

	   | UNRES_INFIXexp _ =>  die "compileExp(UNRES_INFIX)")


    and compile_application_of_ident env f info longid arg =

          (* We have to spot direct application of "prim" - apart from that,
	   we don't have to bother with constructors and the like. They'll
	   compile to functions, but the optimiser will spot them later. *)

          (case lookupLongid env longid (NORMAL info) of
	     CE.LVAR (lv,tyvars,_,il) =>        (* Not a primitive... *)
		let val arg' = compileAtexp env arg
		    val instances' = case to_TypeInfo info
				       of SOME(TypeInfo.VAR_INFO{instances}) =>
					 map compileType instances
					| _ => die "compileExp(APPexp..): wrong type info"
		    val S = (mk_subst (fn () => ("CompileDec.APPexp.LVAR("
						 ^ Lvars.pr_lvar lv ^ ")"))
			     (tyvars,instances')) handle ex =>
		      (lookup_error("lvar", env, longid, NORMAL info);
		       raise ex)

		    val il' = on_il(S, il)
		    fun default () = APP(VAR{lvar=lv,instances=il'},arg',NONE)
		in
		  if Lvars.pr_lvar lv = "=" then (* specialise equality on integers *)
		    case (instances', arg')
		      of ([CONStype([], tyname)], PRIM(RECORDprim, l)) =>
			(if TyName.eq(tyname, TyName.tyName_INT31) then
			   PRIM(equal_int31(), l)
			 else if TyName.eq(tyname, TyName.tyName_INT32) then
			   PRIM(equal_int32(), l)
			 else if TyName.eq(tyname, TyName.tyName_WORD8) then
			   PRIM(equal_word8(), l)
			 else if TyName.eq(tyname, TyName.tyName_WORD31) then
			   PRIM(equal_word31(), l)
			 else if TyName.eq(tyname, TyName.tyName_WORD32) then
			   PRIM(equal_word32(), l)
			 else default())
		       | _ => default()
		  else default()
		end

	    | CE.RESET_REGIONS =>
		let val arg' = compileAtexp env arg
		    val tau' = case to_TypeInfo info
				 of SOME(TypeInfo.VAR_INFO{instances = [tau]}) =>
				   compileType tau
				  | _ => die "compileExp(APPexp..): wrong type info"
		in PRIM(RESET_REGIONSprim{instance = tau'}, [arg'])
		end

	    | CE.FORCE_RESET_REGIONS =>
		let val arg' = compileAtexp env arg
		    val tau' = case to_TypeInfo info
				 of SOME(TypeInfo.VAR_INFO{instances = [tau]}) =>
				   compileType tau
				  | _ => die "compileExp(APPexp..): wrong type info"
		in PRIM(FORCE_RESET_REGIONSprim{instance = tau'}, [arg'])
		end

	    | CE.ABS =>       overloaded_prim env info CE.ABS       (compileAtexp env) (compileExp env) arg true  []
	    | CE.NEG =>       overloaded_prim env info CE.NEG       (compileAtexp env) (compileExp env) arg true  []
	    | CE.PLUS =>      overloaded_prim env info CE.PLUS      (compileAtexp env) (compileExp env) arg false []
	    | CE.MINUS =>     overloaded_prim env info CE.MINUS     (compileAtexp env) (compileExp env) arg false []
	    | CE.MUL =>       overloaded_prim env info CE.MUL       (compileAtexp env) (compileExp env) arg false []
	    | CE.DIV =>       overloaded_prim env info CE.DIV       (compileAtexp env) (compileExp env) arg false
		[PRIM (EXCONprim Excon.ex_DIV, [])]
	    | CE.MOD =>       overloaded_prim env info CE.MOD       (compileAtexp env) (compileExp env) arg false
		[PRIM (EXCONprim Excon.ex_DIV, [])]
	    | CE.LESS =>      overloaded_prim env info CE.LESS      (compileAtexp env) (compileExp env) arg false []
	    | CE.GREATER =>   overloaded_prim env info CE.GREATER   (compileAtexp env) (compileExp env) arg false []
	    | CE.LESSEQ =>    overloaded_prim env info CE.LESSEQ    (compileAtexp env) (compileExp env) arg false []
	    | CE.GREATEREQ => overloaded_prim env info CE.GREATEREQ (compileAtexp env) (compileExp env) arg false []
	    | CE.PRIM => compile_application_of_prim env info arg
	    | CE.EXPORT =>
		let val (name,args) = decompose_prim_call arg
		    val (tau_arg,tau_res) = (* The type (tau_arg->tau_res) is the instance of ('a->'b) *)
			case to_TypeInfo info of
			    SOME (TypeInfo.VAR_INFO {instances = [tau_arg, tau_res]}) =>
				(compileType tau_arg, compileType tau_res)
			  | _ => die ("Wrong type info for exported function " ^ name)
		    val _ = if List.length args = 1 then ()
			    else die ("Wrong number of arguments to the function _export for \
				      \the exported function " ^ name)
		in TLE.PRIM (EXPORTprim {name=name,instance_arg=tau_arg,
					 instance_res=tau_res},
			     map (compileExp env) args)
		end

	    | CE.CON(con,tyvars,tau0,il) => (*See COMPILER_ENV*)
	       let
		 val instances =
		   case to_TypeInfo info
		     of SOME (TypeInfo.CON_INFO{instances,...}) => instances
		      | SOME (TypeInfo.VAR_INFO{instances}) => instances
		      | _ => die "compileAtexp(CON..): no type info"
		 val instances' = map compileType instances
		 val S = mk_subst (fn () => "CompileDec.CON") (tyvars, instances')
		 val il' = on_il(S,il)
	       in PRIM(CONprim{con=con, instances=il'},
		       [compileAtexp env arg])
	       end
	    | CE.REF =>
	       let val instance =
		     case to_TypeInfo info
		       of SOME (TypeInfo.CON_INFO{instances=[instance],...}) => instance
			| _ => die "compileAtexp(REF..): wrong type info"
		   val instance' = compileType instance
	       in PRIM(REFprim {instance=instance'},
		       [compileAtexp env arg])
	       end
	    | CE.EXCON (excon,_) => PRIM(EXCONprim excon, [compileAtexp env arg])
(*
	    | _ => let val f' = compileExp env f
		       val arg' = compileAtexp env arg
		   in APP(f',arg')
		   end
*)
            ) (*fun compile_application_of_ident*)


    and compile_application_of_prim env info atexp =

          (*Application of `prim' to atexp.  We disassemble atexp to get the
	   name s of the primitive operation and its arguments.*)

          (let val (s, args) = decompose_prim_call atexp
	       fun f isequal prim =
		     let val args' = map (compileExp env) args
		         val instance' = (case to_TypeInfo info of
			       SOME (TypeInfo.VAR_INFO {instances = [instance, instanceRes]}) =>
				 compileType instance
			     | _ => die "compileExp(APPexp(PRIM..): wrong type info")
		        (*The code right above depends on the order of the
			 instances recorded during elaboration.  We need the
			 instance corresponding to the argument of prim,
			 which in EfficientCoreElab version is the second*)

                        (* Specialice EQUALprim to EQUAL_INTprim, when possible *)
                        val prim' = case (isequal,instance') of
                                      (true,CONStype([], tyname)) =>
                                        if TyName.eq(tyname,TyName.tyName_INT31) then equal_int31()
					else if TyName.eq(tyname,TyName.tyName_INT32) then equal_int32()
					else if TyName.eq(tyname,TyName.tyName_WORD8) then equal_word8()
					else if TyName.eq(tyname,TyName.tyName_WORD31) then equal_word31()
					else if TyName.eq(tyname,TyName.tyName_WORD32) then equal_word32()
                                        else prim {instance=instance'}
                                    | _ => prim {instance=instance'}
		     in TLE.PRIM (prim', args')
		     end
(*TODO 12/11/1997 18:37. tho.:
the 12 lines above are very similar to the code below
*)
	   in
	     (case s of
		"!" => f false DEREFprim (*ref (REFprim) is a constructor, so it does not show up here*)
	      | ":=" => f false ASSIGNprim
	      | "=" => f true EQUALprim

(*KILL 20/11/1997 15:38. tho.:
 it is wrong to remove "id" here; now it is done in CompLamb.
	      | "id" =>
		  (*type conversions that result in no code to run at
		   run-time are declared as prim "id"'s; for instance, ord is
		   defined: `fun ord (c : char) : int = prim ("id", "id", c)'.*)
		  (case args of
		     [exp] => compileExp env exp
		   | _ => die "compile_application_of_prim: prim id")
*)
	      | _ =>
		  (*unrecognised prim name: this must be a c call; let us
		   hope the run-time system defines a function called s:*)
		  compile_application_of_c_function env info s args)
	   end) (*fun compile_application_of_prim*)

    and compile_application_of_c_function env info name args =
          (case to_TypeInfo info of
	     SOME (TypeInfo.VAR_INFO {instances = [tau_argument, tau_result]}) =>

	       (*Concerning instance lists on c calls:  The built-in id
		`prim' has type scheme \/ 'a, 'b . string * string * 'a -> 'b .
		In ElabDec it was instantiated, and the list instances above
		is the types to which 'a and 'b were instantiated: 'b was
		instantiated to tau_result, 'a to tau_argument (yes, it is a
		bit messy).  Thus the type of the c function is
		tau_argument->tau_result.  From CompileDec and on, we want
		the instance list to be the instance list of the c function
		type rather than the instance list of the type of `prim':*)
	       let val taus1 =
		     (case compileType tau_argument of tau1 =>
			(case List.length args of
			   1 => [tau1]
			 | n => (case tau1 of
				   TLE.RECORDtype taus1 =>
				     if List.length taus1 = n then taus1
				     else die ("prim " ^ name ^ " has wrong number of arguments")
				 | _ => die ("give prim " ^ name ^ " a record argument type"))))
		   val tau2 = compileType tau_result
		   val tau = TLE.ARROWtype (taus1, [tau2])
		   val tyvars = EqSet.list (LambdaExp.tyvars tau)
		   val tyvars_fresh = map (fn tyvar => LambdaExp.fresh_tyvar ()) tyvars
		   val subst = mk_subst
		                 (fn () => "CompileDec.compile_application_of_c_function")
		                    (tyvars, map TLE.TYVARtype tyvars_fresh)

		   (* Names for certain primitives are altered on the basis of
		    * whether tagging of integers is enabled; see the comment
		    * near the compileCName function above. *)
		   val name = compileCName name
	       in
		 TLE.PRIM (CCALLprim {name = name,
				      tyvars = tyvars_fresh,
				      Type = on_Type subst tau,
				      instances = map TLE.TYVARtype tyvars},
		           map (compileExp env) args)
	       end
	   | _ => die "compile_application_of_prim: wrong type info in ccall")


    (*flatten_c_function_type ([t1 * t2] -> [t3]) = [t1, t2] -> [t3]*)
    and flatten_c_function_type (TLE.ARROWtype ([TLE.RECORDtype taus1], taus2)) =
          TLE.ARROWtype (taus1, taus2)
      | flatten_c_function_type (TLE.ARROWtype (taus1, taus2)) =
	  TLE.ARROWtype (taus1, taus2)
      | flatten_c_function_type _ = die "flatten_c_function_type: not arrow"

(*
    (*decompose_prim_call atexp = the name (string) of the called prim & the
     argument exps.  atexp is the atexp after `prim'.  A prim call has the
     form `prim ("f", "fprof", (e1, e2))' assuming the name of the function in
     the runtime system to call is f (and fprof, when profiling is turned
     on), & that f (and fprof) takes two arguments.*)

    and decompose_prim_call
          (RECORDatexp (_, SOME (EXPROW (_, _,
           ATEXPexp (_, SCONatexp (_, SCon.STRING s1)), SOME (EXPROW (_, _,
           ATEXPexp (_, SCONatexp (_, SCon.STRING s2)), SOME (EXPROW (_, _,
           exp3, NONE)))))))) =
	  ((*if !region_profiling then s2 else*) s1, decompose_prim_args exp3)
      | decompose_prim_call _ =
	  die ("\n\nRemember to give two function names in quotes in the declaration of \
	       \a prim.\nMaybe you forgot the profiling function name.")
*)
    and decompose_prim_call
      (RECORDatexp (_, SOME (EXPROW (_, _,
        ATEXPexp (_, SCONatexp (_, SCon.STRING name, NONE)), SOME (EXPROW (_, _,
         exp2, NONE)))), _)) = (name, decompose_prim_args exp2)
      | decompose_prim_call _ =
      die ("\n\nThe first argument to prim must be a string denoting a C function\n\
       \or built-in primitive; in case profiling is enabled, the string \"Prof\" is\n\
       \appended to the name by the compiler.")

    and decompose_prim_args (ATEXPexp (_, RECORDatexp (_, exprow_opt, _))) =
          decompose_prim_args0 exprow_opt
      | decompose_prim_args exp = [exp]
    and decompose_prim_args0 NONE = []
      | decompose_prim_args0 (SOME (EXPROW (_, _, e1, exprow_opt))) =
          e1 :: decompose_prim_args0 exprow_opt


    (*compile_match = compile a match into a FN expression; this is used for
     FNexp & HANDLEexp expressions.  `raise_something' indicates what to
     plant for non-matches; it can be either a lexp that raises exception
     Match or reraises the raised exception.  `warn_on_inexhaustiveness' is
     true, e.g., for case statement on excons, but false for the equivalent
     in a handler.  The `info' is for pretty warnings.  Well, it does not
     really make them prettier.*)

    and compile_match env (info, match, warn_on_inexhaustiveness,
			   tau_return_opt, raise_something) =
      let
	val matches = makeList (fn MATCH(_, _, m) => m) match
	val pats = map (fn MATCH(_, MRULE(_, pat, _), _) => pat) matches
	val exps = map (fn MATCH(_, MRULE(_, _, exp), _) => exp) matches
	val tab = List.tabulate (List.length pats, fn i => i)
	val pat_rhs_s = BasisCompat.ListPair.zipEq (pats, tab)
                 handle BasisCompat.ListPair.UnequalLengths => die "compile_match: zip"
	val rules = map (fn (pat, rhs) => (pat, mk_success_node (pat, rhs)))
	               pat_rhs_s
	val decdag = mk_decdag rules
(*
	val _ = print "DECDAG:\n"
	val _ = print (string_from_edge decdag)
	val _ = print "\n"
*)
	val lvar_switch = new_lvar_from_pats pats
	val obj = VAR {lvar=lvar_switch, instances=[]}
	      (*instances=[] because the argument to a fn cannot be polymorphic*)
	val tau_argument = compileType (domType (type_of_match match))
	val compile_no = fn (i, env_rhs) =>
	                 (compileExp (CE.clearPathEnv env_rhs) (List.nth (exps,i)
					      handle _ => die "compile_match: nth"))

	(* This function has the effect of warning many times,
	 * so we check the decdag instead; mael 2003-12-10 ...
	fun perhapsInsertRaiseMatch a =
	    let val _ =
		if warn_on_inexhaustiveness then
		    Flags.warn (report_SourceInfo info // line "Match not exhaustive.")
		else ()
	    in raise_something a
	    end *)

	val _ =
	    if warn_on_inexhaustiveness andalso
		let val r = reachable decdag
		    val e = exhaustive r
		in not e
		end then
		Flags.warn (report_SourceInfo info // line "Match not exhaustive.")
	    else ()

	val (functions, lexp) =
	    compile_decdag compile_no (obj,tau_argument) raise_something tau_return_opt env decdag

	val _ =
	    List.app (fn i (*number of rhs which is redundant*) =>
		      (case (List.nth (matches, i) handle _ => die "compile_match: 2nd nth") of
			   MATCH (_, MRULE (info, _, _), _) =>
			       Flags.warn (report_SourceInfo info
					   // line "That rule is redundant.")))
	    (redundant_rules (map #2 rules))
      in
	  FN {pat = [(lvar_switch, tau_argument)],
	      body = FIX {functions = functions, scope = lexp}}
      end


   (*compileDec - takes an enclosing environment and a declaration, and
    returns the environment *for this declaration only*, together with a
    function to apply to the declaration's scope to return the entire lambda
    term.  The `topLevel' parameter is only needed because the match compiler
    is expected to report non-binding patterns for non top-level val bindings
    only.  (I think it is always true, right now... and probably will be
    forever.)*)

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
	     let val env1 =
	            case to_TypeInfo i
		      of SOME(TypeInfo.TYENV_INFO TyEnv) =>
			(case TE.lookup TyEnv tycon    (* A datatype replication may or may not
							* introduce an empty VE component. *)
			   of SOME tystr =>
			     if VE.is_empty (TyStr.to_VE tystr) then
			       let val tns = (TyName.Set.list o TyName.Set.map compileTyName) (TyStr.tynames tystr)
			       in CE.declare_tycon(tycon,(tns,CE.emptyCEnv),CE.emptyCEnv)
			       end
			     else  (* if the VE is non-empty then we can look it up in env *)
			       let val (tynames, env_ve) = CE.lookup_longtycon env longtycon
			  	   val env_te = CE.declare_tycon(tycon, (tynames, env_ve), CE.emptyCEnv)
			       in env_ve plus env_te
			       end
			    | NONE => die "DATATYPE_REPLICATIONdec: tycon not in env")
		       | _ => die "DATATYPE_REPLICATIONdec: No TyEnv type info"
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
		 val env' = foldl (fn (env, env') => env' plus env) CE.emptyCEnv envs
	     in (env', fn x => x)
	     end

         | SEQdec(_, dec1, dec2) =>
             let val (env, env', f1) = compileDecs env (topLevel,dec)
	     in (env, f1)
	     end
(*
         | SEQdec(_, dec1, dec2) =>
             let val (env1, f1) = compileDec env (topLevel,dec1)
	         val (env2, f2) = compileDec (env plus env1) (topLevel,dec2)
	     in (env1 plus env2, f1 o f2)
	     end
*)

        (* INFIX/NONFIX declarations have no effect on execution. *)

         | INFIXdec _ => (CE.emptyCEnv, fn x => x)
         | INFIXRdec _ => (CE.emptyCEnv, fn x => x)
         | NONFIXdec _ => (CE.emptyCEnv, fn x => x)

         | EMPTYdec _ => (CE.emptyCEnv, fn x => x)

         | REGIONdec (_,(i,regvars)) =>
           let val () = List.app (fn r => RegVar.attach_location_report r
                                          (fn () => report_SourceInfo_in_ElabInfo i)) regvars
           in (CE.emptyCEnv, fn x => LETREGION{regvars=regvars,scope=x})
           end

   and compileDecs env (topLevel,dec) = (* fast compilation when SEQ associates to the left *)
     (case dec of
        SEQdec(_, dec1, dec2) =>
             let val (env1, env1', f1) = compileDecs env (topLevel,dec1)
                            (* env1' = env plus env1 *)
	         val (env2, f2) = compileDec env1' (topLevel,dec2)
	     in (env1 plus env2, env1' plus env2, f1 o f2)
	     end

      | dec1 =>
             let
               val (env1, f1) = compileDec env (topLevel,dec1)
             in
               (env1, env plus env1, f1)
             end
    )
   (* compileValbind - although there may be `rec' prefixes nested anywhere
      in the valbind, the effect is a single non-recursive layer of
      binding, together with a second layer of several distinct recursive
      valbinds. *)

    and compileValbind env (topLevel, valbind) : CE.CEnv * (LambdaExp -> LambdaExp) =
      let
        fun flattenRecValbind vb: (pat * exp) list =
          case vb
	    of PLAINvalbind(_, pat, exp, vbOpt) =>
	      (pat,exp) :: (case vbOpt
			      of SOME vb => flattenRecValbind vb
			       | NONE => nil)
	     | RECvalbind(_, vb) => flattenRecValbind vb
      in
        case valbind
          of PLAINvalbind(i, pat, exp, vbOpt) =>
            (case to_TypeInfo i
	       of SOME (TypeInfo.PLAINvalbind_INFO{tyvars,Type,...}) =>
		 let
(*		   (* omit tyvars that are not in Type *)
		   val tyvarsType = Type.tyvars Type
		   val tyvars = List.filter (fn tv => List.exists (fn tv' => TyVar.eq(tv,tv')) tyvarsType) tyvars
*)
		   val (env1, f1) = compile_binding env (topLevel, pat, exp, (tyvars, Type))
		 in case vbOpt
		      of SOME vb =>
			let val (envRest, f2) = compileValbind env (topLevel,vb)
			in (env1 plus envRest, f1 o f2)
			end
		       | NONE => (env1, f1)
		 end
		| _ => die "compileValbind: no type info")

           | RECvalbind(_, vb) =>
               let val pairs = flattenRecValbind vb
               in compileREC env pairs
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
      case to_TypeInfo i
	of SOME(TypeInfo.TYENV_INFO TyEnv) =>
	  TE.Fold (fn (tycon,tystr) => fn (env_ve, env_te, dats) =>
	       if VE.is_empty (TyStr.to_VE tystr) then die "compileDatbind"
	       else let val tyname = (NoSome "TypeFcn not simple" o
				      TypeFcn.to_TyName o TyStr.to_theta) tystr
			val VE = TyStr.to_VE tystr
			val (env_ve', tyvars, cbs) = compile'TyStr' (tyname, VE)
			val env_te' = CE.declare_tycon(tycon, ([tyname], env_ve'), env_te)
		    in (env_ve plus env_ve', env_te', (tyvars,tyname,cbs)::dats)
		    end) (CE.emptyCEnv,CE.emptyCEnv,[]) TyEnv
	 | _ => die "No TyEnv type info for compiling datbind"

    and compileTypbind i : CE.CEnv =
      case to_TypeInfo i
	of SOME(TypeInfo.TYENV_INFO TyEnv) =>
	  TE.Fold (fn (tycon, tystr) => fn env' =>
		   if VE.is_empty (TyStr.to_VE tystr) then
		     let val tns = TyStr.tynames tystr
			 val tns = TyName.Set.map compileTyName tns
		         val tns = TyName.Set.list tns
		     in CE.declare_tycon(tycon,(tns,CE.emptyCEnv),env')
		     end
		   else die "compileTypbind: expecting VE to be empty") CE.emptyCEnv TyEnv
	| _ => die "compileTypbind: No TyEnv type info"

    and compileExbind (env:CE.CEnv) exbind : (CE.CEnv * (LambdaExp -> LambdaExp)) =
      case exbind
        of EXBIND(i, OP_OPT(excon, _), _, rest) =>
             let val tyOpt = case to_TypeInfo i
			       of SOME(TypeInfo.EXBIND_INFO {TypeOpt}) => TypeOpt
				| _ => die "No typeOpt info for compiling exbind"
		 val (env1, f1) = compileNewExn env excon tyOpt
		 val (envRest, f2) = case rest
				       of SOME exbind' => compileExbind env exbind'
					| NONE => (CE.emptyCEnv, fn x => x)
	     in (env1 plus envRest, f1 o f2)
             end

         | EXEQUAL(info, OP_OPT(excon1, _), OP_OPT(longexcon2, _), rest) =>
             let val (excon2', tau) = lookupLongexcon env longexcon2 (NORMAL info)
                 val env1 = declareExcon(excon1, (excon2', tau), CE.emptyCEnv)
		 val (envRest, f2) = case rest
				       of SOME exbind' => compileExbind env exbind'
					| NONE => (CE.emptyCEnv, fn x => x)
	     in (env1 plus envRest, f2) (*no new code*)
             end

    and compileNewExn env excon tyOpt =
      let val excon' = compileExCon excon
          val LambdaTypeOpt =
	    case tyOpt
	      of NONE => NONE
	       | SOME tau => SOME (compileType tau)
	  val tau = case LambdaTypeOpt
		      of SOME tau => TLE.ARROWtype([tau],[TLE.exnType])
		       | NONE => TLE.exnType
	  val env1 = declareExcon(excon,(excon',tau),CE.emptyCEnv)
	  val f1 = fn x => EXCEPTION(excon',LambdaTypeOpt, x)
      in (env1,f1)
      end

   (*compile_binding: we are compiling something like

        `let val pat = exp in scope end'.

    compile_binding returns the environment established by `pat', and a
    function: LambdaExp->LambdaExp, which is applied to the `scope'
    expression to yield the entire let expression. This generalises to
    parallel (`and'-linked) bindings. As long as we get the environment
    handling right (and don't make the thing `rec' by mistake), we can
    finally just cascade together the result functions returned by
    compile_binding.  sigma is the type scheme of the exp, recorded during
    elaboration.*)

  (*When the pattern is from a valbind, i.e., something like

     `val <pat> = <exp>',

   we cannot generate a FIX with functions as we do when the pattern is from
   a `fn <pat> => ...' or a `... handle <pat>'.  In the latter cases we
   generate a FIX with functions to implement the finite automaton that
   matches a value against the pattern.  This means that the scope of the
   pattern ends up as a FIX-bound function body, and this will not do when
   the scope may be a FRAME, which the scope of a valbind may be.  Sorry
   about this messy explanation.

   No, it is better to explain this by giving examples of the code generated
   in the two situations.  TODO

   compile_binding maybe ought to, but does not, use the `topLevel'
   argument.

   We treat the case where the pattern is a simple variable as a
   special case; that is, in case we are compiling a binding of the
   form `vid = <exp>' (where vid does not have constructor status)
   then we do not call upon the pattern matcher. Instead, we construct
   the binding manually. ME 1998-08-23

   *)

    and compile_binding env (topLevel, pat, exp, (tyvars, Type))
        : CE.CEnv * (LambdaExp -> LambdaExp) =
      if is_wild_pat pat then
	let val bind = compileExp env exp
	    val f = fn scope => LET {pat = nil,	bind = PRIM(DROPprim,[bind]), scope = scope}
	in (CE.emptyCEnv, f)
	end
      else
      (case simple_pat pat
	 of SOME vid =>
	   let val lvar = Lvars.new_named_lvar (Ident.pr_id vid)
	       val (tyvars', tau') = compileTypeScheme (tyvars, Type)
		 handle ? => (print ("compile_binding.SOME: lvar = " ^ Lvars.pr_lvar lvar ^ "\n"); raise ?)
	       val env' = CE.declareVar (vid, (lvar, tyvars', tau'), CE.emptyCEnv)
	       val bind = compileExp env exp
	       val f = fn scope => LET {pat = [(lvar, tyvars', tau')],
					bind = bind, scope = scope}
	   in (env', f)
	   end
	  | NONE =>
	   let
	     val decdag = mk_decdag [(pat, mk_success_node (pat, 0))]
	     val f = fn scope =>
	       let val lvar_switch = new_lvar_from_pat pat
		   val (tyvars', tau') = compileTypeScheme (tyvars, Type)
		     handle ? => (print ("compile_binding.NONE: lvar = " ^ Lvars.pr_lvar lvar_switch ^ "\n"); raise ?)
		   val obj = VAR {lvar=lvar_switch, instances=map TYVARtype tyvars'}
		   fun compile_no (i, env_rhs) = scope
		   val raise_something = fn obj : LambdaExp =>
		     RAISE (PRIM (EXCONprim Excon.ex_BIND, []), LambdaExp.RaisedExnBind)
	       in
		 case compile_decdag  compile_no (obj,tau') raise_something NONE env decdag
		   of ([], lexp) => LET {pat = [(lvar_switch, tyvars', tau')],
					 bind = compileExp env exp,
					 scope = lexp}
		    | _ =>
		     die "compile_binding: there should only be functions when there are\n\
		      \shared nodes in the decdag, i.e., nodes that are jumped to by\n\
		      \more than one node, and there can be no shared nodes in a decdag\n\
		      \from a valbind, because a valbind only has one rule (one pattern):\n\
		      \all jumps will either be to a fail node, the single success node,\n\
		      \or to the ``next'' ifeq node."
	       end
	     val env_rhs = env_from_decdag decdag
           in
	     if exhaustive (reachable decdag) then () else
	       Flags.warn
	       (report_SourceInfo (DecGrammar.get_info_pat pat)
		// line "Pattern not exhaustive.") ;
	       (env_rhs, f)
	   end
	 )

   (* compileREC - compile a list of `rec' pattern/expression pairs. The
      patterns must all be variables (and aren't even allowed to be
      bracketted??), and the RHS's must all be lambdas. Type constraints
      are allowed, though. Returns the rec env only, plus `fn scope -> lexp'. *)

    and compileREC (env:CE.CEnv) (pat_exp__s : (pat * exp) list) : CE.CEnv * (LambdaExp -> LambdaExp) =
      let
        (* Actually, to support layered patterns in the bindings, multiple
	 * identifiers may map to the same lambda variable. Type information
	 * is recorded on both the LONGIDatpat and LAYEREDpat nodes. *)

	fun mk_env idss_lvars_sigmas =
	  foldr (fn ((ids,lv,(tvs,tau)), env) =>
		 foldr (fn (id,env) => CE.declareVar(id,(lv,tvs,tau),env)) env ids)
	  CE.emptyCEnv idss_lvars_sigmas

	fun add_scheme (_, scheme as SOME _) = scheme
	  | add_scheme (i, _) =
	  case to_TypeInfo i
	    of SOME(TypeInfo.VAR_PAT_INFO{tyvars,Type}) => SOME(compileTypeScheme(tyvars,Type))
	     | SOME _ => die "compileREC.add_scheme.wrong type info"
	     | NONE => die "compileREC.add_scheme.no type info"

	fun ids_pat (TYPEDpat(_, pat, _), scheme, ids) = ids_pat (pat, scheme, ids)
	  | ids_pat (ATPATpat(_, atpat), scheme, ids) = ids_atpat (atpat, scheme, ids)
	  | ids_pat (LAYEREDpat(i, OP_OPT (id,_), _, pat), scheme, ids) =
	  ids_pat (pat, add_scheme(i,scheme), id::ids)
	  | ids_pat (UNRES_INFIXpat _, _, _) = die "ids_pat.UNRES_INFIXpat"
	  | ids_pat (CONSpat _, _, _) = die "ids_pat.CONSpat"
	and ids_atpat (WILDCARDatpat _, scheme, ids) = (scheme, ids)
	  | ids_atpat (SCONatpat _, _, _) = die "ids_atpat.SCONatpat"
	  | ids_atpat (LONGIDatpat(i,OP_OPT(longid, _),_), scheme, ids) =
	  (case Ident.decompose longid
	     of (nil, id) => (add_scheme(i,scheme), id::ids)
	      | _ => die("compileREC.ids_atpat.LONGIDatpat.long: " ^ Ident.pr_longid longid ^ ")"))
	  | ids_atpat (RECORDatpat _, _, _) = die "compileREC.ids_atpat.RECORDatpat"
	  | ids_atpat (PARatpat(_,pat), scheme, ids) = ids_pat(pat, scheme, ids)

	val ids_lv_sch_exp__s =
	  foldr (fn ((pat,exp),acc) =>
		 case ids_pat (pat, NONE, nil)
		   of (NONE, nil) => acc (* only wildcard involved; discard binding *)
		    | (SOME _, nil) => die "compileREC.ids_sch_exp__s.scheme but no ids"
		    | (SOME sch, ids as id :: _) =>
		     let val lv = Lvars.new_named_lvar(Ident.pr_id id)
		     in (ids, lv, sch, exp) :: acc
		     end
		    | _ => die "compileREC.ids_sch_exp__s.no scheme but ids")
	  nil pat_exp__s

	val ids_lv_sch__s = map (fn (ids,lv,sch,_) => (ids,lv,sch)) ids_lv_sch_exp__s
	val ids_lv_ty__s = map (fn (ids,lv,(tvs,ty)) => (ids,lv,([],ty))) ids_lv_sch__s

	val recEnv : CE.CEnv = env plus (mk_env ids_lv_ty__s)
	val scopeEnv : CE.CEnv = mk_env ids_lv_sch__s

	val functions =
	  map (fn (_,lv,(tvs,ty),exp) => {lvar=lv,tyvars=tvs,Type=ty,bind=compileExp recEnv exp})
	  ids_lv_sch_exp__s
(*
	fun id_sigma(TYPEDpat(_, pat, _)) = id_sigma pat
          | id_sigma(ATPATpat(_, LONGIDatpat(info, OP_OPT(longid, _)))) =
              (case Ident.decompose longid
                 of (nil, id) =>
		   let val sigma = case to_TypeInfo info
				     of SOME(TypeInfo.VAR_PAT_INFO{tyvars,Type}) => compileTypeScheme(tyvars,Type)
				      | SOME _ => die "compileREC.id_sigma.wrong type info"
				      | NONE => die "compileREC.no type info"
		   in (id, sigma)
		   end
                  | _ => die("compileREC.id_sigma(long: " ^ Ident.pr_longid longid ^ ")"))
          | id_sigma _ = die "compileREC.id_sigma"

	val ids_sigmas = map id_sigma pats
	val ids_lvars_sigmas = map (fn (id, sigma) => (id, Lvars.new_named_lvar(Ident.pr_id id), sigma)) ids_sigmas
	val ids_lvars_types = map (fn (id,lv,(_,tau)) => (id,lv,([],tau))) ids_lvars_sigmas
	val recEnv: CE.CEnv = mk_env ids_lvars_types
	val scopeEnv: CE.CEnv = mk_env ids_lvars_sigmas

	val binds = map (compileExp (env plus recEnv)) exps
	val functions =
	  (map (fn ((_,lvar,(tyvars,Type)),bind) => {lvar=lvar, tyvars=tyvars, Type=Type, bind=bind})
	   (BasisCompat.ListPair.zipEq (ids_lvars_sigmas,binds)))
	  handle BasisCompat.ListPair.UnequalLengths => die "compileREC.functions.Zip"
*)
	val f' = fn scope => FIX {functions=functions, scope=scope}
      in (scopeEnv : CE.CEnv, f' : LambdaExp -> LambdaExp)
      end

  (* -----------------------------------------------------
   * Modules compilation
   * ----------------------------------------------------- *)

  infix footnote
  fun x footnote y = x

  fun on_ElabInfo phi i =
      case to_TypeInfo i of
	  SOME i' => ElabInfo.plus_TypeInfo i (ElabInfo.TypeInfo.on_TypeInfo(phi,i'))
	| NONE => i

  local
    open TopdecGrammar

    fun comp_strexp(fe,ce,strexp) =
      case strexp
	of STRUCTstrexp(info,strdec) => comp_strdec(fe,ce,strdec)
	 | LONGSTRIDstrexp(info,longstrid) => (lookup_longstrid ce longstrid, fn x => x)
	 | TRANSPARENT_CONSTRAINTstrexp(info, strexp, _) =>
	  let val (ce1,f) = comp_strexp(fe,ce,strexp)
	      val E = case to_TypeInfo info
			of SOME (ElabInfo.TypeInfo.TRANS_CONSTRAINT_INFO E) => E
			 | _ => die "comp_strexp.TRANSPARENT_CONSTRAINTstrexp.no env info"
	      val ce2 = CE.constrain(ce1,E)
	  in (ce2, f)
	  end
	 | OPAQUE_CONSTRAINTstrexp(info, strexp, _) => die "OPAQUE_CONSTRAINTstrexp.not impl"
	 | APPstrexp(i,funid,strexp) =>
	  let
	      val _ = chat ("[compiling functor application begin...]")
	      val (ce1, f1) = comp_strexp(fe,ce,strexp)

	      val (phi,Eres) =
		  case to_TypeInfo i of
		      SOME (ElabInfo.TypeInfo.FUNCTOR_APP_INFO {rea_inst,rea_gen,Env}) =>
			  (Environments.Realisation.oo(rea_inst,rea_gen), Env)
		    | _ => die "int_strexp.APPstrexp.no (phi,E) info"

	      val (fe',lookupInlineFunApp) = fe
	      val (strid,E,strexp0,ce0,ife0) = lookupInlineFunApp fe' funid
		  handle X => (print "**looking up funid failed**\n"; raise X)
	      val E' = Environments.Realisation.on_Env phi E
	      val ce2 = CE.constrain(ce1,E')

	      val strexp0' = TopdecGrammar.map_strexp_info (on_ElabInfo phi) strexp0

	      val (ce3, f2) = comp_strexp((ife0,lookupInlineFunApp),
					  CE.plus(ce0, CE.declare_strid(strid,ce2,CE.emptyCEnv)),
					  strexp0')
	      val ce4 = CE.constrain(ce3,Eres)
	      val _ = chat ("[compiling functor application end.]")
	  in (ce4, f1 o f2)
	  end
(*	  die "APPstrexp.not supported by compiler" *)
	 | LETstrexp(info, strdec, strexp) =>
	  let val (ce1, f1) = comp_strdec(fe,ce,strdec)
	      val (ce2, f2) = comp_strexp(fe,CE.plus(ce,ce1), strexp)
	  in (ce2, f1 o f2)
	  end

    and comp_strdec(fe,ce: CE.CEnv, strdec: strdec) =
      case strdec
	of DECstrdec(info, dec) => compileDec ce (false,dec)
	     (*topLevel=false: we always want the warnings, since this is
	      a compiler - not a top-level loop. - Martin *)
	 | STRUCTUREstrdec(info, strbind) => comp_strbind(fe,ce,strbind)
	 | LOCALstrdec(info, strdec1, strdec2) =>
	  let val (ce1, f1) = comp_strdec(fe,ce,strdec1)
	      val (ce2, f2) = comp_strdec(fe,CE.plus(ce,ce1), strdec2)
	  in (ce2, f1 o f2)
	  end
	 | EMPTYstrdec info => (CE.emptyCEnv, fn x => x)
	 | SEQstrdec(info, strdec1, strdec2) =>
(*	  let val (ce1, f1) = comp_strdec(fe,ce,strdec1)
	      val (ce2, f2) = comp_strdec(fe,CE.plus(ce,ce1), strdec2)
	  in (CE.plus(ce1,ce2), f1 o f2)
	  end
*)
           let val (env1, env',f1) = comp_strdecs(fe,ce,strdec)
           in (env1,f1)
           end

    and comp_strdecs(fe,ce,strdec) = (*fast compilation when SEQ associates to the left *)
       (case strdec of
	   SEQstrdec(info, strdec1, strdec2) =>
             let val (ce1, ce1', f1) = comp_strdecs(fe,ce,strdec1)
                             (* ce1' = ce plus ce1 *)
                 val (ce2, f2) = comp_strdec(fe,ce1',strdec2)
             in (CE.plus(ce1,ce2), CE.plus(ce1',ce2), f1 o f2)
             end
         | strdec1 =>
             let
               val (ce1,f1) = comp_strdec(fe,ce,strdec1)
             in
               (ce1, CE.plus(ce,ce1),f1)
             end
       )
    and comp_strbind(fe,ce,STRBIND(info,strid,strexp,strbind_opt)) =
      let val (ce1, f1) = comp_strexp(fe,ce,strexp)
	  val ce1 = CE.declare_strid(strid,ce1,CE.emptyCEnv)
      in case strbind_opt
	   of SOME strbind' =>
	     let val (ce2, f2) = comp_strbind(fe,ce,strbind')
	     in (CE.plus(ce1,ce2), f1 o f2)
	     end
	    | NONE => (ce1, f1)
      end

  in (*local*)
(*
    fun comp_strdecs(fe,ce,[]) = (CE.emptyCEnv, fn x => x)
      | comp_strdecs(fe,ce, strdec::strdecs) =
      let val (ce1,f1) = comp_strdec(fe,ce,strdec)
	  val (ce2,f2) = comp_strdecs(fe,CE.plus(ce,ce1),strdecs)
      in (CE.plus(ce1,ce2), f1 o f2)
      end
*)
    fun comp_strdecs_aux(fe,ce,[]) = ([CE.emptyCEnv], fn x => x)
      | comp_strdecs_aux(fe,ce, strdec::strdecs) =
      let val (ce1,f1) = comp_strdec(fe,ce,strdec)
	  val (ce2s,f2) = comp_strdecs_aux(fe,CE.plus(ce,ce1),strdecs)
      in (ce1::ce2s, f1 o f2)
      end

    fun comp_strdecs(fe,ce,strdecs) =
      let
        val (ces, f) = comp_strdecs_aux(fe,ce,strdecs)
      in
        (List.foldl(fn(ce',acc)=> CE.plus(acc,ce')) CE.emptyCEnv ces, f)
      end
  end (*local*)

  fun remove_dubs eq nil = nil
    | remove_dubs eq (x::xs) = if List.exists (fn x' => eq(x,x')) xs then remove_dubs eq xs
			       else x :: remove_dubs eq xs

  (* -----------------------------------------------------
   * Main compilation function
   * ----------------------------------------------------- *)

  (* Determine the scope of the declaration. Those lvars and
   * excons which are declared by the declarations are those
   * that appear in env1 but not in env. *)

  fun typed_declared_lvars env env1 =
    (* we associated the declared_lvars with dummy type schemes;
     * the real type schemes are put in later; actually, now we
     * could put them in... *)
    let
      fun rm_dups_sorted(lv:: (l1 as (lv'::rest))) =
             if Lvars.eq(lv,lv') then
               rm_dups_sorted l1
             else lv :: rm_dups_sorted l1
        | rm_dups_sorted l = l

      val lvars_env = CE.lvarsOfCEnv env
      val lvars_env_sorted = rm_dups_sorted(ListSort.sort (fn lv => fn lv' => Lvars.lt(lv,lv')) lvars_env)

      val lvars_env1 = CE.lvarsOfCEnv env1
      val lvars_env1_sorted = rm_dups_sorted(ListSort.sort (fn lv => fn lv' => Lvars.lt(lv,lv')) lvars_env1)

      fun minus(l1 as x::xs,l2 as y::ys) =
            if Lvars.eq(x,y) then minus(xs,ys)
            else if Lvars.lt(x,y) then x:: minus(xs, l2)
            else (* x> y *) minus(l1, ys)
        | minus([], l2) = []
        | minus(l1, []) = l1


                       (* env1 \ env *)
      val lvars_decl = minus(lvars_env1_sorted, lvars_env_sorted)
      val alpha = fresh_tyvar()
    in map (fn lv => {lvar=lv,tyvars = [alpha],Type=TYVARtype alpha})    (* forall alpha. alpha *)
      lvars_decl
    end
(*old
  fun typed_declared_lvars env env1 =
    (* we associated the declared_lvars with dummy type schemes;
     * the real type schemes are put in later; actually, now we
     * could put them in... *)
    let
      fun is_this_eq(lv, lv') = Lvars.eq(lv,lv')
      val lvars_env = CE.lvarsOfCEnv env
      val lvars_decl =
	foldl (fn (lv1, lvs) =>
	       if List.exists (fn lv => is_this_eq(lv,lv1)) lvars_env then lvs
	       else lv1::lvs) [] (CE.lvarsOfCEnv env1)
      val lvars_decl = remove_dubs Lvars.eq lvars_decl
      val alpha = fresh_tyvar()
    in map (fn lv => {lvar=lv,tyvars = [alpha],Type=TYVARtype alpha})    (* forall alpha. alpha *)
      lvars_decl
    end
old*)

  fun declared_excons env env1 : (Excon.excon * Type option) list =
    let val excons_env = CompilerEnv.exconsOfCEnv env
      val excons_decl =
	foldl (fn (ex1, exs) =>
	       if List.exists (fn ex => Excon.eq(ex,ex1)) excons_env then exs
	       else ex1::exs) [] (CE.exconsOfCEnv env1)
      val excons_decl = remove_dubs Excon.eq excons_decl
    in map (fn excon => (excon, NONE)) excons_decl  (*dummy-NONE*)
    end

  type strdec = TopdecGrammar.strdec
  type strexp = TopdecGrammar.strexp
  type funid = TopdecGrammar.funid
  type strid = TopdecGrammar.strid
  type Env = CompilerEnv.ElabEnv
  fun compileStrdecs fe env strdecs =
    let val _ = DatBinds.reset()
        val _ = TV.reset()
 (* 	val _ = Compiler.Profile.reset() *)

	(* val _ = chat "[comp_strdecs begin]" *)
        val _ = Timing.timing_begin()
        val (env1, f1) = comp_strdecs(fe,env,strdecs)
	(* val _ = chat "[comp_strdecs end]" *)

       val scope = FRAME{declared_lvars=typed_declared_lvars env env1,
			 declared_excons=declared_excons env env1}
       (* val _ = chat "[building scope end]" *)

       (* Build the lambda expression *)
       (* val _ = chat "[building term begin]" *)
       fun mk_lamb() = f1 scope
       val lamb = mk_lamb()
       val _ = Timing.timing_end "CompileDec"
(*       val _ = Compiler.Profile.setTimingMode false *)
(*
       val _ = let val name = OS.FileSys.tmpName ()
		   val os = TextIO.openOut name
	       in Compiler.Profile.report os; TextIO.closeOut os;
		 print ("Profile saved in file " ^ name ^ "\n")
	       end
*)
       (* val _ = chat "[building term end]" *)

       (* Then we can extract the datbinds *)
       val datbindss = DatBinds.extract()
       val _ = app unbox_datbinds datbindss
       val pgm = PGM(DATBINDS datbindss, lamb)
    in (env1, pgm)
    end

  type TypeScheme = StatObject.TypeScheme
  type tyvar = LambdaExp.tyvar
  val compileTypeScheme : TypeScheme -> tyvar list * Type = fn sigma =>
    compileTypeScheme (TypeScheme.to_TyVars_and_Type sigma)

  val _ = CE.set_compileTypeScheme compileTypeScheme   (* MEGA HACK - Martin *)


  end;
