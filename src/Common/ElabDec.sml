(* Elaborator for Core Language Declarations*)

(*$ElabDec : DEC_GRAMMAR ENVIRONMENTS STATOBJECT ELABDEC
         PARSE_INFO ELAB_INFO FINMAP REPORT PRETTYPRINT FLAGS
         CRASH LIST_HACKS LIST_SORT*)

functor ElabDec(structure ParseInfo : PARSE_INFO
		structure ElabInfo : ELAB_INFO
		  sharing ElabInfo.ParseInfo = ParseInfo
                structure IG : DEC_GRAMMAR
                  sharing type IG.info = ParseInfo.ParseInfo
                structure OG : DEC_GRAMMAR
		  sharing OG.Lab = IG.Lab
		  sharing OG.SCon = IG.SCon
		  sharing OG.Ident = IG.Ident
		  sharing OG.TyVar = IG.TyVar
		  sharing OG.TyCon = IG.TyCon
		  sharing OG.StrId = IG.StrId
                  sharing type OG.info = ElabInfo.ElabInfo

                structure Environments : ENVIRONMENTS
                  sharing type Environments.longid      = IG.longid
                  sharing type Environments.longtycon   = IG.longtycon
                  sharing type Environments.longstrid   = IG.longstrid
                  sharing type Environments.tycon       = IG.tycon
                  sharing type Environments.valbind     = IG.valbind
                  sharing type Environments.id          = IG.id
                  sharing type Environments.pat         = IG.pat
                  sharing type Environments.ExplicitTyVar = IG.tyvar

                structure StatObject : STATOBJECT
		  sharing StatObject.TyName = Environments.TyName
                  sharing type StatObject.TypeScheme   = Environments.TypeScheme
                  sharing type StatObject.ExplicitTyVar  = IG.tyvar
                  sharing type StatObject.TypeFcn      = Environments.TypeFcn
                  sharing type StatObject.realisation  = Environments.realisation
                  sharing type StatObject.Substitution = Environments.Substitution
                  sharing type StatObject.Type  = Environments.Type
                  sharing type StatObject.TyVar = Environments.TyVar
                  sharing type StatObject.scon  = IG.scon
                  sharing type StatObject.lab   = IG.lab
                  sharing type StatObject.level = Environments.level
                  sharing type ElabInfo.ErrorInfo.Type  = StatObject.Type
                  sharing type ElabInfo.ErrorInfo.TyVar = StatObject.TyVar
                  sharing type ElabInfo.ErrorInfo.TyName = StatObject.TyName
                  sharing type ElabInfo.ErrorInfo.id    = Environments.id
                  sharing type ElabInfo.ErrorInfo.longid = OG.longid
                  sharing type ElabInfo.ErrorInfo.longstrid = OG.longstrid
                  sharing type ElabInfo.ErrorInfo.tycon = OG.tycon
                  sharing type ElabInfo.ErrorInfo.lab   = OG.lab
                  sharing type ElabInfo.ErrorInfo.longtycon = OG.longtycon
                  sharing type ElabInfo.OverloadingInfo.Type = StatObject.Type
                  sharing type ElabInfo.TypeInfo.lab     = IG.lab
                  sharing type ElabInfo.TypeInfo.Type    = StatObject.Type
                  sharing type ElabInfo.TypeInfo.TyEnv  = Environments.TyEnv
                  sharing type ElabInfo.TypeInfo.TyVar   = StatObject.TyVar
		  sharing type ElabInfo.TypeInfo.longid = IG.longid 

                structure FinMap : FINMAP

                structure Report: REPORT

                structure PP: PRETTYPRINT
                  sharing type StatObject.StringTree
                               = Environments.StringTree
                               = IG.StringTree
                               = PP.StringTree
                  sharing type PP.Report = Report.Report

                structure Flags: FLAGS
		structure ListHacks: LIST_HACKS
		structure ListSort: LIST_SORT
                structure Crash: CRASH
               ) : ELABDEC =
  struct
    fun impossible s = Crash.impossible ("ElabDec." ^ s)
    fun noSome None s = impossible s
      | noSome (Some x) s = x
    fun map_opt f (Some x) = Some (f x)
      | map_opt f None = None

    (*import from StatObject:*)
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
         type Type         = StatObject.Type
         type RecType      = StatObject.RecType
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
         type Substitution = StatObject.Substitution
    structure Substitution = StatObject.Substitution
    structure TypeFcn      = StatObject.TypeFcn
    structure Realisation  = StatObject.Realisation
	    
    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    type Env               = Environments.Env
    type Context           = Environments.Context
    type constructor_map   = Environments.constructor_map
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure constructor_map = Environments.constructor_map

    (*import from other modules:*)
    structure ErrorInfo = ElabInfo.ErrorInfo
    structure OverloadingInfo = ElabInfo.OverloadingInfo
    structure TypeInfo = ElabInfo.TypeInfo
    structure Ident = IG.Ident
    type id = Ident.id
    type ParseInfo  = ParseInfo.ParseInfo
    type ElabInfo = ElabInfo.ElabInfo

    (*info*)

    (*okConv ParseInfo = the ParseInfo converted to ElabInfo with
     empty Error and Type fields:*)

    val okConv = ElabInfo.from_ParseInfo 

    (*errorConv (ParseInfo, ErrorInfo) = the ParseInfo converted
     to ElabInfo with the error info ErrorInfo. The assumption
     (forced by this type conversion) is that there can only be
     one error tag per info:*)

    fun errorConv (i : ParseInfo, e : ErrorInfo.ErrorInfo) : ElabInfo =
          ElabInfo.plus_ErrorInfo (okConv i) e 

    (*preOverloadingConv: as okConv, except it includes some
     OverloadingInfo:*)

    fun preOverloadingConv (i : ParseInfo, oi : OverloadingInfo.OverloadingInfo)
          : ElabInfo = ElabInfo.plus_OverloadingInfo (okConv i) oi 

    fun lookupIdError (i : ParseInfo, longid : Ident.longid) : ElabInfo =
          errorConv (i, ErrorInfo.LOOKUP_LONGID longid)

    fun lookupTyConError (i : ParseInfo, longtycon : Environments.longtycon)
          : ElabInfo = errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon)

    fun repeatedIdsError (i : ParseInfo, rids: ErrorInfo.RepeatedId list)
          : ElabInfo = errorConv (i, ErrorInfo.REPEATED_IDS rids)

    (*infixes*)
    val on = Substitution.on     infixr on
    val onC = C.on     infixr onC
    val onVE = VE.on     infixr onVE
    infixr oo    val op oo = Substitution.oo

    val C_cplus_E = C.cplus_E    infixr C_cplus_E
    val C_cplus_TE = C.cplus_TE   infixr C_cplus_TE 
    val C_cplus_VE_and_TE = C.cplus_VE_and_TE    infixr C_cplus_VE_and_TE

    (*types needed for the signature ELABDEC*)
    type PreElabDec  = IG.dec  
    type PostElabDec = OG.dec
    type PreElabTy   = IG.ty
    type PostElabTy  = OG.ty

    fun pr (msg : string, t : PP.StringTree) : unit =
          Report.print (Report.decorate (msg, PP.reportStringTree t))

    fun debug_pr_msg (msg: string): unit =
          if !Flags.DEBUG_ELABDEC then output(std_out,msg) else ()

    fun getRepeatedElements equal ls =
          let
	    fun NoOfOccurences x [] = 0
	      | NoOfOccurences x (y::ys) = 
	          if equal (x, y) then 1 + NoOfOccurences x ys
		  else NoOfOccurences x ys
	  in
	    List.all (fn e => (NoOfOccurences e ls) > 1) ls
	  end
    fun isEmptyTyVarList x = case x of nil => true | _ => false
    fun memberTyVarList x xs = List.exists (fn y => TyVar.eq (x,y)) xs

    fun where list elem =
          (case List.index (General.curry (op =) elem) list of
	     OK n => n
	   | Fail _ => impossible "where")

    (* Hooks needed by the compiler:

          o see signature TYPEINFO

     *)

    local open ElabInfo.TypeInfo
    in
     (* MEMO: no duplication checks here (or anywhere else!) *)
      fun addTypeInfo_CON (ElabInfo, C, isArrow, tau, instances, longid) =
            let
	      val (_, con) = Ident.decompose longid
	      val cons = C.lookup_fellow_constructors C longid
	    in
	      ElabInfo.plus_TypeInfo ElabInfo
	        (CON_INFO {numCons=List.size cons,
			   index=where cons con,instances=instances,
			   tyvars=[],Type=tau,longid=longid})
	    end

      fun addTypeInfo_EXCON (ElabInfo, tau, longid : Ident.longid) =
            (*The excon carries a value if the type tau is
	     functional, and doesn't if it's `exn'.*)
            ElabInfo.plus_TypeInfo ElabInfo
	      (EXCON_INFO {Type=tau, longid=longid})

      fun addTypeInfo_EXBIND (ElabInfo, typeOpt) = (*martin*)
	    ElabInfo.plus_TypeInfo ElabInfo (EXBIND_INFO {TypeOpt=typeOpt})

      fun addTypeInfo_LAB (ElabInfo, index, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (LAB_INFO {index=index, Type=tau, tyvars=[]})

      fun addTypeInfo_RECORD_ATPAT (ElabInfo, Type) =
            ElabInfo.plus_TypeInfo ElabInfo (RECORD_ATPAT_INFO {Type=Type})

      fun addTypeInfo_EXP (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (EXP_INFO {Type=tau})

      fun addTypeInfo_MATCH (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (MATCH_INFO {Type=tau})
        
      fun addTypeInfo_VAR (ElabInfo, instances) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_INFO {instances=instances})

      fun addTypeInfo_VAR_PAT (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo (VAR_PAT_INFO {tyvars=[], Type=tau})

      fun addTypeInfo_DATBIND (ElabInfo, TE : TyEnv) =
	    ElabInfo.plus_TypeInfo ElabInfo (DATBIND_INFO {TE=TE})

(*old (martin)
      fun addTypeInfo_DATBIND(datbind: OG.datbind,TE : TyEnv) =
        (* insert type names for the type constructors in the datatype binding info
         * --- cannot be done locally (in elab_datbind) since the type names
         * are changed when maximising equality for DATATYPEdec and ABSTYPE_dec !!
         * The type name are also changed by the ABS function !!
         * That is, allways be careful with adding type info to datatype bindings
         * and sub-phrases of datatype bindings.
         *)
        let
          fun do_datbind (OG.DATBIND(i,tyvars,tycon,conbind,datbind_opt)) =
            let 
	      fun noSome_here opt = noSome opt "addTypeInfo_DATBIND"
              val tyname = (noSome_here o TypeFcn.to_TyName
			    o TyStr.to_theta o noSome_here o TE.lookup TE) tycon
(*            val _ = pr("do_datbind: t = ",StatObject.layoutTyName t) *)
            in
              OG.DATBIND (ElabInfo.plus_TypeInfo i (DATBIND_INFO{TyName=t}),
			  tyvars, tycon, conbind, map_opt do_datbind datbind_opt)
            end
        in
          do_datbind datbind
        end
old*)

      fun addTypeInfo_PLAINvalbind (ElabInfo, tau) =
            ElabInfo.plus_TypeInfo ElabInfo
	      (PLAINvalbind_INFO {Type=tau, tyvars=[], escaping=[]})
    end

    (********
    Get a fresh type variable
    *********
    Returns a type variable with the equality and overloading 
    attributes set to be false.
    ********)

    fun TyVar_fresh () =
          TyVar.fresh {equality=false, overloaded=false}

    fun Type_fresh () = Type.from_TyVar (TyVar_fresh ())

   (* When we get elaboration errors we often need to return a bogus type.
      It's best if that's just 'a. *)

    val Type_bogus  = Type_fresh 

    fun Unify(tau, tau', i): Substitution * ElabInfo =
      case Type.unify (tau, tau')
        of Some subst => (subst, okConv i)
         | None =>
             (Substitution.bogus,
              errorConv(i, ErrorInfo.UNIFICATION(tau, tau'))
             )
    fun UnifyWithTexts(text,tau,text', tau', i): Substitution * ElabInfo =
      case Type.unify(tau, tau')
        of Some subst => (subst, okConv i)
         | None =>
             (Substitution.bogus,
              errorConv(i, ErrorInfo.UNIFICATION_TEXT(text,tau,text', tau'))
             )

   (* Traversal of patterns to build a VE. We only do this for `rec'
      bindings. Arguably, we should reject records and so on right here,
      but the caller isn't set up to deal with that (and ignoring them
      would give unbound var errors for actual occuring vars), so we work for
      all patterns. We reject illegal pattern forms in the `rec' post-pass. *)

   (*MEMO: these should all be returning sets. *)

    (********
    Find the variables bound in a valbind
    *********
    There is no checking for multiply declared variables
    ********)

    fun dom_vb(C: Context, vb: IG.valbind): id list =
      case vb
        of IG.PLAINvalbind(_, pat, _, vb_opt) =>
             C.dom_pat (C, pat)
	     @ (case vb_opt of
		  Some vb => dom_vb (C, vb)
		| None => nil)

         | IG.RECvalbind(_, vb) =>
             dom_vb(C, vb)

    (*initial_TE datbind = the TE to be used initially in
     elaborating a datbind. We determine the correct equality
     attributes when we maximise equality:*)

    fun initial_TE (IG.DATBIND(_, explicittyvars, tycon, _, None)) =
          TE.init explicittyvars tycon
      | initial_TE (IG.DATBIND(_, explicittyvars, tycon, _, Some datbind)) =
          TE.plus (TE.init explicittyvars tycon, initial_TE datbind)

   (* addLabelInfo: given a RecType and a PATROW, populate the info fields
      with LAB_INFO giving types for each label. This is needed by the compiler. 
      The index of each label (also needed by the compiler) is here set to 0 (zero)
      --- during overloading resolvations the indexes are corrected.
      The label info must be added _during_ elaboration (it cannot simply be done 
      when overloading is resolved) --- consider a patrow in a
      val pat = exp; here the info is needed in pat to generalise the types recorded. 
      The reason that the correct indexes are recorded later is that only when the
      type is resolved (so that record variables have been turned into record types 
      without record variables) do we know the correct index: consider
      fn {2 = x, ...} => x; here we do not know the index of 2 until we have found
      out what ... stands for (e.g., if it stands for { 1 = _ } the index of 2 should 
      be 1 but if it stands for { 3 = _ } the index of 2 should be 0). 

      Thus addLabelIndexInfo below is used during resolvation to record correct indices.

    *)

    fun addLabelInfo(recType, patrow) =
      let
        val labtys = Type.RecType.to_list recType
        val sortedLabs = map (#1) labtys
        fun f (OG.PATROW (i, lab, pat, patrow_opt)) =
          (case ElabInfo.to_ErrorInfo i of
            None => 
              let
                val index = where sortedLabs lab
(*
val _ = output(std_out,"ElabDec.addLabelInfo: index = " ^ Int.string index ^ "\n")
val _ = pr("ElabDec.addLabelInfo: recType = ",
           StatObject.layoutType (StatObject.mkTypeRecType recType))
*)
                val (_,tau) = List.nth index labtys 
                              handle List.Subscript _ =>
                                impossible "addLabelInfo"
              in
                OG.PATROW (addTypeInfo_LAB(i, index, tau),
			   lab, pat, map_opt f patrow_opt)
              end
           | Some _ => OG.PATROW (i, lab, pat, patrow_opt))

          | f (OG.DOTDOTDOT i) = OG.DOTDOTDOT i
      in
        f patrow
      end

    fun addLabelIndexInfo(Type,patrow) =
      let
        val recType = noSome (Type.to_RecType Type) "addLabelIndexInfo"

        val labtys = Type.RecType.to_list recType
        val sortedLabs = map (#1) labtys
        fun f(OG.PATROW(i, lab, pat, patrow_opt)) =
          (case ElabInfo.to_ErrorInfo i of
            None => 
              (case ElabInfo.to_TypeInfo i of
                 Some typeinfo =>
                   (case typeinfo of
                      TypeInfo.LAB_INFO{index,tyvars,Type} => 
                        let
                          val index = where sortedLabs lab
(*
val _ = output(std_out,"ElabDec.addLabelIndexInfo: index = " ^ Int.string index ^ "\n")
val _ = pr("ElabDec.addLabelIndexInfo: recType = ",
           StatObject.layoutType (StatObject.mkTypeRecType recType))
*)
                        in
                          OG.PATROW (ElabInfo.plus_TypeInfo i 
				     (TypeInfo.LAB_INFO {index=index,
							 Type=Type,tyvars=tyvars}),
				     lab, pat, map_opt f patrow_opt)
                        end
                    | _ => 
                        impossible "addLabelIndexInfo: unexpected typeinfo")
               | None => 
                   impossible "addLabelIndexInfo: no typeinfo")
          | Some _ => OG.PATROW(i, lab, pat, patrow_opt))
             
          | f(OG.DOTDOTDOT i) = OG.DOTDOTDOT i
      in
        f patrow
      end
 

    (* 
     * generalise_type_info_valbind(C,S,valbind): apply the substitution S and then 
     * generalise all the type info recorded in PLAINvalbind and the left hand 
     * side patterns of the value binding. Note that we record tyvars and types (and
     * not typeschemes as one could imagine); this is not accidental: we don't 
     * want to risk that the bound type variables are renamed (by alpha-conversion) ---
     * the compiler is a bit picky on the exact type information, so alpha-conversion
     * is not allowed here!
     * 
     * It _is_ necessary to traverse and generalise type information, I think,
     * (instead of just using the generalised types for the variables bound in 
     * the variable environment for the value binding): consider 
     *  datatype ('a, 'b) t = C of 'a;
     *  val C x = C (fn y => y) 
     * Here, the type scheme for x is FORALL['a].'a->'a, but the type for the
     * exp (and C x) is ('a->'a,'b) t, so the bound type variables of the type for 
     * (C x) cannot be obtained from the bound type variables of the type scheme for 
     * x (in particular, one cannot simply quantify those type variables occuring in
     * the type for (C x) which do not occur in the type scheme for x (here 'b)
     * due to imperative type variables and since those type variables could occur
     * in the context).
     *
     * -- Lars
     *)

    fun generalise_type_info_valbind (C : Context, S : Substitution,
                                      valbind: OG.valbind) : OG.valbind =
      let 
	fun harmless_con (C : Context) (longid : Ident.longid) =
	      (case C.lookup_longid C longid of
		 Some (VE.LONGVAR _) => false
	       | Some (VE.LONGCON _) => #2 (Ident.decompose longid) <> Ident.id_REF
	       | Some (VE.LONGEXCON _) => true  
	       | None => true)
		(*why `true' and not `false' or `impossible ...'?  see my diary
		 19/12/1996 14:17. tho.*)

        fun close isExpansive tau = Type.close (not isExpansive) (S on tau)

        fun do_valbind (vb : OG.valbind) : OG.valbind =
          case vb of
            OG.PLAINvalbind(i, pat, exp, vb_opt) =>
              OG.PLAINvalbind
              (case ElabInfo.to_TypeInfo i of
                 Some (TypeInfo.PLAINvalbind_INFO{Type,tyvars=[], escaping = _ }) =>
                   let val (generalisable_tyvars, escaping_tyvars) = 
                             close (OG.expansive (harmless_con C) exp) Type
                   in 
                     ElabInfo.plus_TypeInfo i 
                     (TypeInfo.PLAINvalbind_INFO{Type=Type,
                                                 tyvars=generalisable_tyvars,
                                                 escaping = escaping_tyvars})
                   end
               | _ => i (* impossible "ElabDec.do_valbind: wrong type info"*),
               do_pat (#1 o close (OG.expansive (harmless_con C) exp)) pat,
               exp,
               case vb_opt of None => None | Some vb => Some (do_valbind vb))

          | OG.RECvalbind(i, vb) => OG.RECvalbind(i,do_valbind vb)

        and do_pat close_tau pat =
          case pat of 
            OG.ATPATpat(i, atpat) => OG.ATPATpat(i,do_atpat close_tau atpat)

          | OG.CONSpat(i, longid_op as OG.OP_OPT(longid,withOp), atpat) =>
	      (case ElabInfo.to_TypeInfo i 
		 of Some(TypeInfo.CON_INFO{numCons,index,Type,longid,instances,...}) =>
		   (* generalise type of constructor and recurse *)
		   OG.CONSpat(ElabInfo.plus_TypeInfo i
			      (TypeInfo.CON_INFO{numCons=numCons,
						 index=index,
						 Type=Type,
						 tyvars=close_tau Type,
						 instances=instances,
						 longid=longid}),
			      longid_op,
			      do_atpat close_tau atpat)
		  | _ => pat) (* no generalisation: exceptions do not have type schemes *)

          | OG.TYPEDpat(i, pat, ty) => OG.TYPEDpat(i, do_pat close_tau pat, ty)

          | OG.LAYEREDpat(i, id as OG.OP_OPT(id', withOp),ty_opt, pat) =>
              OG.LAYEREDpat
              (case ElabInfo.to_TypeInfo i of
                 Some(TypeInfo.VAR_PAT_INFO{tyvars,Type}) =>
                   ElabInfo.plus_TypeInfo i 
                   (TypeInfo.VAR_PAT_INFO{tyvars=close_tau Type,Type=Type})
               | _ => impossible "do_pat (LAYERED): wrong type info",
                   id,
                   ty_opt,
                   do_pat close_tau pat)

          | OG.UNRES_INFIXpat _ => impossible "do_pat(UNRES_INFIX)"

        and do_atpat close_tau atpat =
          case atpat of
            OG.WILDCARDatpat _ => atpat

          | OG.SCONatpat _ => atpat

          | OG.LONGIDatpat(i, longid_op as OG.OP_OPT(longid,withOp)) =>
              (case ElabInfo.to_TypeInfo i 
		 of Some(TypeInfo.CON_INFO{numCons,index,Type,longid,instances,...}) =>
                   let val i' = ElabInfo.plus_TypeInfo i
		                (TypeInfo.CON_INFO{numCons=numCons,
						   index=index,
						   Type=Type,
						   tyvars=close_tau Type,
						   instances=instances,
						   longid=longid})
		   in OG.LONGIDatpat(i', longid_op)
		   end
		  | Some(TypeInfo.VAR_PAT_INFO{tyvars,Type}) => 
                   let val i' = ElabInfo.plus_TypeInfo i 
		                (TypeInfo.VAR_PAT_INFO{tyvars=close_tau Type,Type=Type})
		   in OG.LONGIDatpat(i', longid_op)
		   end
		  | _ => atpat) 

          | OG.RECORDatpat(i, patrowOpt) =>
	      OG.RECORDatpat (i, map_opt (do_patrow close_tau) patrowOpt)

          | OG.PARatpat(i, pat) => OG.PARatpat(i,do_pat close_tau pat)

        and do_patrow close_tau patrow = 
          case patrow of 
            OG.DOTDOTDOT _ => patrow
          | OG.PATROW(i, l, pat, patrowOpt) =>
              OG.PATROW
              (case ElabInfo.to_TypeInfo i of
                 Some(TypeInfo.LAB_INFO{index, Type,...}) =>
                   ElabInfo.plus_TypeInfo i 
                   (TypeInfo.LAB_INFO{index=index,Type=Type,
                                      tyvars=close_tau Type})
               | _ => impossible "do_patrow: wrong type info",
		   l,
		   do_pat close_tau pat, map_opt (do_patrow close_tau) patrowOpt)

      in
        do_valbind valbind
      end

    (*
     * phi_on_dec phi dec :
     *   applies the type realisation to the recorded type information in the 
     * dec (used in connection with abstype declarations). Note that the 
     * phi is _not_ applied to type names recorded in DATBIND_INFO
     *)

    fun phi_on_dec phi (dec: OG.dec) =
      let
        val phi_on_TyName = Realisation.on_TyName phi
        val phi_on_Type   = Realisation.on_Type phi
	val phi_on_TE     = (* Environments.tyrea_on_TE *) fn TE => TE
        open TypeInfo
        fun phi_on_TypeInfo ti =
          case ti of
            LAB_INFO {index, tyvars, Type} =>
              LAB_INFO {index=index,tyvars=tyvars,Type=phi_on_Type Type}
          | RECORD_ATPAT_INFO{Type} =>
              RECORD_ATPAT_INFO{Type=phi_on_Type Type}
          | VAR_INFO {instances} =>
              VAR_INFO {instances = map phi_on_Type instances}
          | VAR_PAT_INFO {tyvars,Type} =>
              VAR_PAT_INFO{tyvars=tyvars,Type=phi_on_Type Type}
          | CON_INFO {numCons, index, instances, tyvars, Type,longid} =>
              CON_INFO {numCons=numCons,index=index,
                        instances=map phi_on_Type instances,
                        tyvars=tyvars,Type=phi_on_Type Type,
                        longid=longid}
          | EXCON_INFO {Type,longid} =>
              EXCON_INFO {Type=phi_on_Type Type,
                          longid=longid}
	  | EXBIND_INFO {TypeOpt} => 
	      EXBIND_INFO {TypeOpt = map_opt phi_on_Type TypeOpt}
	  | DATBIND_INFO {TE} =>
	      DATBIND_INFO {TE=phi_on_TE TE}

          | EXP_INFO {Type } =>
              EXP_INFO{Type=phi_on_Type Type}
          | MATCH_INFO {Type} =>
              MATCH_INFO{Type=phi_on_Type Type}
          | PLAINvalbind_INFO {tyvars, escaping, Type} =>
              PLAINvalbind_INFO {tyvars=tyvars, escaping = escaping,Type=phi_on_Type Type}
        fun phi_on_Info i =
              (case ElabInfo.to_TypeInfo i of
		 None => i
	       | Some ti => ElabInfo.plus_TypeInfo i (phi_on_TypeInfo ti))
      in
        OG.map_dec_info phi_on_Info dec
      end

    
    (********************************************************)
    (*      Elaboration (type checking)                     *)
    (********************************************************)


    (****** atomic expressions ******)

    fun elab_atexp (C : Context, atexp : IG.atexp)
      : (Substitution * Type * OG.atexp) =

        case atexp of

          (* special constants *)                               (*rule 1*)
          IG.SCONatexp(i, scon) =>
            (Substitution.Id, Type.of_scon scon, OG.SCONatexp (okConv i, scon))

          (* identifiers - variables or constructors *)         (*rule 2*)
        | IG.IDENTatexp(i, IG.OP_OPT(longid, withOp)) =>
            (case C.lookup_longid C longid of

               (* Variable *)                                   
              Some(VE.LONGVAR sigma) =>
                 let 
                   val (instance,instances) = TypeScheme.instance'' sigma
                 in  
                   (Substitution.Id, instance,
                    OG.IDENTatexp(
                      (case Type.getOverloadedTyVar instance of
                           None => addTypeInfo_VAR(okConv i,
                                                   instances)
                         | Some tv => 
                             addTypeInfo_VAR(preOverloadingConv
                                             (i, OverloadingInfo.UNRESOLVED 
                                              (Type.from_TyVar tv)),
                                             instances)),
                      OG.OP_OPT(longid, withOp)
                    )
                   )
                 end

              (* Constructor *)
            | Some(VE.LONGCON sigma) =>
                let
                  val (tau,instances) = TypeScheme.instance'' sigma
                in
                  (Substitution.Id, tau,
		   OG.IDENTatexp(addTypeInfo_CON(okConv i, C,
						 Type.is_Arrow tau,
						 tau,
						 instances,
						 longid
                                                  ),
                                   OG.OP_OPT(longid, withOp)
				   )
                  )
                end

             (* Exception constructor *)
           | Some(VE.LONGEXCON tau) =>
                (Substitution.Id,
                 tau,
		 OG.IDENTatexp(addTypeInfo_EXCON(okConv i, tau, longid),
			       OG.OP_OPT(longid, withOp)
			       )
		 )

             (* Not found in current context *)
           | None =>
               (Substitution.bogus, Type_bogus (),
		OG.IDENTatexp(lookupIdError (i, longid),
			      OG.OP_OPT(Ident.bogus, withOp)
			      )
		)
          )

          (* record expression *)                               (*rule 3*)
        | IG.RECORDatexp(i, None) =>
            (Substitution.Id, Type.Unit, OG.RECORDatexp(okConv i,None)) 

          (* record expression *)
        | IG.RECORDatexp(i, Some exprow) =>
            let
              val (S, rho, out_exprow) = elab_exprow(C,exprow)
            in
              (S, Type.from_RecType rho,
               OG.RECORDatexp (okConv i, Some out_exprow)) 
            end 

          (* let expression *)                                  (*rule 4%%*)
        | IG.LETatexp(i, dec, exp) => 
            let
              val (S1, E, out_dec)   = elab_dec(C,dec)
              val (S2, tau, out_exp) = elab_exp((S1 onC C) C_cplus_E E, exp)
	      val out_i = (case TyName.Set.list 
			          (TyName.Set.difference 
				     (Type.tynames tau) (C.to_T C)) of
			     [] => okConv i
			   | tynames => 
			       errorConv
			         (i, ErrorInfo.DATATYPES_ESCAPE_SCOPE tynames))
            in
	      (S2 oo S1, tau, OG.LETatexp (out_i, out_dec, out_exp))
            end

          (* parenthesised expression *)
        | IG.PARatexp(i, exp) =>                                (*rule 5*)
            let val (S, tau, out_exp) = elab_exp(C,exp)
            in (S, tau, OG.PARatexp(okConv i,out_exp)) end

    (******** expression rows ********)

    and elab_exprow (C : Context, exprow : IG.exprow) :
        (Substitution * RecType * OG.exprow) =

        case exprow of 

          (* Expression row *)                                  (*rule 6*)
          IG.EXPROW(i, lab, exp, None) =>
            let
              val (S, tau, out_exp) = elab_exp(C, exp)
              val rho = Type.RecType.add_field (lab,tau) Type.RecType.empty
            in
              (S, rho, OG.EXPROW(okConv i,lab,out_exp,None))
            end

          (* Expression row *)
        | IG.EXPROW(i, lab, exp, Some exprow) =>
            let
              val (S1, tau, out_exp   ) = elab_exp(C, exp)
              val (S2, rho, out_exprow) = elab_exprow(S1 onC C,exprow)
            in
              if (List.member lab (Type.RecType.sorted_labs rho)) then
                (S2, rho, 
                 OG.EXPROW(repeatedIdsError(i, [ErrorInfo.LAB_RID lab]),
                           lab, out_exp, Some out_exprow))
              else
                (S2 oo S1, Type.RecType.add_field (lab,S2 on tau) rho,
                 OG.EXPROW(okConv i,lab,out_exp,Some out_exprow))
            end

    (******** expressions ********)

    and elab_exp(C : Context, exp : IG.exp) : 
        (Substitution * Type * OG.exp) =
      let
        val _ =
          if !Flags.DEBUG_ELABDEC then
            pr("elab_exp: ", IG.layoutExp exp)
          else ()

        val (S, ty, exp') = elab_exp'(C, exp)

        val _ =
          if !Flags.DEBUG_ELABDEC then
            pr("giving:   ", Type.layout ty)
          else ()
      in
        (S, ty, exp')
      end

    and elab_exp'(C, exp) =
      case exp

           (* Atomic expression *)                              (*rule 7*)
        of IG.ATEXPexp(i, atexp) =>
             let
               val (S, tau, out_atexp) = elab_atexp(C, atexp)
             in
               (S, tau, OG.ATEXPexp(addTypeInfo_EXP(okConv i, tau),out_atexp))
             end

           (* Application expression *)                         (*rule 8*)
         | IG.APPexp(i, exp, atexp) => 
             let
               val (S1, tau1, out_exp)   = elab_exp(C, exp)
               val (S2, tau2, out_atexp) = elab_atexp(S1 onC C, atexp)
               val new   = Type_fresh ()
               val arrow = Type.mk_Arrow(tau2,new) 
               val (S3, i') = UnifyWithTexts("operand suggests operator type",arrow, 
                                             "but I found operator type",S2 on tau1, i)
               val tau = S3 on new
             in
               (S3 oo S2 oo S1, tau,
                OG.APPexp(addTypeInfo_EXP(i', tau), out_exp, out_atexp))
             end

           (* Typed expression *)                               (*rule 9*)
         | IG.TYPEDexp(i, exp, ty) =>
             let
               val (S1, tau, out_exp) = elab_exp(C, exp)
               val (tau', out_ty) = elab_ty(S1 onC C, ty)
               val (S2, i') = UnifyWithTexts("type of expression",tau,"disagrees with your type constraint", tau', i)
               val tau'' = S2 on tau'
             in
               (S2 oo S1, tau'', OG.TYPEDexp(addTypeInfo_EXP(i',tau''), out_exp, out_ty))
             end

           (* Handle exception *)                               (*rule 10*)
         | IG.HANDLEexp(i, exp, match) =>
             let
               val (S1, tau1, out_exp)   = elab_exp(C, exp)
               val (S2, tau2, out_match) = elab_match(S1 onC C, match)
               val matchTy = Type.mk_Arrow(Type.Exn, tau1)
               val (S3, i') = UnifyWithTexts("handled expression suggests handler type", matchTy, 
                                             "but I found handler type", tau2, i)
               val tau3 = (S3 oo S2) on tau1
             in
               (S3 oo S2 oo S1, tau3,
                OG.HANDLEexp(addTypeInfo_EXP(i',tau3), out_exp, out_match))
             end

           (* Raise exception *)                                (*rule 11*)
(*old
         | IG.RAISEexp(i, exp) =>
             let
               val (S1, tau1, out_exp) = elab_exp(C, exp)
               val exnType = Type.Exn
               val (S2, i')    = Unify(exnType, S1 on tau1, i)
               val tau = Type_fresh ()
             in
               (S2 oo S1, tau, OG.RAISEexp(addTypeInfo_EXP(i',tau), out_exp))
             end
old*)
(*mads begin*)
         | IG.RAISEexp(i, exp) =>
             let
               val (S1, tau1, out_exp) = elab_exp(C, exp)
               val exnType = Type.Exn
               val (S2, i')    = UnifyWithTexts("type of expression after 'raise' should be",
                                                exnType, "but I found type", tau1, i)
               val tau = Type_fresh ()
             in
               (S2 oo S1, tau, OG.RAISEexp(addTypeInfo_EXP(i',tau), out_exp))
             end
(*mads end*)
           (* Function expression *)                            (*rule 12*)
         | IG.FNexp(i, match) => 
             let
               val (S, tau, out_match) = elab_match(C, match)
             in
               (S, tau, OG.FNexp(addTypeInfo_EXP(okConv i,tau), out_match))
             end

         | IG.UNRES_INFIXexp _ =>
             impossible "elab_exp(UNRES_INFIX)"


    (******** matches ********)

    and elab_match (C : Context, match : IG.match) :
        (Substitution * Type * OG.match) =

        case match of

          (* Match *)                                           (*rule 13*)
          IG.MATCH(i, mrule, None) =>
            let val (S, tau, out_mrule) = elab_mrule(C,mrule)
            in (S, tau, OG.MATCH(addTypeInfo_MATCH(okConv i,tau),out_mrule,None)) end

          (* Match *)
        | IG.MATCH(i, mrule, Some match') =>
            let
              val (S ,tau ,out_mrule) = elab_mrule(C,mrule)
              val (S',tau',out_match) = elab_match(S onC C,match')
              val (S'', i') = UnifyWithTexts("type of match rule with wildcard", tau',
                                             "type of previous match rules", S' on tau,i)
              val tau'' = S'' on tau'
            in
              (S'' oo S' oo S, tau'',
               OG.MATCH(addTypeInfo_MATCH(i',tau''), out_mrule, Some out_match))
            end

    (******** match rules ********)

    and elab_mrule (C : Context, mrule : IG.mrule) : 
        (Substitution * Type * OG.mrule) =

        case mrule of

          (* Match rule *)                                      (*rule 14*)
          IG.MRULE(i, pat, exp) =>
            let
              val (S, (VE,tau), out_pat) = elab_pat(C,pat)
              val (S',tau',  out_exp) = 
                     elab_exp(C.plus_VE (S onC C, VE),exp)
              val S'' = S' oo S
	      val out_i = (case TyName.Set.list
			          (TyName.Set.difference (VE.tynames VE)
				     (C.to_T C)) of
			     [] => okConv i
			   | tynames =>
			       errorConv
			         (i, ErrorInfo.DATATYPES_ESCAPE_SCOPE tynames))
            in
	      (S'', Type.mk_Arrow (S'' on tau,tau'),
	       OG.MRULE (out_i ,out_pat,out_exp))
            end

    (******** declarations ********)

    and elab_dec(C : Context, dec : IG.dec) :
        (Substitution * Env * OG.dec) =

        (case dec of

           (* Value declaration *)                              (*rule 15*)
	   
	   IG.VALdec (i, ExplicitTyVars, valbind) =>
	     let
               val U = ListHacks.union (ExplicitTyVars,
		       ListHacks.minus (Environments.unguarded_valbind valbind, C.to_U C))
               val _ = Level.push ()
               val (S, VE, out_valbind) =
                     elab_valbind (C.plus_U (C, U), valbind)
               val _ = Level.pop()
               val VE' = C.close (S onC C, valbind, VE)
               val out_valbind = generalise_type_info_valbind
		                   (S onC C, S, out_valbind)
		 (*generalise type info recorded in PLAINvalbind and
		  patterns of value bindings*)
               val out_i =
		     (case ListHacks.intersect
		             (ExplicitTyVars, C.to_U C) of
		        [] => okConv i
		      | explicittyvars => errorConv
			  (i, ErrorInfo.TYVARS_SCOPED_TWICE
			        (map TyVar.from_ExplicitTyVar
				   explicittyvars)))

	     (*The side condition ``U n tyvars VE' = {}'' is enforced partly
	      by disallowing unification of free explicit tyvars (giving the
	      error message

	              (fn x => let val y : 'a = x in y y end) 666 ;
				       ^^^^^^^^^^
               Type clash,
                  type of left-hand side pattern:     'a
                  type of right-hand side expression: int).

	      And partly the side condition is enforced by the restriction
	      that the same explicit tyvar may not be scoped at two
	      valbinds within each other (Definition 1997, §2.9, last
	      bullet).  The latter is checked above by checking whether
	      any of `ExplicitTyVars' are in U of C (I hope that does it?)
	      24/01/1997 15:38. tho.*)
            
             in
               (S, E.from_VE VE',
		OG.VALdec (out_i, ExplicitTyVars, out_valbind)) 
	     end

           (* `fun'-declaration *)
         | IG.UNRES_FUNdec _ => impossible "elab_dec(UNRES_FUN)"

           (* Type declaration *)                               (*rule 16*)
         | IG.TYPEdec(i, typbind) =>
             let
               (* Note that no substitutions are produced *)
               val (TE, out_typbind) = elab_typbind(C, typbind)
             in
               (Substitution.Id, E.from_TE TE,
                OG.TYPEdec(okConv i, out_typbind))
             end

           (* Datatype declaration *)                           (*rule 17*)
         | IG.DATATYPEdec(i, datbind) =>
             let
               val TE = initial_TE datbind
               val ((VE1, TE1), out_datbind) = elab_datbind(C C_cplus_TE TE, datbind)
               val (VE2, TE2) = Environments.maximise_equality_in_VE_and_TE
		                  (VE1, TE1) 
               val _ = debug_pr_msg "elab_dec(DATATYPEdec)"
             in
               (Substitution.Id, E.from_VE_and_TE (VE2, TE2),
                OG.DATATYPEdec(addTypeInfo_DATBIND (okConv i, TE2),
                               out_datbind)) (*martin*)
             end

	   (*datatype replication*)                             (*rule 18*)
	 | IG.DATATYPE_REPLICATIONdec(i, tycon, longtycon) => 
	     (case C.lookup_longtycon C longtycon of
		Some tystr =>
		  let
		    val TE = TE.singleton (tycon, tystr)
		    val (theta, VE) = TyStr.to_theta_and_VE tystr 
		  in
		    (Substitution.Id,
		     E.from_VE_and_TE (VE,TE),
		     OG.DATATYPE_REPLICATIONdec
		       (addTypeInfo_DATBIND (okConv i, TE), tycon, longtycon))
		  end
	      | None =>
		  (Substitution.bogus,
		   E.bogus,
		   OG.DATATYPE_REPLICATIONdec
		     (lookupTyConError (i,longtycon), tycon, longtycon)))

           (* Abstype declaration *)                            (*rule 19*)
         | IG.ABSTYPEdec(i, datbind, dec) =>
             let
               val TE = initial_TE datbind
               val ((VE1, TE1), out_datbind) = elab_datbind(C C_cplus_TE TE, datbind)
               val (VE2, TE2) = Environments.maximise_equality_in_VE_and_TE
		                  (VE1, TE1)
               val (S, E, out_dec) = elab_dec(C C_cplus_VE_and_TE (VE2,TE2), dec)
               val (E',TE2',phi) = Environments.ABS' (TE2, E)
             in
               (S,E',
		OG.ABSTYPEdec(addTypeInfo_DATBIND (okConv i, TE2'),
			      out_datbind,
			      phi_on_dec phi out_dec)) (*martin*)
             end

           (* Exception declaration *)                          (*rule 20*)
         | IG.EXCEPTIONdec(i, exbind) =>
             let
               val (VE, out_exbind) = elab_exbind (C, exbind)
             in
               (Substitution.Id,
                E.from_VE VE,
                OG.EXCEPTIONdec(okConv i, out_exbind))
             end

           (* Local declaration *)                              (*rule 21*)
         | IG.LOCALdec(i, dec1, dec2) =>
             let
               val (S1, E1, out_dec1) = elab_dec(C,dec1)
               val (S2, E2, out_dec2) = elab_dec((S1 onC C) C_cplus_E E1,dec2)
             in
               (S2 oo S1, E2, OG.LOCALdec(okConv i,out_dec1,out_dec2))
             end

           (* Open declaration *)                               (*rule 22*)
         | IG.OPENdec(i, list) =>
             let
               fun process(E0, list)
                   : Env * Environments.longstrid OG.WithInfo list =
                 case list
                   of IG.WITH_INFO(i, longstrid) :: rest =>
                        (case C.lookup_longstrid C longstrid of
			   Some E =>
                                let val (E', rest') = process(E0, rest)
                                in
                                  (E.plus (E, E'),
                                   OG.WITH_INFO (okConv i, longstrid) :: rest')
                                end

                            | None =>   (* Lookup failure: process rest of
                                           list and build env regardless. *)
                                let
                                  val (E', rest') = process(E0, rest)
                                  val ei = ErrorInfo.LOOKUP_LONGSTRID longstrid
                                in
                                  (E', OG.WITH_INFO(errorConv(i, ei), longstrid)
                                       :: rest'
                                  )
                                end
                        )

                    | nil => (E.empty, nil)

               val (E', list') = process(E.empty, list)
             in
               (Substitution.Id, E', OG.OPENdec(okConv i, list'))
             end

         | IG.INFIXdec(i, prec, ids) =>    (* infix -- no rule in Definition *)
             (Substitution.Id,
              E.from_VE VE.empty,
              OG.INFIXdec(okConv i, prec , ids))

         | IG.INFIXRdec(i, prec, ids) =>   (* infixr -- no rule in Definition *)
             (Substitution.Id,
              E.from_VE VE.empty,
              OG.INFIXRdec(okConv i, prec, ids))

         | IG.NONFIXdec(i, ids) =>         (* nonfix -- no rule in Definition *)
             (Substitution.Id,
              E.from_VE VE.empty,
              OG.NONFIXdec(okConv i, ids))

           (* Empty declaration *)                              (*rule 23*)
         | IG.EMPTYdec(i) =>
             (Substitution.Id, E.from_VE VE.empty, OG.EMPTYdec(okConv i))

           (* Sequential declaration *)                         (*rule 24*)
         | IG.SEQdec(i, dec1, dec2) =>
             let
               val (S1, E1, out_dec1) = elab_dec(C,dec1)
               val (S2, E2, out_dec2) = elab_dec((S1 onC C) C_cplus_E E1,dec2)
               val E1' = E.on (S2, E1)
             in
               (S2 oo S1,
                E.plus (E1',E2),
                OG.SEQdec(okConv i,out_dec1,out_dec2)) 
             end)

    (****** value bindings - Definition, p. ? ******)

    and elab_valbind(C : Context, valbind : IG.valbind)
          : (Substitution * VarEnv * OG.valbind) =

        case valbind of

        (* Simple value binding *)                              (*rule 25*)
        IG.PLAINvalbind(i, pat, exp, valbind_opt) =>
          let
            val (S0, (VE,tau), out_pat) = elab_pat(C, pat)
            val (S1, tau1, out_exp) = elab_exp(S0 onC C, exp)
            val (S2, i') = UnifyWithTexts("type of left-hand side pattern",(S1 oo S0) on tau,
                                          "type of right-hand side expression", tau1, i)

            (* if there was a unification error in the line above, change the right source
               info field of i' to become the right end of exp : *)
            val i' = case ElabInfo.to_ErrorInfo i' of
                    None => i'
                    | Some _ => ElabInfo.retractRight (i', OG.get_info_exp out_exp)

            val (S3, VE', valbind_opt') =
              case valbind_opt
                of Some valbind =>
                     let
                       val (S, VE, vb) =
                         elab_valbind((S2 oo S1 oo S0) onC C, valbind)
                     in
                       (S, VE, Some vb)
                     end

                 | None =>
                     (Substitution.Id, VE.empty, None)
            val intdom = EqSet.intersect (VE.dom VE) (VE.dom VE')
          in 
            if EqSet.isEmpty intdom then
	      (case List.all IG.is_'true'_'nil'_etc
		      (EqSet.list (VE.dom VE)) of
		 [] =>
		   (S3 oo S2 oo S1 oo S0, 
		    VE.plus ((S3 oo S2 oo S1 oo S0) onVE VE, VE'),
		    OG.PLAINvalbind
		    (addTypeInfo_PLAINvalbind(i', tau1),
		     out_pat, out_exp, valbind_opt'))
	       | ids =>
		   (S3, VE',
		    OG.PLAINvalbind
		      (ElabInfo.plus_ErrorInfo i'
		         (ErrorInfo.REBINDING_TRUE_NIL_ETC ids),
		       out_pat, out_exp, valbind_opt')))
            else
              (S3, VE',
               OG.PLAINvalbind((case ElabInfo.to_ErrorInfo i' of
                                  None => 
                                   ElabInfo.plus_ErrorInfo i' 
                                    (ErrorInfo.REPEATED_IDS 
                                     (map ErrorInfo.ID_RID (EqSet.list intdom)))
                                | Some _ => i'),
                               out_pat, out_exp, valbind_opt'))
          end
                                                                (*rule 26*)
                                (* Recursive value binding. Rather tricky
                                   because we have to plant error info after
                                   the second pass. Make that `very tricky.' *)
      | IG.RECvalbind(i, valbind) => 
          let
                                (* Function to unify the occurrence of a
                                   variable in two VE's. The result is a
                                   substitution and an ErrorInfo tag. *)

            fun processID(i, VE, VE', id): Substitution * ElabInfo =
                  (case (VE.lookup VE id, VE.lookup VE' id) of
		     (Some (VE.LONGVAR sigma1), Some (VE.LONGVAR sigma2)) =>
		       let val (_, tau1) = TypeScheme.to_TyVars_and_Type sigma1
			   val (_, tau2) = TypeScheme.to_TyVars_and_Type sigma2
		       in (case Type.unify(tau1, tau2) of
			     Some S => (S, i)
			   | None => (Substitution.bogus,
				      ElabInfo.plus_ErrorInfo i
				      (ErrorInfo.UNIFICATION(tau1, tau2))))
		       end
		    | _ => impossible "processID")

                                (* Traverse the out_valbind, doing a
                                   unification (and adding ErrorInfo if reqd.)
                                   and giving a subst. at each stage. The
                                   ErrorInfo goes into the pattern...
                                   ...somewhere... *)

            fun traverseRecValbind(VE, VE', vb): Substitution * OG.valbind =
              case vb
                of OG.PLAINvalbind(i, pat, exp, vb_opt) =>
                     let val (S, pat') = traverseRecPat(VE, VE', pat)
                         val (S', vb_opt') =
			   case vb_opt
			     of Some vb =>
                                let val (S', vb') = traverseRecValbind (S onVE VE, S onVE VE', vb)
                                in (S' oo S, Some vb')
                                end
			      | None => (S, None)
                     in (S', OG.PLAINvalbind(i, pat', exp, vb_opt'))
                     end

                 | OG.RECvalbind(i, vb) =>
                     let
                       val (S, vb') = traverseRecValbind(VE, VE', vb)
                     in
                       (S, OG.RECvalbind(i, vb'))
                     end

            and traverseRecPat(VE, VE', pat): Substitution * OG.pat =
              case pat
                of OG.ATPATpat(i, atpat) =>
                     let
                       val (S, atpat') = traverseRecAtpat(VE, VE', atpat)
                     in
                       (S, OG.ATPATpat(i, atpat'))
                     end

                 | OG.CONSpat(i, id, atpat) =>
                     let val (S, atpat') = traverseRecAtpat(VE, VE', atpat)
                     in (S, OG.CONSpat(i, id, atpat'))
                     end

                 | OG.TYPEDpat(i, pat, ty) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.TYPEDpat(i, pat', ty))
                     end

                 | OG.LAYEREDpat(i, id as OG.OP_OPT(id', withOp),
                                 ty_opt, pat
                                ) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                       val (S', i') = processID(i, S onVE VE, S onVE VE', id')
                     in
                       (S oo S', OG.LAYEREDpat(i', id, ty_opt, pat'))
                     end
            
                 | OG.UNRES_INFIXpat _ =>
                     impossible "traverseRecPat(UNRES_INFIX)"

            and traverseRecAtpat(VE, VE', atpat): Substitution * OG.atpat =
              case atpat
                of OG.WILDCARDatpat _ => (Substitution.Id, atpat)

                 | OG.SCONatpat _ => (Substitution.Id, atpat)

                 | OG.LONGIDatpat(i, longid_op as OG.OP_OPT(longid, withOp)) =>
		     (case C.lookup_longid C longid of
			Some (VE.LONGCON _) => (Substitution.Id, atpat)
		      | Some (VE.LONGEXCON _) => (Substitution.Id, atpat)
		      | _ => (case Ident.decompose longid of
				([], id) =>
				  let val (S, i') = processID(i, VE, VE', id)
				  in (S, OG.LONGIDatpat(i', longid_op))
				  end
			      | _ => impossible "traverseRecAtpat(longid)"))

                 | OG.RECORDatpat(i, patrowOpt) =>
                     (case patrowOpt of 
                        None => (Substitution.Id,atpat)
                      | Some patrow => 
                          let 
                            val (S, patrow') =
                              traverseRecPatrow (VE, VE', patrow)
                          in
                            (S, OG.RECORDatpat(i, Some patrow'))
                          end)

                 | OG.PARatpat(i, pat) =>
                     let
                       val (S, pat') = traverseRecPat(VE, VE', pat)
                     in
                       (S, OG.PARatpat(i, pat'))
                     end

            and traverseRecPatrow(VE, VE', patrow): Substitution * OG.patrow =
              case patrow of 
                OG.DOTDOTDOT i => (Substitution.Id, patrow)
              | OG.PATROW(i, l, pat, patrowOpt) =>
                  let 
                    val (S, pat') = traverseRecPat(VE, VE', pat)
                    val (S', patrowOpt') =
                      (case patrowOpt of 
                        None => (Substitution.Id, None)
                      | Some patrow => 
                          let 
                            val (S'', patrow') = traverseRecPatrow(VE, VE', patrow)
                          in
                            (S'', Some patrow')
                          end)
                  in
                    (S' oo S, OG.PATROW(i, l, pat', patrowOpt'))
                  end


            (* set up a value environment, VE, for the recursively declared values *)

            val domain_list = dom_vb(C, valbind) 

            fun TypeScheme_fresh () = TypeScheme.from_Type (Type_fresh ())

            fun setup id VE =
	          VE.plus (VE, VE.singleton_var (id, TypeScheme_fresh ()))

            val VE = List.foldL setup VE.empty domain_list
                                (* VE now maps each rec identifier to 'a. *)

                                (* Proceed with type checking. The ErrorInfo
                                   tags for the rec identifiers will be
                                   untouched (I hope, since we might assign
                                   them further on). *)
            val (S, VE', valbind') =
                  elab_valbind (C.plus_VE (C, VE), valbind)

                                (* Post-pass, to patch up the rec identifiers
                                   and plant unification error tags: *)
            val (S', valbind'') =
                  traverseRecValbind (S onVE VE, VE', valbind')

            val VE'' = S' onVE VE'

	    val out_i = (case TyName.Set.list
			        (TyName.Set.difference
				   (VE.tynames VE'') (C.to_T C)) of
			   [] => okConv i
			 | tynames =>
			     errorConv
			       (i,ErrorInfo.DATATYPES_ESCAPE_SCOPE tynames))
          in
	    if !Flags.DEBUG_ELABDEC then
	      pr ("RECvalbind: ", PP.NODE {start="{", finish="}", indent=0,
					   children=[VE.layout VE''],
					   childsep=PP.NONE})
	    else () ;
	    (S' oo S, VE'', OG.RECvalbind (out_i, valbind''))
          end

    (******* type bindings *******)

    and elab_typbind (C : Context, typbind : IG.typbind) : (TyEnv * OG.typbind) =

      case typbind of

        (* Type binding *)                                      (*rule 27*)
        IG.TYPBIND(i, ExplicitTyVars, tycon, ty, typbind_opt) =>
          let
            val _ = Level.push()
            val TyVars =
              map TyVar.from_ExplicitTyVar ExplicitTyVars
            val tyvarsRepeated = getRepeatedElements TyVar.eq  TyVars
            val tyvarsNotInTyVarList =
              List.all 
                (fn tv => not (List.member tv ExplicitTyVars)) 
                (IG.getExplicitTyVarsTy ty)
            val (tau, out_ty) = elab_ty(C, ty)

            val _ = Level.pop()
            val typeFcn = TypeFcn.from_TyVars_and_Type (TyVars, tau)
            val tystr = TyStr.from_theta_and_VE(typeFcn, VE.empty)

            val (TE, out_typbind_opt) = elab_typbind_opt(C, typbind_opt)
              
          in
            if not(isEmptyTyVarList(tyvarsNotInTyVarList)) then
              (TE.plus (TE.singleton(tycon, tystr), TE),
               OG.TYPBIND(errorConv(i, 
                      ErrorInfo.TYVARS_NOT_IN_TYVARSEQ 
                            (map TyVar.from_ExplicitTyVar tyvarsNotInTyVarList)),
                          ExplicitTyVars, tycon, out_ty, out_typbind_opt))
            else
              if (EqSet.member tycon (TE.dom TE)) then 
                (TE.plus (TE.singleton(tycon, tystr), TE),
                 OG.TYPBIND(repeatedIdsError(i, [ErrorInfo.TYCON_RID tycon]),
                            ExplicitTyVars, tycon, out_ty, out_typbind_opt))
              else
                if not(isEmptyTyVarList(tyvarsRepeated)) then
                  (TE.plus (TE.singleton(tycon, tystr), TE),
                   OG.TYPBIND(repeatedIdsError(i,
                                map ErrorInfo.TYVAR_RID tyvarsRepeated),
                              ExplicitTyVars, tycon, out_ty, out_typbind_opt))
                else
                  (TE.plus (TE.singleton(tycon, tystr), TE),
                   OG.TYPBIND(okConv i, ExplicitTyVars, tycon, out_ty, out_typbind_opt))
          end

    and elab_typbind_opt (C : Context, typbind_opt : IG.typbind Option)
      : (TyEnv * OG.typbind Option) =

      case typbind_opt of

        Some(typbind) =>
          let
            val (TE, out_typbind) = elab_typbind(C, typbind)
          in
            (TE, Some out_typbind)
          end

      | None =>
          (TE.empty, None)

    (******* datatype bindings *******)

    and elab_datbind (C : Context, datbind : IG.datbind)
      : ((VarEnv * TyEnv) * OG.datbind) =

      case datbind of

        (* Datatype binding *)                                  (*rule 28*)
        IG.DATBIND(i, ExplicitTyVars, tycon, conbind, datbind_opt) =>
          let
            val _ = Level.push()
            val TyVars = map TyVar.from_ExplicitTyVar ExplicitTyVars
            val tyvarsRepeated = getRepeatedElements TyVar.eq  TyVars
            val tyvarsNotInTyVarList =
              List.all 
                (fn tv => not (List.member tv ExplicitTyVars))
                (IG.getExplicitTyVarsConbind conbind)
            val (typeFcn, _) = 
	      case C.lookup_tycon C tycon of
                Some(tystr) => TyStr.to_theta_and_VE(tystr)
              | None => impossible "datbind(1)"

            val tyname =
              case TypeFcn.to_TyName typeFcn of
                Some(tyname) => tyname
              | None => impossible "datbind(2)"

            val tau_list =
              map Type.from_TyVar TyVars

            val tau =
              Type.from_ConsType (Type.mk_ConsType (tau_list, tyname))

            val (constructor_map : constructor_map,
		 out_conbind) = elab_conbind (C, tau, conbind)
            val _ = Level.pop()
	    val VE = constructor_map.to_VE constructor_map

            val VE_closed = VE.close VE

            val tystr = TyStr.from_theta_and_VE (TypeFcn.from_TyName tyname, VE_closed)

            val ((VE', TE'), out_datbind_opt) = elab_datbind_opt (C, datbind_opt)
	    val out_i =
	          if IG.TyCon.is_'true'_'nil'_etc tycon then
		    errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [])
		  else if IG.TyCon.is_'it' tycon then
		    errorConv (i, ErrorInfo.REBINDING_IT)
		  else if not (isEmptyTyVarList tyvarsNotInTyVarList) then
		    errorConv (i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ 
			       (map TyVar.from_ExplicitTyVar
				  tyvarsNotInTyVarList))
		  else
		    let val repeated_ids_errorinfos =
		      map ErrorInfo.TYVAR_RID tyvarsRepeated
		      @ map ErrorInfo.ID_RID
		          (EqSet.list (EqSet.intersect (VE.dom VE') (VE.dom VE)))
		      @ (if EqSet.member tycon (TE.dom TE')
			 then [ErrorInfo.TYCON_RID tycon] else [])
		    in
		      if repeated_ids_errorinfos = [] then okConv i
		      else repeatedIdsError (i, repeated_ids_errorinfos)
		    end 
          in
	    ( (VE.plus  (VE_closed, VE'),
	       TE.plus (TE.singleton (tycon, tystr), TE')),
  	      OG.DATBIND(out_i, ExplicitTyVars, tycon,
			 out_conbind, out_datbind_opt) )
          end

    and elab_datbind_opt (C : Context, datbind_opt : IG.datbind Option)
      : ((VarEnv * TyEnv) * OG.datbind Option) =

      case datbind_opt of

        Some(datbind) =>
          let
            val ((VE, TE), out_datbind) = elab_datbind(C, datbind)
          in
            ((VE, TE), Some out_datbind)
          end

       | None =>
          ((VE.empty, TE.empty), None)

    (****** constructor bindings *****)

    and elab_conbind (C : Context, tau : Type, conbind : IG.conbind)
      : (constructor_map * OG.conbind) =

      (*I deviate from the definition in letting elab_conbind return
       a mapping from id's (constructors) to TypeScheme's instead of
       a VE, because a VE also contains fellow constructors for each
       constructor, and the fellow constructors cannot be determined
       before the whole conbind has been processed. I let the caller
       of elab_conbind (elab_datbind) turn this map into a VE proper
       19/01/1997 17:02. tho.*)

      case conbind of

        (* Constructor binding *)                               (*rule 29*)
        IG.CONBIND(i, IG.OP_OPT(con, withOp), Some ty, conbind_opt) =>
          let
            val (tau', out_ty) = elab_ty (C, ty)
            val arrow = TypeScheme.from_Type
	                  (Type.mk_Arrow (tau', tau))
            val (constructor_map, out_conbind_opt) =
	          elab_conbind_opt (C, tau, conbind_opt)
          in
	    (constructor_map.add con arrow constructor_map,
	     OG.CONBIND (out_i_for_conbind con constructor_map i,
			 OG.OP_OPT (con, withOp),
			 Some out_ty,
			 out_conbind_opt))
          end

      | IG.CONBIND(i, IG.OP_OPT(con, withOp), None, conbind_opt) =>
          let val (constructor_map, out_conbind_opt) =
	            elab_conbind_opt (C, tau, conbind_opt)
          in
	    (constructor_map.add
	       con (TypeScheme.from_Type tau) constructor_map,
	     OG.CONBIND (out_i_for_conbind con constructor_map i, 
			 OG.OP_OPT(con, withOp), None, out_conbind_opt))
          end

    and out_i_for_conbind con constructor_map i = 
          if constructor_map.in_dom con constructor_map
	  then repeatedIdsError (i, [ErrorInfo.CON_RID con])
	  else if IG.is_'true'_'nil'_etc con
	       then errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [con])
	       else if IG.is_'it' con
		    then errorConv (i, ErrorInfo.REBINDING_IT)
		    else okConv i

    and elab_conbind_opt (C : Context,
			  tau : Type,
			  conbind_opt : IG.conbind Option)
      : (constructor_map * OG.conbind Option) =

      case conbind_opt of

        Some conbind =>
          let
            val (constructor_map, out_conbind) = elab_conbind (C, tau, conbind)
          in
            (constructor_map, Some out_conbind)
          end

      | None => (constructor_map.empty, None)

    (****** exception bindings *****)

    and elab_exbind (C : Context, exbind : IG.exbind)
      : (VarEnv * OG.exbind) =

      case exbind of

        (* Exception binding *)                                 (*rule 30*)
        IG.EXBIND(i, IG.OP_OPT(excon, withOp), Some ty, rest) =>
          let
            val (tau, out_ty) = elab_ty (C, ty)
            val exnTy = Type.mk_Arrow (tau, Type.Exn)
            val VE_this = VE.singleton_excon (excon, exnTy)
            val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
          in 
	    (VE.plus  (VE_this, VE_rest),
	     OG.EXBIND (out_i_for_exbind excon VE_rest i (Some tau), 
		        OG.OP_OPT(excon, withOp), Some out_ty, out_rest))
          end

      | IG.EXBIND(i, IG.OP_OPT(excon, withOp), None, rest) =>
          let
            val VE_this = VE.singleton_excon (excon, Type.Exn)
            val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
          in
	    (VE.plus  (VE_this, VE_rest),
	     OG.EXBIND (out_i_for_exbind excon VE_rest i None, 
		        OG.OP_OPT(excon, withOp), None, out_rest))
          end

        (* Exception binding *)                                 (*rule 31*)
      | IG.EXEQUAL(i, IG.OP_OPT(excon, exconOp),
                      IG.OP_OPT(longid, longidOp), rest) =>
          (case C.lookup_longid C longid of
             Some (VE.LONGEXCON tau) =>
               let
                 val VE_this = VE.singleton_excon (excon, tau)
                 val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
               in 
		 (VE.plus  (VE_this, VE_rest),
		  OG.EXEQUAL (out_i_for_exbind excon VE_rest i None,
			      OG.OP_OPT(excon, exconOp),
			      OG.OP_OPT(longid, longidOp), out_rest))
               end
           | _ => (*Carry on, building an error node.*)
               let val (VE_rest, out_rest) = elab_exbind_opt (C, rest)
               in
                 (VE_rest, OG.EXEQUAL(lookupIdError(i, longid),
				      OG.OP_OPT(excon, exconOp),
				      OG.OP_OPT(longid, longidOp),
				      out_rest))
               end)

    and out_i_for_exbind excon VE_rest i tau_opt =
          if EqSet.member excon (VE.dom VE_rest)
	  then repeatedIdsError (i, [ErrorInfo.EXCON_RID excon])
	  else if IG.is_'true'_'nil'_etc excon
	       then errorConv (i, ErrorInfo.REBINDING_TRUE_NIL_ETC [excon])
	       else if IG.is_'it' excon
		    then errorConv (i, ErrorInfo.REBINDING_IT)
		    else addTypeInfo_EXBIND (okConv i, tau_opt)

    and elab_exbind_opt (C, Some exbind) =
          let val (VE, out_exbind) = elab_exbind (C, exbind)
          in
            (VE, Some out_exbind)
          end

      | elab_exbind_opt(C, None) = (VE.empty, None)

    (****** atomic patterns ******)

    and elab_atpat (C : Context, atpat : IG.atpat) :
        (Substitution * (VarEnv * Type) * OG.atpat) =

        case atpat of

          (* Wildcard *)                                        (*rule 32*)
          IG.WILDCARDatpat i  =>
            (Substitution.Id, 
             (VE.empty, Type_fresh ()),
              OG.WILDCARDatpat(okConv i))

          (* Special constant *)                                (*rule 33*)
        | IG.SCONatpat(i,scon) =>
            (Substitution.Id,
             (VE.empty, Type.of_scon scon),
              OG.SCONatpat(okConv i,scon))

          (* Long identifier *)                                 (*rule 34*)
        | IG.LONGIDatpat(i, IG.OP_OPT(longid, withOp)) =>
            (case C.lookup_longid C longid of
	       Some(VE.LONGCON sigma) =>          (* rule 36 *)
                    let
                      fun isConsType tau =
                            (case Type.to_ConsType tau of
			       Some _ => true
			     | None => false)

                      val (tau,instances) = TypeScheme.instance'' sigma
                      
                      val (tau', i') =
                        if isConsType tau then
                          (tau, okConv i)
                        else
                          (Type_bogus (),
                           errorConv(i, ErrorInfo.NOTCONSTYPE tau)
                          )
                    in
                      (Substitution.Id,
                       (VE.empty, tau'),
		       OG.LONGIDatpat(addTypeInfo_CON(i', C, Type.is_Arrow tau', 
						      tau',instances,longid),
				      OG.OP_OPT(longid, withOp)))
                    end

                | Some(VE.LONGEXCON tau) =>
                    let
                      val exnType = Type.Exn
                      val (_, i') = UnifyWithTexts
			              ("expected long excon in pattern to have type",
				       exnType, "but found it to have type", tau,i)
                    in
                      (Substitution.Id,
                       (VE.empty, exnType),
                       OG.LONGIDatpat(addTypeInfo_EXCON(i',exnType,longid),
                                      OG.OP_OPT(longid, withOp)))
                    end

                | _ =>          (* make new variable environment *) 
                                (* unbound long identifier *)
                    let
                      val tau = Type_fresh ()
                      val tau_scheme = TypeScheme.from_Type tau
                    in
                      case Ident.decompose longid
                        of (nil, id) =>
                             (Substitution.Id,
                              (VE.singleton_var(id, tau_scheme), tau),
                              OG.LONGIDatpat(addTypeInfo_VAR_PAT(okConv i,
                                                                 tau),
                                             OG.OP_OPT(longid, withOp)
                                            )
                             )

                         | (_, _) =>
                             (Substitution.bogus,
                              (VE.bogus, Type_bogus ()),
                              OG.LONGIDatpat(
                                errorConv(i, ErrorInfo.QUALIFIED_ID longid),
                                OG.OP_OPT(longid, withOp)
                              )
                             )
                    end
            )

          (* Record pattern *)                                  (*rule 36*)
        | IG.RECORDatpat(i, row_opt as None) =>
            (Substitution.Id,
             (VE.empty, Type.Unit),
              OG.RECORDatpat(okConv i, None))

        | IG.RECORDatpat(i, row_opt as Some patrow) =>
            let
              val (S, (VE, rho), out_patrow) = elab_patrow(C, patrow)
            in
              (S, 
               (VE,Type.from_RecType rho),
                OG.RECORDatpat(addTypeInfo_RECORD_ATPAT(okConv i, 
                                                        Type.from_RecType rho),
		               Some(addLabelInfo(rho, out_patrow)))) 
            end

          (* Parenthesised pattern *)                           (*rule 37*)
        | IG.PARatpat(i, pat) =>
            let val (S, (VE,tau), out_pat) = elab_pat(C, pat)
            in (S, (VE,tau), OG.PARatpat(okConv i,out_pat)) end

    (****** pattern rows ******)

    and elab_patrow (C : Context, patrow: IG.patrow)
          : (Substitution * (VarEnv * RecType) * OG.patrow) =
      case patrow of

           (* Pattern row *)                                    (*rule 39*)
           IG.PATROW(i, lab, pat, None) =>
             let
               val (S, (VE, tau), out_pat) = elab_pat(C, pat)
             in
               (S, (VE, Type.RecType.add_field (lab, tau) Type.RecType.empty),
                OG.PATROW(okConv i, lab, out_pat, None)
               )
             end

         | IG.PATROW(i, lab, pat, Some patrow) =>
             let
               val (S, (VE, tau), out_pat) = elab_pat(C, pat)
               val (S', (VE', rho), out_patrow) = elab_patrow(C, patrow)
               val intdom = EqSet.intersect (VE.dom VE) (VE.dom VE')
             in
               case (EqSet.isEmpty intdom, 
                     List.member lab (Type.RecType.sorted_labs rho)) of
                 (true, false) =>
                   (S' oo S,
                    (VE.plus (VE, VE'),
                     Type.RecType.add_field (lab, tau) rho
                     ), OG.PATROW(okConv i, lab, out_pat, Some out_patrow)
                    )
               | (true, true) => 
                   (Substitution.bogus,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i,[ErrorInfo.LAB_RID lab]),
                              lab, out_pat, Some out_patrow))
               | (false, false) =>
                   (Substitution.bogus,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i, 
                                  map ErrorInfo.ID_RID (EqSet.list intdom)),
                              lab, out_pat, Some out_patrow))
               | (false, true) => 
                   (Substitution.bogus,
                    (VE', rho),
                    OG.PATROW(repeatedIdsError(i, 
                               (map ErrorInfo.ID_RID (EqSet.list intdom)) @
                               [ErrorInfo.LAB_RID lab]),
                              lab, out_pat, Some out_patrow))
             end

        | IG.DOTDOTDOT i => (* Flexible record treatment... *)  (*rule 38*)
            let 
              val rho = Type.RecType.dotdotdot ()
            in
              (Substitution.Id,
               (VE.empty, rho),
               OG.DOTDOTDOT(preOverloadingConv(i,
                   OverloadingInfo.UNRESOLVED (Type.from_RecType rho))))
            end

    (****** patterns - Definition, p. ? ******)

    and elab_pat (C : Context, pat : IG.pat)
      : (Substitution * (VarEnv * Type) * OG.pat) =
      let
        val _ =
          if !Flags.DEBUG_ELABDEC then
            pr("elab_pat: ", IG.layoutPat pat)
          else ()

        val (S, (VE, ty), pat') = elab_pat'(C, pat)

        val _ =
          if !Flags.DEBUG_ELABDEC then
            let 
              val t = PP.NODE{start="{", finish="}", indent=0,
                              children=[VE.layout VE,
                                        Type.layout ty
                                       ],
                              childsep=PP.RIGHT "; "
                             }
            in
              pr("giving:   ", t)
            end
              else ()
      in
        (S, (VE, ty), pat')
      end

    and elab_pat'(C, pat) =
        case pat of

          (* Atomic pattern *)                                  (*rule 40*)
          IG.ATPATpat(i, atpat) =>
            let val (S, (VE,tau), out_atpat) = elab_atpat(C, atpat)
            in (S, (VE,tau), OG.ATPATpat(okConv i,out_atpat)) end

          (* Constructed pattern *)                             (*rule 41*)
        | IG.CONSpat(i, IG.OP_OPT(longid, withOp), atpat) =>
            let
              val (S, (VE,tau'), out_atpat) = elab_atpat(C, atpat)
            in
              case C.lookup_longid C longid of

                Some(VE.LONGCON sigma) =>
                  let
                    val new = Type_fresh ()
                    val arrow = Type.mk_Arrow(tau', new) 
                    val (tau1,instances) = TypeScheme.instance'' sigma
                    val (S1, i') = UnifyWithTexts("argument to long value constructor \
		                                  \in pattern suggests constructor type",
		                                  arrow, 
						  "but constructor has type", tau1, i)
                    val tau2 = S1 on new
                  in
                    (S1 oo S, (S1 onVE VE, tau2),
                     OG.CONSpat(addTypeInfo_CON(i', C, true,
                                                (S1 on arrow),instances,
                                                longid),
                                OG.OP_OPT(longid, withOp),
                                out_atpat
                               )
                    )
                  end

              | Some(VE.LONGEXCON tau) =>
                  let
                    val arrow = Type.mk_Arrow(tau',Type.Exn)
                    val (S1, i') = UnifyWithTexts("argument to long \
                          \exception constructor in pattern requires exception \
			  \constructor type ", arrow, 
                          "but the exception constructor has type", tau, i)
                  in
                    (S1 oo S,
                     (S1 onVE VE,Type.Exn),
                     OG.CONSpat(addTypeInfo_EXCON(i',S1 on arrow,longid),
                                OG.OP_OPT(longid, withOp),
                                    out_atpat
                               )
                    )
                  end

(***KEVIN's idea for expressions as patterns:

              | Some(VE.LONGVAR sigma) =>
                  let
                    val new = Type_fresh ()
                    val arrow = Type.mk_Arrow(new, tau')
                    val tau = TypeScheme.instance sigma
                    val (S1, i') = Unify(arrow,tau,i)
                  in
                    (S1 oo S,
                    (S1 onVE VE, S1 on new),
                     OG.CONSpat(i', OG.OP_OPT(longid, withOp),
                                    out_atpat
                               )
                    )
                  end

***)

              | _ => (* Mark the error. *)
                  (Substitution.bogus,
                   (VE, Type_bogus ()),
                   OG.CONSpat(lookupIdError(i, longid),
                              OG.OP_OPT(Ident.bogus, withOp),
                              out_atpat
                             )
                  )
            end

          (* Typed pattern *)                                   (*rule 42*)
        | IG.TYPEDpat(i, pat, ty) =>
            let
              val (S, (VE,tau), out_pat) = elab_pat(C, pat)
              val (tau', out_ty) = elab_ty(C, ty)
              val (S', i') = UnifyWithTexts("pattern has type", tau, "which conflicts\
                       \ with your type constraint", tau', i)
              val S'' = S' oo S
            in
              (S'',
               (S'' onVE VE, S'' on tau), OG.TYPEDpat(i', out_pat, out_ty))
            end

          (* Layered pattern *)                                 (*rule 43*)
        | IG.LAYEREDpat(i, IG.OP_OPT(id, withOp), None, pat) =>
            let
              val (S, (VE1, tau), out_pat) = elab_pat(C, pat)
              val VE2 = VE.singleton_var(id, TypeScheme.from_Type tau)
              val intdom = EqSet.intersect (VE.dom VE1) (VE.dom VE2)
              val VE3 = VE.plus (VE1, VE2)
            in
              if EqSet.isEmpty intdom then
                (S, (VE3, tau), 
                 OG.LAYEREDpat(addTypeInfo_VAR_PAT(okConv i,tau), 
                               OG.OP_OPT(id, withOp),
                               None, out_pat))
              else
                (S, (VE3, tau),
                 OG.LAYEREDpat(repeatedIdsError(i, map ErrorInfo.ID_RID 
                                                   (EqSet.list intdom)),
                               OG.OP_OPT(id, withOp),
                               None, out_pat))
            end

        | IG.LAYEREDpat(i, IG.OP_OPT(id, withOp), Some ty, pat) =>
            let
              val (S, (VE1, tau), out_pat) = elab_pat(C, pat)
              val (tau', out_ty) = elab_ty(C, ty)
              val (S', i') = UnifyWithTexts("pattern has type", tau, 
                              "which conflicts with your constraint", tau', i)
              val i' = addTypeInfo_VAR_PAT(i', tau') (*added, mads*)
              val S'' = S' oo S
              val VE2 = VE.singleton_var (id, TypeScheme.from_Type tau)
              val intdom = EqSet.intersect (VE.dom VE1) (VE.dom VE2)
              val VE3 = VE.plus (VE1, VE2)
            in
              if EqSet.isEmpty intdom then
                (S'',
                 (S'' onVE VE3, S'' on tau),
                 OG.LAYEREDpat(i', OG.OP_OPT(id, withOp), Some out_ty, out_pat)
                 )
              else
                (S'', 
                 (S'' onVE VE3, S'' on tau),
                 OG.LAYEREDpat(repeatedIdsError(i, map ErrorInfo.ID_RID (EqSet.list intdom)),
                               OG.OP_OPT(id, withOp), Some out_ty, out_pat)
                 )
            end

        | IG.UNRES_INFIXpat _ =>
            impossible "elab_pat(UNRES_INFIX)"

    (****** types  ******)

    and elab_ty (C : Context, ty : IG.ty) : (Type * OG.ty) =

        case ty of

          (* Explicit type variable *)                          (*rule 44*)
          IG.TYVARty(i, ExplicitTyVar) =>
            (Type.from_TyVar' 
               (C.ExplicitTyVarEnv_lookup (C.to_U' C) ExplicitTyVar)
               (TyVar.from_ExplicitTyVar ExplicitTyVar),
             OG.TYVARty(okConv i, ExplicitTyVar))

          (* Record type *)                                     (*rule 45*)
        | IG.RECORDty(i, None) =>
            (Type.Unit, OG.RECORDty (okConv i, None))

          (* Record type *)
        | IG.RECORDty(i, Some tyrow) =>
            let
              val (rho, out_tyrow) = elab_tyrow(C, tyrow)
            in
              (Type.from_RecType rho,
               OG.RECORDty(okConv i, Some out_tyrow))
            end

        (* Constructed type *)                                  (*rule 46*)
        | IG.CONty(i, ty_list, longtycon) =>
            let
              val res_list    = map (fn ty => elab_ty (C, ty)) ty_list
              val tau_list    = map #1 res_list
              val out_ty_list = map #2 res_list
            in
              case C.lookup_longtycon C longtycon of
		Some tystr =>
                     let
                       val (typeFcn, _) = TyStr.to_theta_and_VE tystr
                       val expectedArity = TypeFcn.arity typeFcn
                       val actualArity = List.size tau_list
                       val _ = debug_pr_msg "elab_ty(CONty)"
(*debug
		       val _ = pr ("elab_ty(CONty) - TypeFcn = ", TypeFcn.layout typeFcn)
debug*)
                     in
                       if expectedArity = actualArity then
                         (TypeFcn.apply (typeFcn, tau_list),
                          OG.CONty (okConv i, out_ty_list, longtycon))
                       else
                         (Type_bogus (),
                          OG.CONty(errorConv(i, ErrorInfo.WRONG_ARITY{
                                                  expected=expectedArity,
                                                  actual=actualArity
                                                }
                                            ),
                                   out_ty_list, longtycon
                                  )
                         )
                     end

                 | None =>
                       (Type_bogus (),
                        OG.CONty(
                          lookupTyConError(i, longtycon),
                          out_ty_list, longtycon
                        )
                       )
            end

          (* Function type *)                                   (*rule 47*)
        | IG.FNty(i, ty, ty') =>
            let
              val (tau , out_ty ) = elab_ty(C, ty )
              val (tau', out_ty') = elab_ty(C, ty')
            in
              (Type.mk_Arrow (tau, tau'),
               OG.FNty(okConv i, out_ty, out_ty'))
            end

          (* Parenthesised type *)                              (*rule 48*)
        | IG.PARty(i, ty) =>
            let
              val (tau, out_ty) = elab_ty(C, ty)
            in
              (tau, OG.PARty(okConv i, out_ty))
            end

    (****** type rows ******)

    and elab_tyrow (C : Context, tyrow : IG.tyrow)
      : (RecType * OG.tyrow) =

        case tyrow of

          (* Type row *)                                        (*rule 49*) 
          IG.TYROW(i, lab, ty, None) =>
            let
              val (tau, out_ty) = elab_ty(C, ty)
              val rho = Type.RecType.add_field (lab,tau) Type.RecType.empty
            in
              (rho, OG.TYROW(okConv i, lab, out_ty, None))
            end

          (* Type row *)
        | IG.TYROW(i, lab, ty, Some tyrow) =>
            let
              val (tau, out_ty) = elab_ty(C, ty)
              val (rho, out_tyrow) = elab_tyrow(C, tyrow)
            in
              if (List.member lab (Type.RecType.sorted_labs rho)) then
                (rho,
                 OG.TYROW(repeatedIdsError(i, [ErrorInfo.LAB_RID lab]),
                          lab, out_ty, Some out_tyrow))
              else
                (Type.RecType.add_field (lab,tau) rho,
                 OG.TYROW(okConv i, lab, out_ty, Some out_tyrow))
            end
          
(**** Overloading resolution ****)  

fun resolve_overloading (S : Substitution, dec : OG.dec): OG.dec =

    (* resolves overloading in dec, by applying S on every recorded
       overloaded type variable --- if repeated application of S 
       yields int or real, we record this information in the info
       field; otherwise overloading cannot be resolved
       and error-info is inserted in the info-field. 
     *)

let
  open OG 

  local
  val tau_to_overloadinginfo_alist =
        [(Type.Int,    OverloadingInfo.RESOLVED_INT),
	 (Type.Real,   OverloadingInfo.RESOLVED_REAL),
	 (Type.String, OverloadingInfo.RESOLVED_STRING),
	 (Type.Char,   OverloadingInfo.RESOLVED_CHAR),
	 (Type.Word,   OverloadingInfo.RESOLVED_WORD)]

  (*tau_to_overloadinginfo raises List.First _*)
  fun tau_to_overloadinginfo tau  =
        #2 (List.first (fn (tau', oi) => Type.eq (tau, tau'))
	      tau_to_overloadinginfo_alist)

  in
  (*resolve_tau gives OverloadingInfo.RESOLVED_INT when overloading couldn't be
   resolved.  According to the definition (p. 72), int is the default type
   except for /, but / is not overloaded in this compiler; / always has type
   real * real -> real, as there is only one kind of real.
   25/06/1997 10:30. tho.*)

  fun resolve_tau (typ : Type) : OverloadingInfo.OverloadingInfo =
        let val typ' = S on typ
	in
	  if !Flags.DEBUG_ELABDEC then
	    (pr("res: tv is: ", Type.layout typ);
	     pr("res:  S on tv yields type: ", Type.layout typ'))
	  else ();
	  (case Type.to_TyVar typ' of
	     None => (tau_to_overloadinginfo typ'
		      handle List.First _ => OverloadingInfo.RESOLVED_INT)
		 (*TODO 25/06/1997 10:11. tho.
		  can raise List.First _ occur?  I'd rather do an impossible
		  here:  If typ' is not a tyvar, it must be one of int, real,
		  string, char, & word; everything else would be a type
		  error.  Well, perhaps it can occur then, namely when there
		  is a type error (they do occur), and since type errors
		  should not make the compiler crash, it is probably best to
		  not do an impossible.  The only thing to do is then to
		  return RESOLVED_INT, as unresolved overloading should not
		  result in an error message.*)
	   | Some tv' =>
	       if Type.eq  (typ',typ)
	       then (if !Flags.DEBUG_ELABDEC
		     then output (std_out, "res: Some tv\n") else () ; 
		     OverloadingInfo.RESOLVED_INT)
	       else resolve_tau typ')    (* Repeat application of S *)
	end
  end (*local*)

  datatype flexresResult = FLEX_RESOLVED | FLEX_NOTRESOLVED
  fun flexrecres(typ : Type) : flexresResult =
        let
	  fun loop typ = 
	        let val typ' = S on typ
		in
		  if !Flags.DEBUG_FLEXRECORDS then 
		    (pr("flexrecres: typ = ", Type.layout typ);
		     pr("flexrecres: typ' = ", Type.layout typ'))
		  else ();
		  if Type.eq (typ',typ) then typ else loop typ'
		end
	in
	  if Type.existsRecVarsType (loop typ) then FLEX_NOTRESOLVED
	  else FLEX_RESOLVED
	end

  local
    open TypeInfo 
    infix on_repeated on_repeated_TypeScheme on_TypeInfo

    fun S on_repeated tau =
          (if !Flags.DEBUG_ELABDEC then
	     pr ("on_repeated: tau = ", Type.layout tau) else ();
	   let val tau' = S on tau
	   in if Type.eq (tau',tau) then tau' else S on_repeated tau'
	   end)

    fun S on_repeated_TypeScheme sigma =
      let val sigma' = Substitution.onScheme (S,sigma)
      in
        if TypeScheme.eq (sigma',sigma) then sigma' 
        else S on_repeated_TypeScheme sigma'
      end

    fun S on_TypeInfo (LAB_INFO {index,Type,tyvars}) =
            LAB_INFO {index=index,Type=S on_repeated Type,tyvars=tyvars}
      | S on_TypeInfo (RECORD_ATPAT_INFO {Type}) = 
	    RECORD_ATPAT_INFO {Type=S on_repeated Type}
      | S on_TypeInfo (VAR_INFO {instances}) = 
	    VAR_INFO {instances=map (fn tau => S on_repeated tau) instances}
      | S on_TypeInfo (VAR_PAT_INFO {tyvars,Type}) =
	    VAR_PAT_INFO {tyvars=tyvars,Type=S on_repeated Type}
      | S on_TypeInfo (CON_INFO {numCons,index,tyvars,Type,longid,instances}) = 
	    CON_INFO {numCons=numCons,index=index,tyvars=tyvars,
		      Type=S on_repeated Type,longid=longid,
		      instances= map (fn tau => S on_repeated tau) instances}
      | S on_TypeInfo (EXCON_INFO {Type,longid}) = 
	    EXCON_INFO {Type=S on_repeated Type,longid=longid}
      | S on_TypeInfo (EXBIND_INFO {TypeOpt=None}) = EXBIND_INFO {TypeOpt=None}
      | S on_TypeInfo (EXBIND_INFO {TypeOpt=Some Type}) = 
	    EXBIND_INFO {TypeOpt=Some (S on_repeated Type)}   
      | S on_TypeInfo (DATBIND_INFO {TE}) = DATBIND_INFO {TE=TE}  (*MEMO...*)
      | S on_TypeInfo (EXP_INFO {Type}) = 
	    EXP_INFO {Type=S on_repeated Type}
      | S on_TypeInfo (MATCH_INFO {Type}) = 
	    MATCH_INFO {Type=S on_repeated Type}
      | S on_TypeInfo (PLAINvalbind_INFO {tyvars,escaping,Type}) =
	    PLAINvalbind_INFO {tyvars=tyvars, escaping=escaping,
			       Type=S on_repeated Type}
  in
    fun resolve_i ElabInfo =
          (case ElabInfo.to_TypeInfo ElabInfo of
	     Some typeinfo =>
	       ElabInfo.plus_TypeInfo ElabInfo (S on_TypeInfo typeinfo)
	   | None => ElabInfo)

  end (*local open TypeInfo ...*)

  (*resolve_X X: apply resolve_i to all info fields i in X and resolve_tau to
   all overloadinginfos on id's in X, and also do something about flex
   records.*)

  fun resolve_atexp (atexp : atexp) : atexp =
      case atexp of
          SCONatexp _ => atexp
        | IDENTatexp(i, op_opt) =>
              (case ElabInfo.to_OverloadingInfo i of 
                   None => IDENTatexp (resolve_i i, op_opt)
                 | Some (OverloadingInfo.UNRESOLVED typ) =>
		     IDENTatexp
		       (ElabInfo.plus_OverloadingInfo i (resolve_tau typ), 
			op_opt)
                 | Some _ => impossible "resolve_atexp")
        | RECORDatexp(i, None) => RECORDatexp(resolve_i i,None)
        | RECORDatexp(i, Some exprow) =>
              RECORDatexp(resolve_i i, Some (resolve_exprow exprow))
        | LETatexp(i, dec, exp) =>
              LETatexp(resolve_i i, resolve_dec dec, resolve_exp exp)
        | PARatexp(i, exp) =>
              PARatexp(resolve_i i, resolve_exp exp)
              
  and resolve_exprow (exprow: exprow) : exprow =
      case exprow of 
          EXPROW(i, l, exp, None) =>
              EXPROW(resolve_i i, l, resolve_exp exp, None)
        | EXPROW(i, l, exp, Some exprow) =>
              EXPROW(resolve_i i, l, resolve_exp exp, Some (resolve_exprow exprow))
              
  and resolve_exp (exp: exp) : exp =
      case exp of
          ATEXPexp(i, atexp) => 
              ATEXPexp(resolve_i i, resolve_atexp atexp)
        | APPexp(i, exp, atexp) => 
              APPexp(resolve_i i, resolve_exp exp, resolve_atexp atexp)
        | TYPEDexp(i, exp, ty) =>
              TYPEDexp(resolve_i i, resolve_exp exp, ty)
        | HANDLEexp(i, exp, match) =>
              HANDLEexp(resolve_i i, resolve_exp exp, resolve_match match)
        | RAISEexp(i, exp) => 
              RAISEexp(resolve_i i, resolve_exp exp)
        | FNexp(i, match) =>
              FNexp(resolve_i i, resolve_match match)
        | UNRES_INFIXexp _ =>
              impossible "resolve_exp(UNRES_INFIX)"

  and resolve_match (match: match) : match =
      case match of 
          MATCH(i, mrule, None) => 
              MATCH(resolve_i i, resolve_mrule mrule, None)
        | MATCH(i, mrule, Some match) =>
              MATCH(resolve_i i, resolve_mrule mrule, Some (resolve_match match))

  and resolve_mrule (MRULE(i, pat, exp) : mrule) : mrule =
      MRULE(resolve_i i, resolve_pat pat, resolve_exp exp)
      
  and resolve_dec (dec : dec) : dec =
        (case dec of 
	   VALdec(i, tyvars, valbind) =>
	     VALdec(resolve_i i, tyvars, resolve_valbind valbind)
	 | UNRES_FUNdec _ => impossible "resolve_dec(UNRES_FUNdec)"
	 | TYPEdec _ => dec
	 | DATATYPEdec(i,datbind) => DATATYPEdec(resolve_i i,datbind)
	 | DATATYPE_REPLICATIONdec(i, tycon, longtycon) => 
	     DATATYPE_REPLICATIONdec(resolve_i i, tycon, longtycon)
	 | ABSTYPEdec(i, datbind, dec) =>
	     ABSTYPEdec(resolve_i i, datbind, resolve_dec dec)
	 | EXCEPTIONdec(i,exbind) => EXCEPTIONdec(resolve_i i, exbind)
	 | LOCALdec(i, dec1, dec2) =>
	     LOCALdec(resolve_i i, resolve_dec dec1, resolve_dec dec2)
	 | OPENdec _ => dec
	 | SEQdec(i, dec1, dec2) =>
	     SEQdec(resolve_i i, resolve_dec dec1, resolve_dec dec2)
	 | INFIXdec _ => dec
	 | INFIXRdec _ => dec
	 | NONFIXdec _ => dec
	 | EMPTYdec _ => dec)

  and resolve_valbind (valbind : valbind) : valbind =
      case valbind of
          PLAINvalbind(i, pat, exp, None) =>
              PLAINvalbind(i, resolve_pat pat, resolve_exp exp, None)
        | PLAINvalbind(i, pat, exp, Some valbind) =>
              PLAINvalbind(i, resolve_pat pat, 
                           resolve_exp exp, Some (resolve_valbind valbind))
        | RECvalbind(i, valbind) =>
              RECvalbind(i, resolve_valbind valbind)
      
  and resolve_atpat (atpat : atpat) : atpat =
    case atpat of
      WILDCARDatpat _ => atpat
    | SCONatpat _ => atpat
    | LONGIDatpat(i,x) => LONGIDatpat(resolve_i i,x)
    | RECORDatpat(i, None) => RECORDatpat(resolve_i i,None)
    | RECORDatpat(i, Some patrow) =>
        let
          val i' = resolve_i i 
          val patrow' = resolve_patrow patrow
        in
          case ElabInfo.to_TypeInfo i' of
            Some typeinfo =>
              (case typeinfo of
                 TypeInfo.RECORD_ATPAT_INFO{Type} => 
                   (* Type has been resolved, c.f. i' *)
                   RECORDatpat(i',Some (addLabelIndexInfo(Type,patrow')))
               | _ => impossible ("resolve_atpat(RECORDatpat): " ^ 
                                        "wrong typeinfo"))
          | None => impossible ("resolve_atpat(RECORDatpat): " ^ 
                                      "no typeinfo")
        end
    | PARatpat(i, pat) =>
        PARatpat(resolve_i i, resolve_pat pat)

  and resolve_patrow (patrow : patrow): patrow  =
    case patrow of
      DOTDOTDOT(i) => 
        (case (ElabInfo.to_OverloadingInfo i) of 
           None => patrow
         | Some (OverloadingInfo.UNRESOLVED typ) =>
             (case flexrecres typ of
                FLEX_RESOLVED => 
                  DOTDOTDOT (ElabInfo.remove_OverloadingInfo i)
              | FLEX_NOTRESOLVED =>
                  DOTDOTDOT
		    (ElabInfo.plus_ErrorInfo 
		       i ErrorInfo.FLEX_REC_NOT_RESOLVED))
         | Some _ => impossible "resolve_patrow")
    | PATROW(i, lab, pat, None) => 
        PATROW(resolve_i i, lab, resolve_pat pat, None)
    | PATROW(i, lab, pat, Some patrow) =>
        PATROW(resolve_i i, lab, resolve_pat pat, Some (resolve_patrow patrow))

  and resolve_pat (pat : pat) : pat =
    case pat of
      ATPATpat(i, atpat) =>
        ATPATpat(resolve_i i, resolve_atpat atpat)
    | CONSpat(i, longidopt, atpat) =>
        CONSpat(resolve_i i, longidopt, resolve_atpat atpat)
    | TYPEDpat(i, pat, ty) =>
        TYPEDpat(resolve_i i, resolve_pat pat, ty)
    | LAYEREDpat(i, idopt, tyopt, pat) =>
        LAYEREDpat(resolve_i i, idopt, tyopt, resolve_pat pat)
    | UNRES_INFIXpat _ =>
        impossible "resolve_pat(UNRES_INFIX)"


in
  resolve_dec dec
end (*fun resolve_overloading (ugly)*)

    (****** Elaborate a declaration and resolve overloading ******)

    val elab_dec : (Context * IG.dec) -> (Env * OG.dec) =

      fn (C, dec) =>
        let
          val (S, E, out_dec) = elab_dec(C, dec)
          val dec' = resolve_overloading (S, out_dec)
        in
          (E, dec')
        end

end; 
