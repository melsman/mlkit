(* Error information *)

(*$ErrorInfo: STATOBJECT MODULE_STATOBJECT IDENT LAB TYCON TYNAME
        SIGID STRID FUNID REPORT PRETTYPRINT ERROR_INFO*)

functor ErrorInfo(structure StatObject : STATOBJECT
		  structure ModuleStatObject : MODULE_STATOBJECT
		    sharing ModuleStatObject.TyName = StatObject.TyName
		    sharing type ModuleStatObject.TyVar = StatObject.TyVar
		        and type ModuleStatObject.Type = StatObject.Type
			and type ModuleStatObject.TypeScheme = StatObject.TypeScheme
			and type ModuleStatObject.TypeFcn = StatObject.TypeFcn
                  structure Ident: IDENT
		    sharing type Ident.id = ModuleStatObject.id
                  structure Lab:   LAB
                  structure TyCon: TYCON
		    sharing type TyCon.longtycon = ModuleStatObject.longtycon
                  structure SigId: SIGID
                  structure StrId: STRID
		    sharing type StrId.strid = ModuleStatObject.strid
		        and type StrId.longstrid = ModuleStatObject.longstrid
                  structure FunId: FUNID
                  structure Report: REPORT
		  structure PrettyPrint : PRETTYPRINT
		    sharing type StatObject.StringTree = PrettyPrint.StringTree
		    ) : ERROR_INFO =
  struct
    (*import from StatObject:*)
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure TypeFcn      = StatObject.TypeFcn
    structure Realisation  = StatObject.Realisation
    type Type              = StatObject.Type
    type TypeScheme        = StatObject.TypeScheme
    type RecType           = StatObject.RecType
    type TyVar             = StatObject.TyVar
    type TyName            = TyName.TyName
    type TypeFcn           = StatObject.TypeFcn
    type SigMatchError     = ModuleStatObject.SigMatchError

    (*import from other modules:*)
    type id                = Ident.id
    type longid            = Ident.longid
    type tycon             = TyCon.tycon
    type longtycon         = TyCon.longtycon
    type lab               = Lab.lab
    type sigid             = SigId.sigid
    type strid             = StrId.strid
    type longstrid         = StrId.longstrid
    type funid             = FunId.funid

    val StringTree_to_string = PrettyPrint.flatten1

    datatype RepeatedId = ID_RID of id      (* Repeated identifier, syntax *)
                        | LAB_RID of lab    (* errors *)
                        | TYCON_RID of tycon
                        | EXCON_RID of id
                        | CON_RID of id
                        | TYVAR_RID of TyVar
                        | STRID_RID of strid
                        | SIGID_RID of sigid
                        | FUNID_RID of funid

    fun pr_repeatedId (ID_RID id) = Ident.pr_id id
      | pr_repeatedId (LAB_RID lab) = Lab.pr_Lab lab
      | pr_repeatedId (TYCON_RID tycon) = TyCon.pr_TyCon tycon
      | pr_repeatedId (EXCON_RID id) = Ident.pr_id id
      | pr_repeatedId (CON_RID id) = Ident.pr_id id
      | pr_repeatedId (TYVAR_RID tyvar) = TyVar.string tyvar
      | pr_repeatedId (STRID_RID strid) = StrId.pr_StrId strid
      | pr_repeatedId (SIGID_RID sigid) = SigId.pr_SigId sigid
      | pr_repeatedId (FUNID_RID funid) = FunId.pr_FunId funid

    datatype ErrorInfo =
     (* Core errors: *)
        UNIFICATION of Type * Type
      | UNIFICATION_TEXT of string * Type * string * Type
      | LOOKUP_LONGID of longid
      | LOOKUP_LONGTYCON of longtycon
      | NOTCONSTYPE of Type
      | QUALIFIED_ID of longid
      | UNGUARDED_TYVARS of TyVar list
      | UNGENERALISABLE_TYVARS of id list
      | WRONG_ARITY of {expected: int, actual: int}
      | FLEX_REC_NOT_RESOLVED 
      | REPEATED_IDS of RepeatedId list
      | TYVARS_NOT_IN_TYVARSEQ of TyVar list
      | DATATYPES_ESCAPE_SCOPE of TyName list
      | TYVARS_SCOPED_TWICE of TyVar list
      | REBINDING_TRUE_NIL_ETC of id list
      | REBINDING_IT

     (* General module errors: *)
      | SPECIFYING_TRUE_NIL_ETC of id list
      | SPECIFYING_IT
      | LOOKUP_SIGID of sigid
      | LOOKUP_LONGSTRID of longstrid
      | LOOKUP_FUNID of funid
      | EXDESC_SIDECONDITION
      | SHARING_TYPE_NOT_TYNAME of longtycon * TypeFcn
      | SHARING_TYPE_RIGID of longtycon * TyName
      | SHARING_TYPE_ARITY of TyName list
      (*the following four errors come from rule 64, Definition 1997:*)
      | WHERE_TYPE_NOT_WELLFORMED of longtycon * TyName * Type
      | WHERE_TYPE_EQTYPE of longtycon * TyName * Type
      | WHERE_TYPE_RIGID of longtycon * TyName
      | WHERE_TYPE_NOT_TYNAME of longtycon * TypeFcn * Type
      | WHERE_TYPE_ARITY of TyVar list * (longtycon * TyName)

      (* Signature matching errors: *)
      | SIGMATCH_ERROR of SigMatchError

     (* Module unification errors: *)
      | CYCLE of longstrid
      | U_RIGIDTYCLASH of longtycon * longtycon
      | TYPESTRILLFORMEDNESS of longtycon * longtycon
      | U_CONFLICTING_DOMCE of longtycon * longtycon
      | U_CONFLICTINGARITY of longtycon * longtycon
      | RIGIDTYFUNEQERROR of longtycon * longtycon

    type Report = Report.Report
    val line = Report.line
    infix //
    val op // = Report.//

    fun prStrIds strids =
          (case List.foldR
                  (fn strid => fn str =>
		   (case str of
		      "" => StrId.pr_StrId strid
		    | _ => StrId.pr_StrId strid ^ "." ^ str))
		      "" strids of
	     "" => ""
	   | x => x ^ ".")

   (* A lot of the module unification errors carry the same argument types: *)

    fun pr_UnifyArgs (longtycon1, longtycon2) =
          TyCon.pr_LongTyCon longtycon1 ^ ", " ^ TyCon.pr_LongTyCon longtycon2

    fun maybe_plural_s [x] = " "
      | maybe_plural_s xs = "s "

    fun pp_list pp_x [] = ""
      | pp_list pp_x [x] = pp_x x
      | pp_list pp_x [x,x'] = pp_x x ^ " and " ^ pp_x x'
      | pp_list pp_x (x::xs) = pp_x x ^ ", " ^ pp_list pp_x xs

    fun quote s = "`" ^ s ^ "'"

    fun longtycon_and_tyname_as_string longtycon t =
          let val s1 = TyCon.pr_LongTyCon longtycon
	      val s2 = TyName.pr_TyName t
	  in
	    s1 ^ (if s1 = s2 then "" else " (=" ^ s2 ^ ")")
	  end

    fun report (UNIFICATION (ty1, ty2)) =
          let
	    val names = StatObject.newTVNames ()
	    val pr = Type.pretty_string names
	  in
	    line "Type mismatch,"
	    // line ("   expecting: " ^ pr ty1)
	    // line ("   found:     " ^ pr ty2)
	  end

      | report (UNIFICATION_TEXT(text1, ty1, text2, ty2)) =
	  let
	    val names = StatObject.newTVNames ()
	    val pr1 = Type.pretty_string  names
	    val pr2 = Type.pretty_string_as_ty  names
	    fun pad n = if n<=0 then [] else " " :: pad (n-1)
	    fun max(i:int, j:int) = if i>j then i else j
	    val n = max(size text1, size text2)
	  in
	    line "Type clash,"
	    // line("   " ^ text1 ^ ": " ^  implode(pad(n-size text1)) ^ pr1 ty1)
	    // line("   " ^ text2 ^ ": " ^  implode(pad(n-size text2))  ^ pr2 (ty2,ty1))
	  end

      | report (LOOKUP_LONGID longid) =
	  line ("unbound identifier " ^ Ident.pr_longid longid ^ ".")
	  
      | report (LOOKUP_LONGTYCON longtycon) =
	  line ("unbound type constructor " ^ TyCon.pr_LongTyCon longtycon ^ ".")

      | report (NOTCONSTYPE ty) =
	  line (Type.string ty ^ "is not a constructed type.")

      | report (QUALIFIED_ID longid) =
	  line ("qualified identifier not allowed.")

      | report (UNGUARDED_TYVARS tyvars) =
	  line ("unguarded type variable" ^ maybe_plural_s tyvars 
		^ pp_list TyVar.string tyvars
		^ " in topdec.")

      | report (UNGENERALISABLE_TYVARS ids) =
	  line ("Provide a type annotation for "
		^ pp_list Ident.pr_id ids  ^ ".")

      | report (WRONG_ARITY{expected, actual}) =
	  line ("Wrong arity (expected " ^ Int.string expected
		^ ", actual " ^ Int.string actual ^ ").")

      | report (FLEX_REC_NOT_RESOLVED) =
	  line "Overloading not resolved in record containing the\
	   \ record wildcard (...)."

      | report (REPEATED_IDS ids) =
	  line ("Repeated identifier" ^ maybe_plural_s ids
		^ pp_list pr_repeatedId ids ^ ".")

      | report (TYVARS_NOT_IN_TYVARSEQ tyvars) =
	  line("unbound type variable" ^ maybe_plural_s tyvars
	       ^ pp_list TyVar.string tyvars ^ ".")

      | report (DATATYPES_ESCAPE_SCOPE tynames) =
	  line ("The datatype" ^ maybe_plural_s tynames
		^ pp_list TyName.pr_TyName tynames
		^ (case tynames of
		     [tyname] => " escapes its scope."
		   | tynames => " escape their scope."))

      | report (TYVARS_SCOPED_TWICE tyvars) =
	  line ("The scope of the type variable"
		^ maybe_plural_s tyvars
		^ pp_list TyVar.string tyvars
		^ " is bigger than this val declaration.")

      | report (REBINDING_TRUE_NIL_ETC ids) =
	  line ("You may not rebind `true', `false', `nil', `::', or `ref'.")

      | report REBINDING_IT =
	  line ("You may not rebind `it' as a constructor.")

      | report (SPECIFYING_TRUE_NIL_ETC ids) =
	  line ("You may not specify `true', `false', `nil', `::', or `ref'.")

      | report SPECIFYING_IT =
	  line ("You may not specify `it' as a constructor.")

      | report (LOOKUP_SIGID sigid) =
	  line ("unbound signature identifier " ^ SigId.pr_SigId sigid ^ ".")

      | report (LOOKUP_LONGSTRID longstrid) =
	  line ("unbound structure identifier "
		^ StrId.pr_LongStrId longstrid ^ ".")

      | report (LOOKUP_FUNID funid) =
	  line ("unbound functor identifier " ^ FunId.pr_FunId funid ^ ".")

      | report (EXDESC_SIDECONDITION) =
	  line "Type variables not allowed in type expression in exception description"
             
      | report (SHARING_TYPE_NOT_TYNAME (longtycon, theta)) =
	  line (TyCon.pr_LongTyCon longtycon
		^ " is "
		^ TypeFcn.pretty_string' (StatObject.newTVNames ()) theta
	        ^ ", so you cannot share it with anything.")
		    
      | report (SHARING_TYPE_RIGID (longtycon, t)) =
	  line (longtycon_and_tyname_as_string longtycon t
		^ " is defined before the preceding specification,")
	  // line "so you cannot share it here."

      | report (SHARING_TYPE_ARITY tynames) =
	  line ("You cannot share "
		^ pp_list (quote o TyName.pr_TyName) tynames
		^ " because their arities are different.")

      | report (WHERE_TYPE_NOT_WELLFORMED (longtycon, t, tau)) =
	  line (longtycon_and_tyname_as_string longtycon t 
		^ " is bound to a datatype; it cannot also be "
		^ Type.pretty_string (StatObject.newTVNames ()) tau ^ ".")

      | report (WHERE_TYPE_EQTYPE (longtycon, t, tau)) =
	  line (longtycon_and_tyname_as_string longtycon t
		^ " must be an eqtype, but "
		^ Type.pretty_string (StatObject.newTVNames ()) tau
		^ " does not admit equality.")

      | report (WHERE_TYPE_RIGID (longtycon, t)) =
	  line (longtycon_and_tyname_as_string longtycon t
		^ " is also specified outside this signature,")
	  // line "so you cannot define it here."

      | report (WHERE_TYPE_NOT_TYNAME (longtycon, theta, tau)) =
	  let val TVNames = StatObject.newTVNames () in
	  line (TyCon.pr_LongTyCon longtycon
		^ " is "
		^ TypeFcn.pretty_string' TVNames theta
	        ^ ", so it cannot be " 
	        ^ Type.pretty_string TVNames tau ^ ".")
	  end

      | report (WHERE_TYPE_ARITY (tyvars, (longtycon, tyname))) =
	  line ("Does " ^ longtycon_and_tyname_as_string longtycon tyname
		^ " have " ^ Int.string (TyName.arity tyname)
	        ^ " or " ^ Int.string (List.size tyvars)
	        ^ " arguments?")


      (* Signature Matching Errors *)

      | report (SIGMATCH_ERROR sigmatcherror) =
	  let open ModuleStatObject
	  in case sigmatcherror
	       of MISSINGSTR longstrid => line ("Missing structure: " ^ StrId.pr_LongStrId longstrid ^ ".")

		| MISSINGTYPE longtycon => line ("Missing type: " ^ TyCon.pr_LongTyCon longtycon ^ ".")

		| S_CONFLICTINGARITY(longtycon, _) => line ("S/Conflicting arity: " ^ TyCon.pr_LongTyCon longtycon ^ ".")

		| CONFLICTINGEQUALITY(longtycon, _) => line ("Conflicting equality attributes: "
							     ^ TyCon.pr_LongTyCon longtycon ^ ".")

		| MISSINGVAR (strids, id) => line ("Missing variable: " ^ prStrIds strids
						   ^ Ident.pr_id id ^ ".")

		| MISSINGEXC(strids, id) => line ("Error in signature matching: the exception `" ^ prStrIds strids
						  ^ Ident.pr_id id ^ "' is specified,")
		                            // line("but absent from the structure.")

		| S_RIGIDTYCLASH longtycon => line ("Rigid type clash for: " ^ TyCon.pr_LongTyCon longtycon ^ ".")

		| S_CONFLICTING_DOMCE longtycon =>
		 line ("Error in signature matching involving datatype `"
		       ^ TyCon.pr_LongTyCon longtycon ^ "'")
		 // line ("(The domain of the constructor environment specified in")
		 // line (" the signature clashes with the domain of the constructor")
		 // line (" environment in the structure.)")

	       | NOTYENRICHMENT{qualid=(strids, id),str_sigma,str_vce,sig_sigma,sig_vce} =>
		 line ("Error in signature matching:") 
		 // line ("the type specified in the signature does not enrich")
		 // line ("the type inferred in the structure;")
		 // line ("Structure declares:")
		 // line (str_vce ^ " " ^ prStrIds strids  ^ Ident.pr_id id ^ " :")
		 // line ("  " ^ StringTree_to_string(StatObject.TypeScheme.layout str_sigma))
		 // line ("Signature specifies:")
		 // line (sig_vce ^ " " ^ prStrIds strids  ^ Ident.pr_id id ^ " :")
		 // line ("  " ^ StringTree_to_string(StatObject.TypeScheme.layout sig_sigma) ^ ".")

	       | EXCNOTEQUAL(strids, id, (ty1, ty2)) =>
		 line("Error in signature matching:") 
		 // line("specified and declared type for exception `"
			 ^ prStrIds strids ^ Ident.pr_id id ^"' differ;")
		 // line("  declared  type: " ^ Type.string ty1)
		 // line("  specified  type: " ^ Type.string ty2)
	  end

      | report (CYCLE longstrid) =
	  line ("Cyclic sharing specification; cyclic structure path: "
		^ StrId.pr_LongStrId longstrid ^ ".")

      | report (U_RIGIDTYCLASH args) =
	  line ("Rigid type clash: " ^ pr_UnifyArgs args)

      | report (TYPESTRILLFORMEDNESS args) =
	  line ("Ill-formed type structure: " ^ pr_UnifyArgs args)

      | report (U_CONFLICTING_DOMCE args) =
	  line ("Conflicting CE domains: " ^ pr_UnifyArgs args)

      | report (U_CONFLICTINGARITY args) =
	  line ("U/Conflicting arity: " ^ pr_UnifyArgs args)

      | report (RIGIDTYFUNEQERROR args) =
	  line ("Equality attribute differs: " ^ pr_UnifyArgs args)
  end;


