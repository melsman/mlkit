(* Grammar for modules (modules version) - Definition v3 pages 12-14 *)

(*$TopdecGrammar : DEC_GRAMMAR FUNID SIGID PRETTYPRINT
    TOPDEC_GRAMMAR*)

functor TopdecGrammar(structure FunId: FUNID
		      structure SigId: SIGID
		      structure DecGrammar : DEC_GRAMMAR
		      structure PrettyPrint : PRETTYPRINT
		        sharing type DecGrammar.StringTree = PrettyPrint.StringTree
			  ) : TOPDEC_GRAMMAR =
  struct
    structure DecGrammar = DecGrammar
    structure StrId = DecGrammar.StrId
    structure FunId = FunId
    structure SigId = SigId
    open DecGrammar

    type strid = StrId.strid
    type funid = FunId.funid
    type sigid = SigId.sigid

			    (* Figure 6 *)

    datatype strexp =
      STRUCTstrexp of info * strdec |
      LONGSTRIDstrexp of info * longstrid |
      TRANSPARENT_CONSTRAINTstrexp of info * strexp * sigexp |
      OPAQUE_CONSTRAINTstrexp of info * strexp * sigexp |
      APPstrexp of info * funid * strexp |
      LETstrexp of info * strdec * strexp

    and strdec =
      DECstrdec of info * dec |
      STRUCTUREstrdec of info * strbind |
      LOCALstrdec of info * strdec * strdec |
      EMPTYstrdec of info |
      SEQstrdec of info * strdec * strdec

    and strbind =
      STRBIND of info * strid * strexp * strbind Option

    and sigexp =
      SIGsigexp of info * spec |
      SIGIDsigexp of info * sigid |
      WHERE_TYPEsigexp of info * sigexp * tyvar list * longtycon * ty


    and sigdec =
      SIGNATUREsigdec of info * sigbind

    and sigbind =
      SIGBIND of info * sigid * sigexp * sigbind Option

			    (* Figure 7 *)

    and spec =
      VALspec of info * valdesc |
      TYPEspec of info * typdesc |
      EQTYPEspec of info * typdesc |
      DATATYPEspec of info * datdesc |
      DATATYPE_REPLICATIONspec of info * tycon * longtycon |
      EXCEPTIONspec of info * exdesc |
      STRUCTUREspec of info * strdesc |
      INCLUDEspec of info * sigexp |
      SHARING_TYPEspec of info * spec * longtycon WithInfo list |
      SHARINGspec of info * spec * longstrid WithInfo list |
      EMPTYspec of info |
      SEQspec of info * spec * spec

    and valdesc =
      VALDESC of info * id * ty * valdesc Option

    and typdesc =
      TYPDESC of info * tyvar list * tycon * typdesc Option

    and datdesc =
      DATDESC of info * tyvar list * tycon * condesc * datdesc Option

    and condesc =
      CONDESC of info * id * ty Option * condesc Option

    and exdesc =
      EXDESC of info * id * ty Option * exdesc Option

    and strdesc =
      STRDESC of info * strid * sigexp * strdesc Option

			    (* Figure 8 *)

    and fundec =
      FUNCTORfundec of info * funbind

    and funbind =
      FUNBIND of info * funid * strid * sigexp * strexp * funbind Option

    and topdec =
      STRtopdec of info * strdec * topdec Option |
      SIGtopdec of info * sigdec * topdec Option |
      FUNtopdec of info * fundec * topdec Option 

    fun info_on_strexp (STRUCTstrexp (info, strdec)) = info
      | info_on_strexp (LONGSTRIDstrexp (info, longstrid)) = info
      | info_on_strexp (TRANSPARENT_CONSTRAINTstrexp (info, strexp, sigexp)) = info
      | info_on_strexp (OPAQUE_CONSTRAINTstrexp (info, strexp, sigexp)) = info
      | info_on_strexp (APPstrexp (info, funid, strexp)) = info
      | info_on_strexp (LETstrexp (info, strdec, strexp)) = info 
					   
    fun info_on_strdec (DECstrdec (info, dec)) = info
      | info_on_strdec (STRUCTUREstrdec (info, strbind)) = info
      | info_on_strdec (LOCALstrdec (info, strdec1, strdec2)) = info
      | info_on_strdec (EMPTYstrdec info) = info
      | info_on_strdec (SEQstrdec (info, strdec1, strdec2)) = info

    fun info_on_strbind (STRBIND (info, strid, strexp, strbind_opt)) = info

    fun info_on_sigexp (SIGsigexp (info, spec)) = info
      | info_on_sigexp (SIGIDsigexp (info, sigid)) = info
      | info_on_sigexp (WHERE_TYPEsigexp (info, sigexp, tyvars, longtycon, ty)) = info

    fun info_on_sigdec (SIGNATUREsigdec (info, sigbind)) = info

    fun info_on_sigbind (SIGBIND (info, sigid, sigexp, sigbind_opt)) = info

    fun info_on_spec (VALspec (info, valdesc)) = info
      | info_on_spec (TYPEspec (info, typdesc)) = info
      | info_on_spec (EQTYPEspec (info, typdesc)) = info
      | info_on_spec (DATATYPEspec (info, datdesc)) = info
      | info_on_spec (DATATYPE_REPLICATIONspec (info, tycon, longtycon)) = info
      | info_on_spec (EXCEPTIONspec (info, exdesc)) = info
      | info_on_spec (STRUCTUREspec (info, strdesc)) = info
      | info_on_spec (INCLUDEspec (info, sigexp)) = info
      | info_on_spec (SHARING_TYPEspec (info, spec, longtycon_WithInfo_s)) = info
      | info_on_spec (SHARINGspec (info, spec, longstrid_WithInfo_s)) = info
      | info_on_spec (EMPTYspec info) = info
      | info_on_spec (SEQspec (info, spec1, spec2)) = info

    fun info_on_valdesc (VALDESC (info, id, ty, valdesc_opt)) = info

    fun info_on_typdesc (TYPDESC (info, tyvars, tycon, typdesc_opt)) = info

    fun info_on_datdesc (DATDESC (info, tyvars, tycon, condesc, datdesc_opt)) = info
      
    fun info_on_condesc (CONDESC (info, id, ty_opt, condesc_opt)) = info
      
    fun info_on_exdesc (EXDESC (info, id, ty_opt, exdesc_opt)) = info
      
    fun info_on_strdesc (STRDESC (info, strid, sigexp, strdesc_opt)) = info

    fun info_on_fundec (FUNCTORfundec (info, funbind)) = info

    fun info_on_funbind (FUNBIND (info, funid, strid, sigexp, strexp, funbind_opt)) = info
      
    fun info_on_topdec (STRtopdec (info, strdec, topdec_opt)) = info
      | info_on_topdec (SIGtopdec (info, sigdec, topdec_opt)) = info
      | info_on_topdec (FUNtopdec (info, fundec, topdec_opt)) = info


    (* map on info *)
    fun map_strexp_info f strexp =
      case strexp
	of STRUCTstrexp (info, strdec) => STRUCTstrexp(f info, map_strdec_info f strdec)
	 | LONGSTRIDstrexp (info, longstrid) => LONGSTRIDstrexp(f info, longstrid)
	 | TRANSPARENT_CONSTRAINTstrexp (info, strexp, sigexp) => 
	  TRANSPARENT_CONSTRAINTstrexp(f info, map_strexp_info f strexp, sigexp)
	 | OPAQUE_CONSTRAINTstrexp (info, strexp, sigexp) => 
	  OPAQUE_CONSTRAINTstrexp(f info, map_strexp_info f strexp, sigexp)
	 | APPstrexp (info, funid, strexp) =>
	  APPstrexp (f info, funid, map_strexp_info f strexp)
	 | LETstrexp (info, strdec, strexp) =>
	  LETstrexp (f info, map_strdec_info f strdec,map_strexp_info f strexp)
    and map_strdec_info f strdec =
      case strdec
	of DECstrdec (info, dec) => DECstrdec(f info, DecGrammar.map_dec_info f dec)
	 | STRUCTUREstrdec (info, strbind) => STRUCTUREstrdec(f info, map_strbind_info f strbind)
	 | LOCALstrdec (info, strdec1, strdec2) => 
	  LOCALstrdec(f info, map_strdec_info f strdec1, map_strdec_info f strdec2)
	 | EMPTYstrdec info => EMPTYstrdec (f info)
	 | SEQstrdec (info, strdec1, strdec2) => 
	  SEQstrdec(f info, map_strdec_info f strdec1, map_strdec_info f strdec2)
    and map_strbind_info f strbind =
      case strbind
	of STRBIND (info, strid, strexp, strbind_opt) => 
	  STRBIND(f info, strid, map_strexp_info f strexp, 
		  case strbind_opt
		    of Some strbind => Some (map_strbind_info f strbind)
		     | None => None) 

    (* pretty-printing *)

    local
      fun f res (CONDESC(_, _, tyopt, conopt)) =
	let
	  val res' = 
	    case tyopt of 
	      None => res 
	    | Some ty => (DecGrammar.getExplicitTyVarsTy ty) @ res
	in
	  case conopt of 
	    None => res'
	  | Some condesc => f res' condesc 
	end
    in
      val getExplicitTyVarsCondesc = f []
    end

    local
      open PrettyPrint
    in

    type StringTree = StringTree
    val INDENT = 3			(* standard indentation level. *)

    fun makeList (f: 'a -> 'a Option) (x: 'a) =
      x :: (case f x
	      of Some y => makeList f y
	       | None => nil
	   )

    fun layoutStrexp strexp =
      case strexp
	of STRUCTstrexp(_, strdec) =>
	     NODE{start="struct ", finish=" end", indent=INDENT,
		     children=[layoutStrdec strdec], childsep=NONE
		    }

	 | LONGSTRIDstrexp(_, longstrid) =>
	     layoutAtom StrId.pr_LongStrId longstrid

	 | TRANSPARENT_CONSTRAINTstrexp(_, strexp, sigexp) =>
	     layout_together [layoutStrexp strexp, LEAF ":",
				 layoutSigexp sigexp] INDENT

	 | OPAQUE_CONSTRAINTstrexp(_, strexp, sigexp) =>
	     layout_together [layoutStrexp strexp, LEAF ":>",
				 layoutSigexp sigexp] INDENT

	 | APPstrexp(_, funid, strexp) =>
	     NODE{start=FunId.pr_FunId funid ^ "(", finish=")",
		     indent=INDENT,
		     children=[layoutStrexp strexp], childsep=NONE
		    }

	 | LETstrexp(_, strdec, strexp) =>
	     NODE{start="let ", finish=" end", indent=INDENT,
		     children=[layoutStrdec strdec, layoutStrexp strexp],
		     childsep=LEFT " in "
		    }

    and layoutStrdec strdec =
      case strdec
	of DECstrdec(_, dec) =>
	     DecGrammar.layoutDec dec

	 | STRUCTUREstrdec(_, strbind) =>
	     NODE{start="structure ", finish="", indent=INDENT,
		     children=[layoutStrbind strbind], childsep=NONE
		    }

	 | LOCALstrdec(_, strdec, strdec') =>
	     NODE{start="local ", finish=" end", indent=INDENT,
		     children=[layoutStrdec strdec, layoutStrdec strdec'],
		     childsep=LEFT " in "
		    }

	 | EMPTYstrdec _ =>
	     LEAF ""

	 | SEQstrdec(_, strdec, strdec') =>
	     NODE{start="", finish="", indent=0,
		     children=[layoutStrdec strdec, layoutStrdec strdec'],
		     childsep=RIGHT "; "
		    }

    and layoutStrbind strbind =
      let
	val strbinds = makeList (fn STRBIND(_, _, _, opt) => opt) strbind

	fun layout1(STRBIND(_, strid, strexp, _)) =
	      NODE{start="", finish="", indent=0,
		      children=[LEAF(StrId.pr_StrId strid),
				layoutStrexp strexp],
		      childsep=RIGHT " = "}
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 strbinds,
		childsep=LEFT " and "
	       }
      end

    and layoutSigexp sigexp =
      (case sigexp
	of SIGsigexp(_, spec) =>
	     NODE{start="sig ", finish=" end", indent=INDENT,
		     children=[layoutSpec spec], childsep=NONE
		    }

	 | SIGIDsigexp(_, sigid) =>
	     layoutAtom SigId.pr_SigId sigid

	 | WHERE_TYPEsigexp(i, sigexp, tyvars, longtycon, ty) =>
	     layout_together
	     [layoutSigexp sigexp,
	      NODE {start="where type ", finish="", indent=0,
		       children=
		       [layout_together
			((case DecGrammar.layoutTyvarseq tyvars of
			    Some st => [st]
			  | None => [])
			  @ [LEAF (TyCon.pr_LongTyCon longtycon)]) INDENT,
			DecGrammar.layoutTy ty],
		       childsep=RIGHT " = "}] INDENT)

    and layoutSigdec (SIGNATUREsigdec(_, sigbind)) =
          NODE{start="signature ", finish="", indent=INDENT,
		  children=[layoutSigbind sigbind], childsep=NONE}

    and layoutSigbind sigbind =
      let
	val sigbinds = makeList (fn SIGBIND(_, _, _, opt) => opt) sigbind

	fun layout1(SIGBIND(_, sigid, sigexp, _)) =
	  NODE{start="", finish="", indent=0,
		  children=[layoutAtom SigId.pr_SigId sigid,
			    layoutSigexp sigexp
			   ],
		  childsep=RIGHT " = "
		 }
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 sigbinds,
		childsep=LEFT " and "
	       }
      end

    and layoutSpec spec =
      case spec
	of VALspec(_, valdesc) =>
	     NODE{start="val ", finish="", indent=INDENT,
		     children=[layoutValdesc valdesc], childsep=NONE
		    }

	 | TYPEspec(_, typdesc) =>
	     NODE{start="type ", finish="", indent=INDENT,
		     children=[layoutTypdesc typdesc], childsep=NONE
		    }

	 | EQTYPEspec(_, typdesc) =>
	     NODE{start="eqtype ", finish="", indent=INDENT,
		     children=[layoutTypdesc typdesc], childsep=NONE
		    }

	 | DATATYPEspec(_, datdesc) =>
	     NODE{start="datatype ", finish="", indent=INDENT,
		     children=[layoutDatdesc datdesc], childsep=NONE
		    }

	 | DATATYPE_REPLICATIONspec(i,tycon,longtycon) => 
	     DecGrammar.layout_datatype_replication
	     (i, tycon, longtycon)

	 | EXCEPTIONspec(_, exdesc) =>
	     NODE{start="exception ", finish="", indent=INDENT,
		     children=[layoutExdesc exdesc], childsep=NONE
		    }

	 | STRUCTUREspec(_, strdesc) =>
	     NODE{start="structure ", finish="", indent=INDENT,
		     children=[layoutStrdesc strdesc], childsep=NONE
		    }

	 | INCLUDEspec(_, sigexp) =>
	     NODE{start="include ", finish="", indent=8,
		     children=[layoutSigexp sigexp],
		     childsep=NONE}

	 | SHARING_TYPEspec(i, spec, longtycon_withinfo_list) =>
	     let	     
	       val st_longtycon_withinfo_list =
		     NODE{start=" sharing type ", finish="",
			     indent=INDENT,
			     childsep=LEFT " = ",
			     children=map (LEAF o TyCon.pr_LongTyCon o strip_info)
				      longtycon_withinfo_list}
	     in
	       layout_together [layoutSpec spec, st_longtycon_withinfo_list]
	       INDENT
	     end

	 | SHARINGspec(i, spec, longstrid_withinfo_list) =>
	     let	     
	       val st_longstrid_withinfo_list =
		     NODE{start=" sharing ", finish="",
			     indent=INDENT,
			     childsep=LEFT " = ",
			     children=map (LEAF o StrId.pr_LongStrId o strip_info)
				      longstrid_withinfo_list}
	     in
	       layout_together [layoutSpec spec, st_longstrid_withinfo_list]
	       INDENT
	     end

	 | EMPTYspec _ =>
	     LEAF ""

	 | SEQspec(_, spec, spec') =>
	     NODE{start="", finish="", indent=0,
		     children=[layoutSpec spec, layoutSpec spec'],
		     childsep=RIGHT "; "
		    }

    and layoutValdesc valdesc =
      let
	val valdescs = makeList (fn VALDESC(_, _, _, opt) => opt) valdesc

	fun layout1(VALDESC(_, id, ty, _)) =
	  NODE{start="", finish="", indent=0,
		  children=[layoutAtom Ident.pr_id id,
			    DecGrammar.layoutTy ty
			   ],
		  childsep=LEFT " : "
		 }
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 valdescs,
		childsep=LEFT " and "
	       }
      end

    and layoutTypdesc typdesc =
      let
	val typdescs = makeList (fn TYPDESC(_, _, _, opt) => opt) typdesc

	fun layout1(TYPDESC(_, tyvarseq, tycon, _)) =
	  NODE{start="", finish="", indent=0,
		  children=(case DecGrammar.layoutTyvarseq tyvarseq
			      of Some t => [t]
			       | None => nil
			   ) @ [layoutAtom TyCon.pr_TyCon tycon],
		  childsep=RIGHT " "
		 }
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 typdescs,
		childsep=LEFT " and "
	       }
      end

    and layoutDatdesc datdesc =
      let
	val datdescs = makeList (fn DATDESC(_, _, _, _, opt) => opt) datdesc

	fun layoutCondesc condesc =
	  let
	    val condescs = makeList (fn CONDESC(_, _, _, opt) => opt) condesc

	    fun layout1(CONDESC(_, id, ty_opt, _)) =
	      NODE{start="", finish="", indent=0,
		      children=
		        layoutAtom Ident.pr_id id
			:: (case ty_opt of Some ty => [DecGrammar.layoutTy ty]
			      		 | None => nil
			   ),
		      childsep=LEFT " of "
		     }
	  in
	    NODE{start="", finish="", indent=0,
		    children=map layout1 condescs,
		    childsep=LEFT " | "
		   }
	  end
	  
	fun layoutBind(tyvarseq, tycon) =
	  NODE{start="", finish="", indent=0,
		  children=(case DecGrammar.layoutTyvarseq tyvarseq
			      of Some t => [t]
			       | None => nil
			   ) @ [layoutAtom TyCon.pr_TyCon tycon],
		  childsep=RIGHT " "
		 }

	fun layout1(DATDESC(_, tyvarseq, tycon, condesc, _)) =
	  NODE{start="", finish="", indent=0,
		  children=[layoutBind(tyvarseq, tycon), layoutCondesc condesc],
		  childsep=RIGHT " = "
		 }
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 datdescs,
		childsep=LEFT " and "
	       }
      end

    and layoutExdesc exdesc =
      let
	val exdescs = makeList (fn EXDESC(_, _, _, opt) => opt) exdesc

	fun layout1(EXDESC(_, id, ty_opt, _)) =
	  NODE{start="", finish="", indent=0,
		  children=
		    layoutAtom Ident.pr_id id
		    :: (case ty_opt of Some ty => [DecGrammar.layoutTy ty]
				     | None => nil
		       ),
		  childsep=LEFT " of "
		 }
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 exdescs,
		childsep=LEFT " and "
	       }
      end

    and layoutStrdesc strdesc =
      let
	val strdescs = makeList (fn STRDESC(_, _, _, opt) => opt) strdesc

	fun layout1(STRDESC(_, strid, sigexp, _)) =
	  NODE{start="", finish="", indent=0,
		  children=[layoutAtom StrId.pr_StrId strid,
			    layoutSigexp sigexp
			   ],
		  childsep=LEFT " : "
		 }
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 strdescs,
		childsep=LEFT " and "
	       }
      end

    and layoutFundec (FUNCTORfundec(_, funbind)) =
          NODE{start="functor ", finish="", indent=INDENT,
		  children=[layoutFunbind funbind], childsep=NONE}

    and layoutFunbind funbind =
      let
	val funbinds =
	  makeList (fn FUNBIND(_, _, _, _, _, opt) => opt) funbind

	fun layout1(FUNBIND(_, funid, strid, sigexp, strexp, _)) =
	  let
	    val lhs =
	      NODE{start=FunId.pr_FunId funid ^ "(", finish=")",
		      indent=INDENT,
		      children=[layoutAtom StrId.pr_StrId strid,
				layoutSigexp sigexp],
		      childsep=LEFT " : "}
	  in
	    NODE{start="", finish="", indent=0,
		    children=[lhs, layoutStrexp strexp],
		    childsep=RIGHT " = "}
	  end
      in
	NODE{start="", finish="", indent=0,
		children=map layout1 funbinds,
		childsep=LEFT " and "}
      end

    fun layoutTopdec topdec =
      let
	val topdecs =
	  makeList (fn STRtopdec(_, _, topdec_opt) => topdec_opt
	             | SIGtopdec(_, _, topdec_opt) => topdec_opt
		     | FUNtopdec(_, _, topdec_opt) => topdec_opt) topdec
      in
	NODE{start="", finish="", indent=0, childsep=RIGHT "; ",
	     children =
	       map (fn STRtopdec(_, strdec, _) => layoutStrdec strdec
	             | SIGtopdec(_, sigdec, _) => layoutSigdec sigdec
		     | FUNtopdec(_, fundec, _) => layoutFundec fundec)
	       topdecs}
      end
    end (*local*)

  end;
