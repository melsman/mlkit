(* Grammar for modules - Definition v3 pages 12-14 *)

signature TOPDEC_GRAMMAR =
  sig
    (* Core declarations. *)

    structure DecGrammar : DEC_GRAMMAR
    type dec = DecGrammar.dec

    (* Various kinds of module identifiers. *)

    structure StrId : STRID sharing StrId = DecGrammar.StrId
    structure FunId : FUNID
    structure SigId : SIGID
    type strid = StrId.strid
    type longstrid = StrId.longstrid 
    type funid = FunId.funid
    type sigid = SigId.sigid

    (* Objects from the core syntax (needed for specs). *)

    type id = DecGrammar.id
    type tyvar = DecGrammar.tyvar
    type ty = DecGrammar.ty
    eqtype tycon sharing type tycon = DecGrammar.tycon
    type longtycon = DecGrammar.longtycon

    (* info place-holder. *)

    type info      sharing type info = DecGrammar.info

    datatype 'a WithInfo = WITH_INFO of info * 'a
      sharing type WithInfo = DecGrammar.WithInfo

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
      STRBIND of info * strid * strexp * strbind option

    and sigexp =
      SIGsigexp of info * spec |
      SIGIDsigexp of info * sigid |
      WHERE_TYPEsigexp of info * sigexp * tyvar list * longtycon * ty

    and sigdec =
      SIGNATUREsigdec of info * sigbind

    and sigbind =
      SIGBIND of info * sigid * sigexp * sigbind option

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
      VALDESC of info * id * ty * valdesc option

    and typdesc =
      TYPDESC of info * tyvar list * tycon * typdesc option

    and datdesc =
      DATDESC of info * tyvar list * tycon * condesc * datdesc option

    and condesc =
      CONDESC of info * id * ty option * condesc option

    and exdesc =
      EXDESC of info * id * ty option * exdesc option

    and strdesc =
      STRDESC of info * strid * sigexp * strdesc option

			    (* Figure 8 *)

    and fundec =
      FUNCTORfundec of info * funbind 

    and funbind =
      FUNBIND of info * funid * strid * sigexp * strexp * funbind option

    and topdec =
      STRtopdec of info * strdec * topdec option |
      SIGtopdec of info * sigdec * topdec option |
      FUNtopdec of info * fundec * topdec option

    val empty_topdec : topdec

    val getExplicitTyVarsCondesc : condesc -> tyvar list
    val info_on_strexp : strexp -> info
    val info_on_strdec : strdec -> info
    val info_on_strbind : strbind -> info
    val info_on_sigexp : sigexp -> info
    val info_on_sigdec : sigdec -> info
    val info_on_sigbind : sigbind -> info
    val info_on_spec : spec -> info
    val info_on_valdesc : valdesc -> info
    val info_on_typdesc : typdesc -> info
    val info_on_datdesc : datdesc -> info
    val info_on_condesc : condesc -> info
    val info_on_exdesc : exdesc -> info
    val info_on_strdesc : strdesc -> info
    val info_on_fundec : fundec -> info
    val info_on_funbind : funbind -> info
    val info_on_topdec : topdec -> info

    val map_strexp_info : (info -> info) -> strexp -> strexp
      (* does not proceed into sigexp's *)

    type StringTree sharing type StringTree = DecGrammar.StringTree

    val layoutStrexp  : strexp  -> StringTree
    and layoutStrdec  : strdec  -> StringTree
    and layoutStrbind : strbind -> StringTree
    and layoutSigexp  : sigexp  -> StringTree
    and layoutSpec    : spec    -> StringTree
    and layoutFunbind : funbind -> StringTree
    and layoutTopdec  : topdec  -> StringTree
  end;
