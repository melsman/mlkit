(* Contains functions to assist the analysis of derived forms, patch up the
   holes resulting from ambiguities in the SML grammar, and generally assist
   in the building of the parse tree. *)

(*$GRAMMAR_UTILS : DEC_GRAMMAR TOPDEC_GRAMMAR*)
signature GRAMMAR_UTILS =
  sig
    structure TopdecGrammar : TOPDEC_GRAMMAR
    (*these two are abbreviations: (get rid of them)*)
    structure M : TOPDEC_GRAMMAR   sharing M = TopdecGrammar
    structure C : DEC_GRAMMAR      sharing C = TopdecGrammar.DecGrammar
    type info = TopdecGrammar.info
    type Report

   (*We can get syntax errors while analysing layered patterns (this is
    during the activity of the parser; we aren't post-passing yet).*)

    type pos
    exception LAYERPAT_ERROR of (pos * pos) 

    val topdecOfExp: C.exp -> M.topdec
    val composeStrDec: info * M.strdec * M.strdec -> M.strdec
    val composeSpec: info * M.spec * M.spec -> M.spec
    val inventStrId: unit -> M.strid
    val composeDec: info * C.dec * C.dec -> C.dec
    val tuple_atexp_with_info : info -> C.exp list -> C.atexp
    val tuple_atexp : C.exp list -> C.atexp
    val case_exp : info -> C.exp * C.match -> C.exp
    val sequenceExp: C.exp list -> C.exp
    val inventId: unit -> C.id
    val inventId_from_atpat: C.atpat -> C.id
    val atexpOfIdent : info -> C.id -> C.atexp
    val patOfIdent : info -> C.id * bool -> C.pat
    val patOfAtpat: C.atpat -> C.pat
    val expOfAtexp: C.atexp -> C.exp
    val list_atexp : info -> C.exp list -> C.atexp
    val hash : info -> C.lab -> C.atexp
    val exp_true : info -> C.exp
    val exp_false : info -> C.exp
    val if_then_else_exp : info -> C.exp * C.exp * C.exp -> C.exp
    val while_exp : info -> C.exp * C.exp -> C.exp
    val rewriteDatBind: C.datbind * C.typbind -> C.datbind
    val tuple_atpat_with_info : info -> C.pat list -> C.atpat
    val tuple_atpat : C.pat list -> C.atpat
    val list_atpat : info -> C.pat list -> C.atpat
    val layeredPat: info * C.pat * C.pat -> C.pat
    val tuple_type : info -> C.ty list -> C.ty
    val rewrite_type_abbreviation_spec: M.tyvar list * M.tycon * M.ty *
          M.info * M.info -> info * M.spec
    val fold_specs_to_spec : (info * M.spec) list -> M.spec

    val raise_lexical_error_if_none : pos -> 'a option -> 'a
        (*raise LexBasics.LEXICAL_ERROR (pos, "grr") if the option is NONE.*)

   (* The following all come from the appropriate modules, but they're here
      for convenience and brevity. *)

    val mk_IdentLab: string -> C.lab
    val mk_IntegerLab: int -> C.lab
    val mk_Id: string -> C.id
    val mk_LongId: string list -> C.longid
    val mk_FunId: string -> M.funid
    val mk_StrId: string -> M.strid
    val longStrIdOfStrId : M.strid -> M.longstrid
    val mk_SigId: string -> M.sigid
    val mk_LongStrId: string list -> M.longstrid
    val mk_TyVar: string -> C.tyvar
    val mk_TyCon: string -> C.tycon
    val mk_LongTyCon: string list -> M.longtycon
    val mk_IntSCon: int -> C.scon
    val mk_WordSCon: int -> C.scon
    val mk_StringSCon: string -> C.scon
    val mk_CharSCon: int -> C.scon
    val mk_RealSCon: string -> C.scon
    val impossible : string -> 'a
    val span_info : info * info -> info
    val PP : pos -> pos -> info	(* `PP L R' generates position
					   information. *)
    val un_PP : info -> pos * pos
    val rightmost :  ('a -> info) -> 'a ->
                     ('b -> info) -> 'b option -> pos
    val rightmost' : pos -> ('b -> info) -> 'b option -> pos
    val rightmost_of_three : pos -> ('b -> info) -> 'b option ->
                                    ('c -> info) -> 'c option -> pos
    val rightmost_of_four  : pos -> ('b -> info) -> 'b option ->
                                    ('c -> info) -> 'c option ->
                                    ('d -> info) -> 'd option -> pos
    val right : info -> pos
    val wi_Convert: ('a -> 'b) -> 'a C.WithInfo list -> 'b C.WithInfo list
					(* Conversion of "with-info" tagged
					   lists (of identifiers etc.) *)
    val reportPosition_from_info : info -> Report
  end;
