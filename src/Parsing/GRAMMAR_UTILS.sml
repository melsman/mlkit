(* Contains functions to assist the analysis of derived forms, patch up the
   holes resulting from ambiguities in the SML grammar, and generally assist
   in the building of the parse tree. *)

signature GRAMMAR_UTILS =
  sig
    (*these two are abbreviations: (get rid of them)*)
    structure M : TOPDEC_GRAMMAR

    type info = M.info
    type Report

   (*We can get syntax errors while analysing layered patterns (this is
    during the activity of the parser; we aren't post-passing yet).*)

    type pos
    exception LAYERPAT_ERROR of (pos * pos)

    val topdecOfExp           : M.DecGrammar.exp -> M.topdec
    val composeStrDec         : info * M.strdec * M.strdec -> M.strdec
    val composeSpec           : info * M.spec * M.spec -> M.spec
    val inventStrId           : unit -> M.strid
    val composeDec            : info * M.DecGrammar.dec * M.DecGrammar.dec -> M.DecGrammar.dec
    val tuple_atexp_with_info : info -> M.DecGrammar.exp list -> (info*M.DecGrammar.regvar) option -> M.DecGrammar.atexp
    val tuple_atexp           : M.DecGrammar.exp list -> (info*M.DecGrammar.regvar) option -> M.DecGrammar.atexp
    val case_exp              : info -> M.DecGrammar.exp * M.DecGrammar.match -> M.DecGrammar.exp
    val sequenceExp           : M.DecGrammar.exp list -> M.DecGrammar.exp
    val inventId              : unit -> M.DecGrammar.id
    val inventId_from_atpat   : M.DecGrammar.atpat -> M.DecGrammar.id
    val atexpOfIdent          : info -> M.DecGrammar.id -> M.DecGrammar.atexp
    val patOfIdent            : info -> M.DecGrammar.id * (info*M.DecGrammar.regvar list) option * bool -> M.DecGrammar.pat
    val patOfAtpat            : M.DecGrammar.atpat -> M.DecGrammar.pat
    val expOfAtexp            : M.DecGrammar.atexp -> M.DecGrammar.exp
    val list_atexp            : info -> M.DecGrammar.exp list -> M.DecGrammar.atexp
    val hash                  : info -> M.DecGrammar.lab -> M.DecGrammar.atexp
    val exp_true              : info -> M.DecGrammar.exp
    val exp_false             : info -> M.DecGrammar.exp
    val exp_quote             : info -> string -> M.DecGrammar.exp
    val exp_antiquote         : info -> M.DecGrammar.atexp -> M.DecGrammar.exp
    val if_then_else_exp      : info -> M.DecGrammar.exp * M.DecGrammar.exp * M.DecGrammar.exp -> M.DecGrammar.exp
    val while_exp             : info -> M.DecGrammar.exp * M.DecGrammar.exp -> M.DecGrammar.exp
    val rewriteDatBind        : M.DecGrammar.datbind * M.DecGrammar.typbind -> M.DecGrammar.datbind
    val tuple_atpat_with_info : info -> M.DecGrammar.pat list -> M.DecGrammar.atpat
    val tuple_atpat           : M.DecGrammar.pat list -> M.DecGrammar.atpat
    val list_atpat            : info -> M.DecGrammar.pat list -> M.DecGrammar.atpat
    val layeredPat            : info * M.DecGrammar.pat * M.DecGrammar.pat -> M.DecGrammar.pat
    val tuple_type            : info -> M.DecGrammar.ty list -> M.DecGrammar.ty

    val fold_specs_to_spec    : (info * M.spec) list -> M.spec

    val rewrite_type_abbreviation_spec : M.tyvar list * M.tycon * M.ty * M.info * M.info -> info * M.spec

    val raise_lexical_error_if_none    : pos -> 'a option -> 'a
        (*raise LexBasics.LEXICAL_ERROR (pos, "constant too big") if the option is NONE.*)

   (* The following all come from the appropriate modules, but they're here
      for convenience and brevity. *)

    val mk_IdentLab           : string -> M.DecGrammar.lab
    val mk_IntegerLab         : int -> M.DecGrammar.lab
    val mk_Id                 : string -> M.DecGrammar.id
    val mk_LongId             : string list -> M.DecGrammar.longid
    val mk_FunId              : string -> M.funid
    val mk_StrId              : string -> M.strid
    val longStrIdOfStrId      : M.strid -> M.longstrid
    val mk_SigId              : string -> M.sigid
    val mk_LongStrId          : string list -> M.longstrid
    val mk_TyVar              : string -> M.DecGrammar.tyvar
    val mk_TyCon              : string -> M.DecGrammar.tycon
    val mk_LongTyCon          : string list -> M.longtycon
    val mk_IntSCon            : IntInf.int -> M.DecGrammar.scon
    val mk_WordSCon           : IntInf.int -> M.DecGrammar.scon
    val mk_StringSCon         : string -> M.DecGrammar.scon
    val mk_CharSCon           : int -> M.DecGrammar.scon
    val mk_RealSCon           : string -> M.DecGrammar.scon
    val impossible            : string -> 'a
    val span_info             : info * info -> info
    val PP                    : pos -> pos -> info      (* `PP L R' generates position
                                                           information. *)
    val un_PP                 : info -> pos * pos
    val rightmost             : ('a -> info) -> 'a -> ('b -> info) -> 'b option -> pos
    val rightmost'            : pos -> ('b -> info) -> 'b option -> pos
    val rightmost_of_three    : pos -> ('b -> info) -> 'b option ->
                                       ('c -> info) -> 'c option -> pos
    val rightmost_of_four     : pos -> ('b -> info) -> 'b option ->
                                       ('c -> info) -> 'c option ->
                                       ('d -> info) -> 'd option -> pos
    val right                 : info -> pos
    val wi_Convert            : ('a -> 'b) -> 'a M.DecGrammar.WithInfo list -> 'b M.DecGrammar.WithInfo list
                                        (* Conversion of "with-info" tagged
                                           lists (of identifiers etc.) *)
    val reportPosition_from_info : info -> Report
  end
