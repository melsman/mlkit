(* MAKE Hooks for automatic file generation. *)

(*$LexSML_: ParseSIG_ LEX_BASICS LEX_UTILS*)
val _ = System.use "Parsing/Topdec.lex.sml";

(*$ParseSIG_: MyBase*)
val _ = System.use "Parsing/Topdec.grm.sig";

(*$ParseSML_: MyBase ParseSIG_ LEX_BASICS GRAMMAR_UTILS*)
val _ = System.use "Parsing/Topdec.grm.sml";
