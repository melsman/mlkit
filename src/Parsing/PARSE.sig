(* The lexing function is expressed in concrete types because of the untidy
   nature of the MLLex and MLYacc interfaces. You really don't want to see
   them. *)

signature PARSE =
  sig
    type InfixBasis
    type topdec
    type SourceReader

    val nameOf: SourceReader -> string

(*    val sourceFromStdIn: unit -> SourceReader *)
    val sourceFromFile: string -> SourceReader (*may raise Io s*)
    val sourceFromString: string -> SourceReader

    type Report

   (* In the following, the reason for an intermediate State type is that
      SourceReaders are line-by-line things. On a successful parse we want to
      continue reading the current line rather than rebuilding the lexing
      context and starting again (on the next line). *)

    type State
    datatype Result = SUCCESS of InfixBasis * topdec * State
      		    | ERROR of Report
		    | LEGAL_EOF	(* End-of-file before a phrase encountered.
				   (EOF in the middle of a phrase is an
				   error) *)

    val explicit_regions : bool ref (* support parsing of explicit region
                                     * annotations, such as 'region'
                                     * declarations. *)

    val begin: SourceReader -> State
    val parse: InfixBasis * State -> Result
  end
