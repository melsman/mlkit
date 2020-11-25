(*PARSE_INFO

 Part of the front end:

        parsing                 elaboration
                 TopdecGrammar  (ElabTopdec)  TopdecGrammar  (CompileDec)
  SML  --------->    with     -------------->     with      -------------> LambdaExp
                   ParseInfo                    ElabInfo

 ParseInfo is the information on the abstract syntax tree after
 parsing.  It is a pair containing SourceInfo and DFInfo (Derived
 Form).

 The SourceInfo on a syntax tree node is the position in the
 source SML program of the beginning and the end of the text that
 was parsed as this node.

 DFInfo is not always present on a syntax tree node.  Each kind
 of info is implemented by a module called the same as the info
 kind.*)

signature PARSE_INFO =
  sig
    (*The composite info types, pre-elab:*)
    type ParseInfo

    (*The constituent info types (imported from other modules):*)
    structure SourceInfo : SOURCE_INFO
    structure DFInfo : DF_INFO
    type SourceInfo  sharing type SourceInfo = SourceInfo.SourceInfo
    type DFInfo      sharing type DFInfo = DFInfo.DFInfo

    type StringTree       sharing type StringTree
                                       = DFInfo.StringTree
                                       = SourceInfo.StringTree

    val from_SourceInfo : SourceInfo -> ParseInfo
    val to_SourceInfo   : ParseInfo -> SourceInfo
    val plus_DFInfo     : ParseInfo -> DFInfo -> ParseInfo
    val to_DFInfo       : ParseInfo -> DFInfo option
    val layout          : ParseInfo -> StringTree
  end
