(*ELAB_INFO

 Part of the front end:

        parsing                 elaboration                 
                 TopdecGrammar  (ElabTopdec)  TopdecGrammar  (CompileDec)
  SML  --------->    with     -------------->     with      -------------> LambdaExp
                   ParseInfo                    ElabInfo 

 ElabInfo is the information on the abstract syntax tree after
 elaboration.  It is a quadruple containing ParseInfo, ErrorInfo,
 TypeInfo, and OverloadingInfo.

 ParseInfo is the info on the abstract syntax tree before
 elaboration.  ErrorInfo is only present on a syntax tree node if
 elaboration gave an error on that node.  TypeInfo and
 OverloadingInfo are also not always present.  Each kind of info
 is implemented by a module called the same as the info kind.*)

(*$ELAB_INFO : PARSE_INFO ERROR_INFO TYPE_INFO OVERLOADING_INFO*)

signature ELAB_INFO =
  sig
    (*type supplied by this module:*) 
    type ElabInfo

    (*the constituent info types (imported from other modules):*)
    structure ParseInfo : PARSE_INFO
    structure ErrorInfo : ERROR_INFO
    structure TypeInfo : TYPE_INFO
    structure OverloadingInfo : OVERLOADING_INFO
    type ParseInfo = ParseInfo.ParseInfo
    type ErrorInfo = ErrorInfo.ErrorInfo
    type TypeInfo = TypeInfo.TypeInfo
    type OverloadingInfo = OverloadingInfo.OverloadingInfo

    (*other types imported from other modules:*)
    type StringTree       sharing type StringTree
                                       = ParseInfo.StringTree
                                       = TypeInfo.StringTree
		                       = OverloadingInfo.StringTree
    sharing type ErrorInfo.Report = ParseInfo.SourceInfo.Report

    val plus_ErrorInfo :         ElabInfo -> ErrorInfo -> ElabInfo
    val plus_TypeInfo :          ElabInfo -> TypeInfo -> ElabInfo
    val plus_OverloadingInfo :   ElabInfo -> OverloadingInfo -> ElabInfo
          (*Add overloading info regadless of whether
	   there already is overloading info attached*)
    val to_ParseInfo :           ElabInfo -> ParseInfo
    val to_ErrorInfo :           ElabInfo -> ErrorInfo option
    val to_TypeInfo :            ElabInfo -> TypeInfo option
    val to_OverloadingInfo :     ElabInfo -> OverloadingInfo option
    val remove_OverloadingInfo : ElabInfo -> ElabInfo
    val from_ParseInfo :         ParseInfo -> ElabInfo

    val retractRight : ElabInfo * ElabInfo -> ElabInfo
    val layout : ElabInfo -> StringTree
  end;
