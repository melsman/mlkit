(*SourceInfo is a part of the ParseInfo.  See PARSE_INFO for an
 overview of the different kinds of info.*)

(*$SOURCE_INFO*)

signature SOURCE_INFO =
  sig
    (*type supplied by this module:*) 
    type SourceInfo

    (*types imported from other modules:*)
    type pos (*from LexBasics, I think*)
    type Report
    type StringTree

    val from_positions : pos -> pos -> SourceInfo
    val to_positions : SourceInfo -> pos * pos

    val report : SourceInfo -> Report
    val layout : SourceInfo -> StringTree
  end;
