(*$SourceInfo: LEX_BASICS PRETTYPRINT CRASH SOURCE_INFO*)

functor SourceInfo (structure LexBasics : LEX_BASICS
		    structure PrettyPrint : PRETTYPRINT
		    sharing type LexBasics.StringTree = PrettyPrint.StringTree
		    structure Crash: CRASH
		      ) : SOURCE_INFO =
  struct
    type pos = LexBasics.pos
    type Report = LexBasics.Report
    type StringTree = PrettyPrint.StringTree

    datatype SourceInfo = POSinfo of {left : pos, right : pos}

    fun from_positions left right = POSinfo {left=left, right=right}
    fun to_positions (POSinfo {left, right}) = (left, right)

    fun report (POSinfo {left, right}) =
          LexBasics.reportPosition {left=left, right=right}

    fun layout (POSinfo {left, right}) =
          PrettyPrint.NODE {start="SourceInfo(", finish=")", indent=0,
			    childsep=PrettyPrint.RIGHT ", ",
			    children=[LexBasics.layoutPos left,
				      LexBasics.layoutPos right]}
  end;
