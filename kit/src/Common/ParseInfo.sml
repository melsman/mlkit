structure ParseInfo: PARSE_INFO =
  struct
    fun impossible s = Crash.impossible ("ParseInfo." ^ s)

    structure SourceInfo = SourceInfo
    structure DFInfo = DFInfo
    type SourceInfo      = SourceInfo.SourceInfo
    type DFInfo          = DFInfo.DFInfo
    type StringTree      = PrettyPrint.StringTree

    datatype ParseInfo =
      PARSE_INFO of {SourceInfo : SourceInfo, DFInfo : DFInfo option}

    fun from_SourceInfo SourceInfo =
	  PARSE_INFO {SourceInfo=SourceInfo, DFInfo=NONE}

    fun plus_DFInfo (PARSE_INFO {SourceInfo, DFInfo=NONE}) DFInfo =
	  PARSE_INFO {SourceInfo=SourceInfo, DFInfo=SOME DFInfo}
      | plus_DFInfo _ _ = impossible "plus_DFInfo"

    fun to_SourceInfo (PARSE_INFO {SourceInfo, ...}) = SourceInfo
    fun to_DFInfo (PARSE_INFO {DFInfo, ...}) = DFInfo

    fun layout (PARSE_INFO {SourceInfo, DFInfo}) =
          PrettyPrint.NODE {start="ParseInfo{",
			    finish="}",
			    indent=3,
			    children=[SourceInfo.layout SourceInfo,
				      PrettyPrint.layout_opt DFInfo.layout DFInfo],
			    childsep=PrettyPrint.RIGHT "; "}
  end;
