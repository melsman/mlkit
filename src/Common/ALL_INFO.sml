signature ALL_INFO =
  sig
    structure SourceInfo      : SOURCE_INFO
    structure DFInfo          : DF_INFO
    structure ParseInfo       : PARSE_INFO
      sharing ParseInfo.SourceInfo = SourceInfo
      sharing ParseInfo.DFInfo = DFInfo
    structure ErrorInfo       : ERROR_INFO
    structure OverloadingInfo : OVERLOADING_INFO
    structure ElabInfo : ELAB_INFO
      sharing ElabInfo.ParseInfo = ParseInfo
      sharing ElabInfo.ErrorInfo = ErrorInfo
      sharing ElabInfo.OverloadingInfo = OverloadingInfo
  end