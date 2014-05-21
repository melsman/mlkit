structure PreElabDecGrammar = DecGrammar
      (struct
	   type GrammarInfo = ParseInfo.ParseInfo
	   val bogus_info = 
	       ParseInfo.from_SourceInfo(SourceInfo.from_positions LexBasics.DUMMY LexBasics.DUMMY)
       end)
