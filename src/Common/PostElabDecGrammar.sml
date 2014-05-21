structure PostElabDecGrammar =
    DecGrammar(struct
		   type GrammarInfo = AllInfo.ElabInfo.ElabInfo
		   val bogus_info = AllInfo.ElabInfo.from_ParseInfo PreElabDecGrammar.bogus_info
	       end)
