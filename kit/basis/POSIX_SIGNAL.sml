signature POSIX_SIGNAL =
    sig
	eqtype signal
	
        val toWord   : signal -> SysWord.word
	val fromWord : SysWord.word -> signal
    end