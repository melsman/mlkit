structure StringFinMap : MONO_FINMAP = 
    FinMapPT(struct type T = string
		    val eq = op =
		    val lt = op <
		    val hash = strhash
		    fun toString a = a
	     end)
