structure SMLofNJ =
  struct
    structure SysInfo =
      struct
	fun getHostArch() = "HPPA"
	fun getOSName() = "HPUX"
      end
    fun exportFn(kitbinkit_path,kitexe) = print "SMLofNJ.exportFn not implemented"
  end