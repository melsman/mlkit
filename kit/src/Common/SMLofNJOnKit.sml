structure SMLofNJ =
  struct
    structure SysInfo =
      struct
	fun getHostArch() = "X86"  (*"HPPA"*)
	fun getOSName() = "Linux"  (*"HPUX"*)
      end
    fun exportFn(kitbinkit_path,kitexe) = print "SMLofNJ.exportFn not implemented"
  end