structure SMLofNJ =
  struct
    structure SysInfo =
      struct
	fun getHostArch() = "X86"
	fun getOSName() = "Linux"
(*	fun getOSName() = "BSD" *)
      end
    fun exportFn(kitbinkit_path,kitexe) = print "SMLofNJ.exportFn not implemented"
  end