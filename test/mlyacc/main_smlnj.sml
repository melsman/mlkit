(* main.sml
 *)

structure Main =
  struct
    val s = OS.FileSys.getDir()
    fun doit() = ParseGen.parseGen(s^"/mlyacc/DATA/ml.grm")
    fun testit _ = ParseGen.parseGen(s^"/mlyacc/DATA/ml.grm")
  end

val doit = Main.doit