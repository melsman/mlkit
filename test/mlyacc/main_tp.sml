(* main.sml
 *)

structure Main =
  struct
    val s = OS.FileSys.getDir()
    fun doit() = ParseGen.parseGen(s^"/mlyacc/DATA/ml.grm")
    fun testit _ = ParseGen.parseGen(s^"/mlyacc/DATA/ml.grm")
  end

fun doit 0 = ()
  | doit n = (Main.doit(); doit (n-1))

val _ = doit 20