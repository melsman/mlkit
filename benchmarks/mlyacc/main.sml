(* main.sml
 *)

structure Main =
  struct
    val s = OS.FileSys.getDir()
    fun doit() = ParseGen.parseGen(s^"/mlyacc/DATA/ml.grm")
    fun testit _ = ParseGen.parseGen(s^"/mlyacc/DATA/ml.grm")
  end

fun loop n f = if n <= 0 then ()
               else (f(); loop (n-1) f)

val _ = loop 10 Main.doit
