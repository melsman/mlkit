(* main.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Main =
  struct
    val name = "Nucleic"
    fun doit () = print (concat[Int.toString (Nucleic.anticodon_length ()), "\n"])
    fun testit strm = TextIO.output(strm, concat[
	    Int.toString (Nucleic.anticodon_length ()), "\n"
	  ])
  end

val _ = Main.doit()