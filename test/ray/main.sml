(* main.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Main structure for running raytracer as benchmark.
 *)

structure Main =
  struct

    fun files_eq f1 f2 =
      let val is1 = TextIO.openIn f1
	  val is2 = TextIO.openIn f2
	  val N = 50
	  fun loop() =
	    let 
	      val (s,b) =
		let val v1 = TextIO.inputN (is1,N)
		    val v2 = TextIO.inputN (is2,N)
		in (size v1, v1 = v2)
		end
	    in 
	      if b then
		if s < N then true
		else loop()
	      else false
	    end
      in loop() before 
	(TextIO.closeIn is1; TextIO.closeIn is2)
      end

    fun doit () = let
	  val strm = TextIO.openIn "ray/input"
	  in
	    Interface.rtInit();
	    Interp.parse strm;
	    TextIO.closeIn strm;
            print(if files_eq "ray/output" "ray/DATA/TEST" then "Ok\n" 
		  else "Error\n")
	  end

    fun testit _ = ()

  end

val _ = Main.doit()

