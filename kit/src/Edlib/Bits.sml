structure Bits =
  struct
    local
      exception Bits
      fun wrap f (i1:int,i2:int) : int =
	let val i1 = Word.fromInt i1
	    val i2 = Word.fromInt i2
	in Word.toInt (f(i1,i2))
	end handle Overflow => raise Bits
      fun wrap1 f (i:int) : int =
	let val i = Word.fromInt i
	in Word.toInt (f i)
	end handle Overflow => raise Bits
    in
      val xorb : int * int -> int = wrap Word.xorb
      val lshift : int * int -> int = wrap Word.<<
      val andb : int * int -> int = wrap Word.andb
      val notb : int -> int = wrap1 Word.notb
      val rshift : int * int -> int = wrap Word.>>
      val orb : int * int -> int = wrap Word.orb
    end
  end