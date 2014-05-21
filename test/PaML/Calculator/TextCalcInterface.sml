structure TextCalcInterface :> CalcInterface =
struct
   fun println s = (print s; print "\n")

   datatype event =
	    DIGIT of int
	  | ENTER
	  | CLEAR
	  | PLUS
	  | MINUS
	  | MULTIPLY
	  | DIVIDE
(*      | DOT
*)
   open TextIO

   fun getEvent () =
	case input stdIn of
	    "e\n" => ENTER
	  | "c\n" => CLEAR
	  | "+\n" => PLUS
	  | "-\n" => MINUS
	  | "*\n" => MULTIPLY
	  | "/\n" => DIVIDE
	  | ""    => Process.exit Process.success 
	  | s     => case Int.fromString s of
			 SOME n => DIGIT n
		       | NONE   => ( println "Wrong input try again"
				   ; getEvent())
   fun drop1() = ignore(input1 stdIn)

   fun getEvent() =
       case input1 stdIn of
	   SOME #"e" => ENTER before drop1()
	 | SOME #"c" => CLEAR before drop1()
	 | SOME #"+" => PLUS before drop1()
	 | SOME #"-" => MINUS before drop1()
	 | SOME #"*" => MULTIPLY before drop1()
	 | SOME #"/" => DIVIDE before drop1()
	 | SOME c    => if Char.isDigit c then
                            DIGIT(ord c - 48) before drop1()
			else ( println "Wrong input try again"
		             ; drop1()
			     ; getEvent())
	 | NONE      => Process.exit Process.success 


   fun displayLines sl =
       ( println "displayLines:"
       ; app println sl
       ; println "---"
       )
    
   fun displayAccum s =
       ( println "displayAccum:"
       ; println s
       ; println "---"
       )


   (* Error signaling functions *)
   fun beep() = println "BEEEEEP"

   fun internalError s =
       ( println "INTERNALERROR:"
       ; println s
       )
end
