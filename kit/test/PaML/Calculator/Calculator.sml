functor Calculator (CI : CalcInterface) (*<: PalmMain*) =
struct
    exception Internal
    fun ierror msg = ( CI.internalError msg
                     ; raise Internal)


    type 'a stack = 'a list
    fun push e s = e :: s
    fun pop (e::s) = SOME(e,s)
      | pop _      = NONE
    fun copy []       = nil
      | copy (e :: s) = e :: copy s

    type state = int stack

    (* State manipulation functions *)
    fun enter (e::s) = e::e::s
      | enter s      = (CI.beep(); s)

    fun clear _ = [0]

    fun binop opr (x::y::s) = opr(x,y) :: s
      | binop _   s         = (CI.beep(); s)

    fun init()  = []

    fun show sl = CI.displayLines(map Int.toString sl)
    fun accum i = CI.displayAccum(Int.toString i)

    datatype event = datatype CI.event

    fun getNum (state, acc) =
	case CI.getEvent() of
	    DIGIT i => let val acc = acc*10 + i
		       in  accum acc
                         ; getNum(state, acc)
		       end
	  | ENTER   => let val state = push acc state
		       in  show state
                         ; process(state, CI.getEvent())
		       end
	  | e       => process(push acc state, e)

    and process (state, event) =
	let fun next fnk = 
                let val state = fnk state
		in  show state
                  ; process(state, CI.getEvent())
		end
	in 
	    case event of
	    ENTER    => next enter
	  | CLEAR    => next clear
	  | PLUS     => next (binop op+)
	  | MINUS    => next (binop op-)
	  | MULTIPLY => next (binop op* )
	  | DIVIDE   => next (binop op div)
	  | DIGIT n  => ( show (push n state)
                        ; getNum (state, n))
	end

    fun main i = 
	let val init = init()
	in  show init
          ; process(init, CI.getEvent())
	end handle Internal => ()
end
