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
    
    fun getNum (arg as (state, event, acc)) =
	let fun tailreg a = if true then a else arg
	in
	    case event of
		DIGIT i => getNum
		           let val acc = acc*10 + i
			   in  accum acc
			     ; tailreg(state, CI.getEvent(),acc)
			   end
	      | ENTER   => process
			   let val state = push acc state
			   in  show state
			     ; tailreg(state, CI.getEvent(),0)
			   end
	      | e       => process(tailreg(push acc state, e,0))
	end

    and process (arg as (state, event,_)) =
	let fun tailreg a = if true then a else arg
	    fun next fnk = 
                process 
		let val state = fnk state
		in  show state
                  ; tailreg (state, CI.getEvent(),0)
		end
	in 
	    case event of
	    ENTER    => next enter
	  | CLEAR    => next clear
	  | PLUS     => next (binop op+ )
	  | MINUS    => next (binop op- )
	  | MULTIPLY => next (binop op* )
	  | DIVIDE   => next (binop op div)
	  | _        => getNum (tailreg(state, event,0))
	end

    fun main i = 
	let val init = init()
	in  show init
          ; process(init, CI.getEvent(),0)
	end handle Internal => ()
end
