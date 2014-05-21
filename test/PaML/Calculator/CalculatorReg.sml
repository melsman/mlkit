functor Calculator (CI : CalcInterface) :> sig val main : int -> unit end =
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
    fun init()  = []

    fun enter (s as (e::_)) = e::s
      | enter s             = (CI.beep(); s)

    fun clear _ = init()

    fun binop (opr, (x::y::s)) = opr(x,y) :: s
      | binop (_, s)           = (CI.beep(); s)

    fun plus s     = binop (op+, s)
    fun minus s    = binop (op-, s)
    fun multiply s = binop (op*, s)
    fun divide s   = binop (op div, s)

    fun show sl = CI.displayLines(map Int.toString sl)
    fun accum i = CI.displayAccum(Int.toString i)

    datatype event = datatype CI.event

    fun copyE ENTER     = ENTER
      | copyE CLEAR     = CLEAR
      | copyE PLUS      = PLUS
      | copyE MINUS     = MINUS
      | copyE MULTIPLY  = MULTIPLY
      | copyE DIVIDE    = DIVIDE
      | copyE (DIGIT i) = DIGIT i
(*
    
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
*)
    local	    


	fun pLoop(arg as (state, event)) =
	    let fun loop fnk = 
		    pLoop 
		    let val state = fnk state
		    in  show state
		      ; (state, CI.getEvent())
		    end
	    in 
		case event of
		    ENTER    => loop enter
		  | CLEAR    => loop clear
		  | PLUS     => loop plus
		  | MINUS    => loop minus
		  | MULTIPLY => loop multiply
		  | DIVIDE   => loop divide
		  | _        => arg
	    end

	fun gnLoop(arg as ((state, event), acc)) =
	    case event of
		DIGIT i => gnLoop
		           let val acc = acc*10 + i
			   in  accum acc
			     ; ((state, CI.getEvent()), acc)
			   end
	      | ENTER   => let val state = push acc state
			   in  show state
			     ; ((state, CI.getEvent()), acc)
			   end
	      | _       => arg
			
	and getNum arg =
	
	    let 	val res = #1(gnLoop (arg,0))
	    in  process  res
	    end

	and  process arg =
	    let val res = pLoop arg
	    in  getNum res
	    end
    in


    fun main i = 
	let val init = init()
	    val ()   = show init
	    val res  = process(init, CI.getEvent())
	in  ()
	end handle Internal => ()

    end


end
