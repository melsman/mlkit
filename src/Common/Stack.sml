
functor Stack() : STACK =
  struct
    type '_a stack = ('_a list) ref
      
    exception EmptyStack

    fun empty () = 
      (* Return an empty stack (must be typed somehow) *)
      (ref []) : '_a stack

    fun push (s : '_a stack, e: '_a) =
      (* Push an element onto the stack *)
      s := e :: (!s)
      
    fun peek (s : '_a stack) =
      (* Take a look at the top element of the stack *)
      (hd (!s))
      handle Hd => raise EmptyStack

    fun drop (s: '_a stack) =
      (* Remove top element from the stack *)
      (s := tl (!s); ())
      handle Tl => raise EmptyStack

    fun pop (s : '_a stack) =
      (* Return and remove the top element of the stack *)
      let
	val res = peek s
      in
	drop s;
	res
      end
    
    fun clear (s : '_a stack) =
      (* Remove all elements from the stack *)
      s := !(empty ())
  end
