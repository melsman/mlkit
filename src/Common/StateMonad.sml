(*$StateMonad: STATE STATE_MONAD
 *)

functor FunctionalStateMonad(structure State : STATE)
	  : sig 
	      include STATE_MONAD
              sharing type State.state = state
            end =
struct 
  open State
  infix plus

  type 'a S = state -> ('a * state)

  fun unitS (x: 'a) : 'a S = fn s => (x,s)
  fun bindS(m,k) = fn s => let val (a, s') = m s 
                               val (b,s'') = k a s'
			    in (b,s'')
			    end
  infix bindS

  fun mapS (f: 'a -> 'b) (m: 'a S) : 'b S = 
    m bindS (fn a => unitS (f a))

  fun joinS z = z bindS (fn m => m)

  fun ooS (f,g) a = 
    fn s => 
    let val g' = g a 
        val (b,s') = g' s
	val f' = f b
        val (c,s'') = f' s'
    in (c,s'')
    end
  infix ooS

  fun mapList (f : 'a -> 'b S) (xs: 'a list) : 'b list S =
    let
      val xs' = map f xs
    in
      List.foldR (fn (m: 'b S) => fn (mres: 'b list S) => 
	          mres bindS (fn bs => m bindS (fn b => unitS(b::bs))))
	         (unitS [])
		 xs'
    end

  fun resetS (m : unit S) : unit S = m bindS (fn () => fn s => ((),initialState))

  fun getS m = m bindS (fn () => fn s => (s,s))

  fun plusS m = m bindS (fn s => fn s' => ((), s' plus s))
 
  fun setS m = m bindS (fn s => fn s' => ((),s))

  fun showS s (m : 'a S) : 'a * state = (m s)
end;

functor ImperativeStateMonad(structure State : STATE)
	  : sig 
	      include STATE_MONAD
              sharing type State.state = state
            end =
struct 
  open State
  infix plus

  type 'a S = 'a

  val state = ref initialState

  fun unitS (x: 'a) = x

  fun bindS (m,k) = k m
  infix bindS

  fun mapS f m = m bindS (fn a => unitS (f a))

  fun joinS z = z bindS (fn m => m)

  fun ooS (f: 'b -> 'c S, g : 'a -> 'b S) = f o g
  infix ooS

  val mapList = map

  fun resetS (m: unit S) : unit S = state := initialState;

  fun getS (m: unit S) : state S = !state 

  fun plusS (m: state S) : unit S = state := ((!state) plus m)

  fun setS (m : state S) : unit S = state := m

  fun showS s (m : 'a S) : 'a * state = (m,!state)
end;

