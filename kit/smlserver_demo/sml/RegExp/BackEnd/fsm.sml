(* fsm.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * 
 * Non-deterministic and deterministic finite-state machines.
 *)


signature NFA = 
    sig

	exception SyntaxNotHandled

	structure IntSet : ORD_SET where type Key.ord_key = int

	type nfa

	val build : RegExpSyntax.syntax * int -> nfa
	val buildPattern : RegExpSyntax.syntax list -> nfa
	val start : nfa -> IntSet.set
	val move : nfa -> int * char -> IntSet.set
	val chars : nfa -> int -> char list
	val accepting : nfa -> int -> int option

	val print : nfa -> unit
    end

structure Nfa : NFA = 
    struct 

	exception SyntaxNotHandled

	datatype move = Move of int * char option * int

	fun compareCharOption (NONE,NONE) = EQUAL
	  | compareCharOption (NONE,SOME (c)) = LESS
	  | compareCharOption (SOME(c),NONE) = GREATER
	  | compareCharOption (SOME(c),SOME(c')) = Char.compare (c,c')

	structure S = RegExpSyntax
	structure IntSet = 
	    ListSetFn (struct 
			   type ord_key = int 
			   val compare = Int.compare 
		       end)
	structure Int2Set = 
	    ListSetFn (struct
			   type ord_key = int * int
			   fun compare ((i1,i2),(j1,j2)) = 
			       case (Int.compare (i1,j1))
				 of EQUAL => Int.compare (i2,j2)
				  | v => v
		       end)
	structure MoveSet = 
	    ListSetFn (struct 
			   type ord_key = move 
			   fun compare (Move (i,c,j),Move (i',c',j')) =
			       (case (Int.compare (i,i'))
				    of EQUAL => 
					(case (compareCharOption (c,c')) 
					     of EQUAL => Int.compare (j,j')
					      | v => v)
				     | v => v)
		       end)
	structure CharSet = 
	    ListSetFn (struct
			   type ord_key = char
			   val compare = Char.compare
		       end)

	structure I = IntSet
	structure I2 = Int2Set
	structure M = MoveSet
	structure C = CharSet
	    
	(* create sets from lists *)
	fun iList l = I.addList (I.empty,l)
	fun mList l = M.addList (M.empty,l)

	datatype nfa = Nfa of {states : I.set,
			       moves : M.set,
			       accepting : I2.set}

	fun print (Nfa {states,moves,accepting}) = 
	    let val pr = TextIO.print
		val prI = TextIO.print o Int.toString
		val prI2 = TextIO.print o (fn (i1,i2) => (Int.toString i1))
		val prC = TextIO.print o Char.toString
	    in
		pr ("States: 0 -> ");
		prI (I.numItems (states)-1);
		pr "\nAccepting:";
		I2.app (fn k => (pr " "; prI2 k)) accepting;
		pr "\nMoves\n";
		M.app (fn (Move (i,NONE,d)) => (pr " ";
						prI i;
						pr " --@--> ";
						prI d;
						pr "\n")
	                | (Move (i,SOME c,d)) => (pr " ";
						  prI i;
						  pr " --";
						  prC c;
						  pr "--> ";
						  prI d;
						  pr "\n")) moves
	    end

	fun nullAccept n = Nfa {states=iList [0,1], moves=M.add (M.empty, Move (0,NONE,1)),
			        accepting=I2.singleton (1,n)}
	fun nullRefuse n = Nfa {states=iList [0,1], moves=M.empty,
				accepting=I2.singleton (1,n)}

	fun renumber n st = n + st
	fun renumberMove n (Move (s,c,s')) = Move (renumber n s, c, renumber n s')
 	fun renumberAcc n (st,n') = (n+st,n')

	fun build' n (S.Group e) = build' n e
	  | build' n (S.Alt l) = 
	      foldr (fn (Nfa {states=s1,
			      moves=m1,...},
			 Nfa {states=s2,
			      moves=m2,...}) => 
		     let val k1 = I.numItems s1
			 val k2 = I.numItems s2
			 val s1' = I.map (renumber 1) s1
			 val s2' = I.map (renumber (k1+1)) s2
			 val m1' = M.map (renumberMove 1) m1
			 val m2' = M.map (renumberMove (k1+1)) m2
		     in
			 Nfa {states=I.addList (I.union (s1',s2'),
						[0,k1+k2+1]),
			      moves=M.addList (M.union (m1',m2'),
					       [Move (0,NONE,1),
						Move (0,NONE,k1+1),
						Move (k1,NONE,k1+k2+1),
						Move (k1+k2,NONE,k1+k2+1)]),
			      accepting=I2.singleton (k1+k2+1,n)}
		     end)
	            (nullRefuse n) (map (build' n) l)
	  | build' n (S.Concat l) = 
	      foldr (fn (Nfa {states=s1,moves=m1,...},
			 Nfa {states=s2,moves=m2,accepting}) =>
		     let val k = I.numItems s1 - 1
			 val s2' = I.map (renumber k) s2
			 val m2' = M.map (renumberMove k) m2
			 val accepting' = I2.map (renumberAcc k) accepting
		     in
			 Nfa {states=I.union (s1,s2'),
			      moves=M.union (m1,m2'),
			      accepting=accepting'}
		     end)
	            (nullAccept n) (map (build' n) l)
	  | build' n (S.Interval (e,n1,n2)) = raise SyntaxNotHandled
	  | build' n (S.Option e) = build' n (S.Alt [S.Concat [], e])
	  | build' n (S.Plus e) = 
	      let val (Nfa {states,moves,...}) = build' n e
		  val m = I.numItems states
	      in
		  Nfa {states=I.add (states,m),
		       moves=M.addList (moves, [Move (m-1,NONE,m),
						Move (m-1,NONE,0)]),
		       accepting=I2.singleton (m,n)}
	      end
	  | build' n (S.Star e) = build' n (S.Alt [S.Concat [], S.Plus e])
          | build' n (S.MatchSet s) = 
	      if (S.CharSet.isEmpty s) then nullAccept (n)
	      else
		  let val moves = S.CharSet.foldl (fn (c,moveSet) => M.add (moveSet,Move (0,SOME c,1)))
		                                  M.empty s
		  in
		      Nfa {states=iList [0,1],
			   moves=moves,
			   accepting=I2.singleton (1,n)}
		  end
	  | build' n (S.NonmatchSet s) = 
	      let val moves = S.CharSet.foldl (fn (c,moveSet) => M.add (moveSet,Move (0,SOME c,1)))
		                              M.empty (S.CharSet.difference (S.allChars,s))
	      in
		  Nfa {states=iList [0,1],
		       moves=moves,
		       accepting=I2.singleton (1,n)}
	      end
	  | build' n (S.Char c) = Nfa {states=iList [0,1],
				       moves=M.singleton (Move (0,SOME c,1)),
				       accepting=I2.singleton (1,n)}
	  | build' n (S.Begin) = raise SyntaxNotHandled
	  | build' n (S.End) = raise SyntaxNotHandled


	fun build (r,n) = let val (Nfa {states,moves,accepting}) = build' n r
			  (* Clean up the nfa to remove epsilon moves.
			   * A simple way to do this:
			   * 1. states={0}, moves={}
			   * 2. for every s in states,
			   * 3.   compute closure(s)
			   * 4.   for any move (i,c,o) with i in closure (s)
			   * 5.       add move (0,c,o) to moves
			   * 6.       add state o to states
			   * 7. repeat until no modifications to states and moves
			   *)
			  in
			      Nfa {states=states, moves=moves, accepting=accepting}
			  end

	fun buildPattern rs = 
	    let fun loop ([],_) = []
		  | loop (r::rs,n) = (build (r,n))::(loop (rs,n+1))
		val rs' = loop (rs,0)
		val renums = foldr (fn (Nfa {states,...},acc) => 1::(map (fn k=>k+I.numItems states) 
								     acc)) [] rs'
		val news = ListPair.map (fn (Nfa {states,moves,accepting},renum) =>
					      let val newStates=I.map (renumber renum) states
						  val newMoves=M.map (renumberMove renum) moves
						  val newAcc=I2.map (renumberAcc renum) accepting
					      in
						  Nfa{states=newStates,
						      moves=newMoves,
						      accepting=newAcc}
					      end) (rs',renums)
		val (states,moves,accepting) = foldl (fn (Nfa{states,moves,accepting},(accS,accM,accA))=>
						      (I.union (states,accS),
						       M.union (moves,accM),
						       I2.union (accepting,accA)))
		                                     (I.singleton 0,
						      M.addList (M.empty,
								 map (fn k => Move (0,NONE,k)) renums),
						      I2.empty) news
	    in
		Nfa {states=states,moves=moves,accepting=accepting}
		
	    end		
		      
	fun accepting (Nfa {accepting,...}) state = 
	    let val item = I2.find (fn (i,_) => (i=state)) accepting
	    in
		case item
		  of NONE => NONE
		   | SOME (s,n) => SOME (n)
	    end

	(* Compute possible next states from orig with character c *)
	fun oneMove (Nfa {moves,...}) (orig,char) = 
	      M.foldr (fn (Move (_,NONE,_),set) => set
	                | (Move (or,SOME c,d),set) => 
		             if (c=char) andalso (or=orig) 
				 then I.add (set,d)
			     else set)
	              I.empty moves

	fun closure (Nfa {moves,...}) origSet =
	    let fun addState (Move (orig,NONE,dest),(b,states)) =
		      if (I.member (states,orig) andalso
			  not (I.member (states,dest)))
			  then (true,I.add (states,dest))
		      else (b,states)
		  | addState (_,bs) = bs
		fun loop (states) = 
		    let val (modified,new) = M.foldr addState
			                             (false,states) moves
		    in
			if modified
			    then loop (new) 
			else new 
		    end
	    in
		loop (origSet)
	    end
	
	fun move nfa =
	    let val closure = closure nfa
		val oneMove = oneMove nfa
	    in
		closure o oneMove
	    end

	fun start nfa = closure nfa (I.singleton 0)

	fun chars (Nfa{moves,...}) state = let
	      fun f (Move(s1, SOME c, s2), s) =
		      if (s1 = state) then C.add(s, c) else s
		| f (_, s) = s
	      in
		C.listItems (M.foldl f C.empty moves)
	      end

    end

signature DFA = 
    sig

	exception SyntaxNotHandled
	
	type dfa

	val build : RegExpSyntax.syntax -> dfa
	val buildPattern : RegExpSyntax.syntax list -> dfa
	val move : dfa -> int * char -> int option
	val accepting : dfa -> int -> int option
	val canStart : dfa -> char -> bool

    end

structure Dfa : DFA = 
    struct

	exception SyntaxNotHandled

	datatype move = Move of int * char option * int

	fun compareCharOption (NONE,NONE) = EQUAL
	  | compareCharOption (NONE,SOME (c)) = LESS
	  | compareCharOption (SOME(c),NONE) = GREATER
	  | compareCharOption (SOME(c),SOME(c')) = Char.compare (c,c')

	structure N = Nfa
	structure IntSet = N.IntSet
	structure IntSetSet = 
	    ListSetFn (struct
			   type ord_key = IntSet.set
			   val compare = IntSet.compare
		       end)
	structure Int2Set = 
	    ListSetFn (struct
			   type ord_key = int * int
			   fun compare ((i1,i2),(j1,j2)) = 
			       case (Int.compare (i1,j1))
				 of EQUAL => Int.compare (i2,j2)
				  | v => v
		       end)
	structure MoveSet = 
	    ListSetFn (struct 
			   type ord_key = move 
			   fun compare (Move (i,c,j),Move (i',c',j')) =
			       (case (Int.compare (i,i'))
				  of EQUAL => 
				      (case (compareCharOption (c,c')) 
					 of EQUAL => Int.compare (j,j')
					  | v => v)
				   | v => v)
		       end)
        structure CharSet = 
	    ListSetFn (struct
			   type ord_key = char
			   val compare = Char.compare
		       end)

        structure IS = IntSetSet
        structure I = IntSet
	structure I2 = Int2Set
        structure M = MoveSet
	structure C = CharSet
	structure A2 = Array2
	structure A = Array
	structure Map = ListMapFn (struct
				       type ord_key = IntSet.set
				       val compare = IntSet.compare
				   end)
	    
        (* create sets from lists *)
        fun iList l = I.addList (I.empty,l)
	fun mList l = M.addList (M.empty,l)

	datatype dfa = Dfa of {states : I.set,
			       moves : M.set,
			       accepting : I2.set,
			       table : int option A2.array,
			       accTable : (int option) A.array,
			       startTable : bool A.array}

	fun print (Dfa {states,moves,accepting,...}) = 
	    let val pr = TextIO.print
		val prI = TextIO.print o Int.toString
		val prI2 = TextIO.print o (fn (i1,i2) => Int.toString i1)
		val prC = TextIO.print o Char.toString
	    in
		pr ("States: 0 -> ");
		prI (I.numItems (states)-1);
		pr "\nAccepting:";
		I2.app (fn k => (pr " "; prI2 k)) accepting;
		pr "\nMoves\n";
		M.app (fn (Move (i,NONE,d)) => (pr " ";
						prI i;
						pr " --@--> ";
						prI d;
						pr "\n")
	                | (Move (i,SOME c,d)) => (pr " ";
						  prI i;
						  pr " --";
						  prC c;
						  pr "--> ";
						  prI d;
						  pr "\n")) moves
	    end


	fun move' moves (i,c) = 
	    (case (M.find (fn (Move (s1,SOME c',s2)) =>
			   (s1=i andalso c=c'))
		   moves)
	       of NONE => NONE
		| SOME (Move (s1,SOME c',s2)) => SOME s2)
(*	fun move (Dfa {moves,...}) (i,c) = move' moves (i,c) *)
	fun move (Dfa {table,...}) (i,c) = A2.sub (table,i,ord(c)-ord(Char.minChar))

	fun accepting' accepting i = I2.foldr (fn ((s,n),NONE) => if (s=i) 
								      then SOME(n)
								  else NONE
                                                | ((s,n),SOME(n')) => if (s=i)
									  then SOME(n)
								      else SOME(n'))
	                                      NONE accepting
(*	fun accepting (Dfa {accepting,...}) i = accepting' accepting i *)
	fun accepting (Dfa {accTable,...}) i = A.sub (accTable,i)

	fun canStart (Dfa {startTable,...}) c = A.sub (startTable,ord(c))

	fun build' nfa = 
	    let val move = N.move nfa
		val accepting = N.accepting nfa
		val start = N.start nfa
		val chars = N.chars nfa
		fun getAllChars (ps) = 
		    I.foldl
		    (fn (s,cs) => C.addList (cs,chars s))
		    C.empty ps
		val initChars = getAllChars (start)
		fun getAllStates (ps,c) = 
		    I.foldl
		    (fn (s,ss) => I.union (ss,move (s,c)))
		    I.empty ps
		fun loop ([],set,moves) = (set,moves)
		  | loop (x::xs,set,moves) = 
		    let val cl = getAllChars (x)
			val (nstack,sdu,ml) = 
			    C.foldl
			    (fn (c,(ns,sd,ml)) =>
			     let val u = getAllStates (x,c)
			     in
				 if (not (IS.member (set,u))
				     andalso (not (IS.member (sd,u))))
				     then (u::ns,
					   IS.add (sd,u),
					   (x,c,u)::ml)
				 else (ns,sd,(x,c,u)::ml)
			     end) ([],IS.empty,[]) cl
		    in
			loop (nstack@xs,IS.union(set,sdu),ml@moves)
		    end
		val (sSet,mList) = loop ([start],IS.singleton (start), [])
		val num = ref 1
		fun new () = let val n = !num
			     in
				 num := n+1 ; n
			     end
		val sMap = Map.insert (Map.empty, start, 0)
		val sSet' = IS.delete (sSet,start)
		val sMap = IS.foldl (fn (is,map) => Map.insert (map,is,new ()))
		                    sMap sSet'
		val states = I.addList (I.empty,List.tabulate(!num,fn x => x))
		val moves = M.addList (M.empty,
				       map (fn (is1,c,is2) =>
					    Move (valOf (Map.find (sMap,is1)),
						  SOME c,
						  valOf (Map.find (sMap,is2))))
				           mList)
		(* Given a set of accepting states, look for a given state,
		 * with the minimal corresponding pattern number
		 *)
		fun minPattern accSet = let val l = map (valOf o accepting) (I.listItems accSet)
					    fun loop ([],min) = min
					      | loop (n::ns,min) = 
						           if (n<min) then loop (ns,n)
							   else loop (ns,min)
					in
					    loop (tl(l),hd(l))
					end
		val accept = IS.foldl (fn (is,cis) =>
				       let val items = I.filter (fn k => 
								 case (accepting k)
								     of SOME _ => true
								      | NONE => false) is
				       in
					   if (I.isEmpty items) 
					       then cis
					   else 
					       I2.add (cis,(valOf (Map.find (sMap,is)),
							    minPattern items))
				       end) I2.empty sSet
		val table = A2.tabulate A2.RowMajor (!num, 
					 ord(Char.maxChar)-ord(Char.minChar)+1,
					 fn (s,c) => move' moves (s,chr(c+ord(Char.minChar))))
		val accTable = A.tabulate (!num, 
					   fn (s) => accepting' accept s)
		val startTable = A.tabulate (ord(Char.maxChar)-
					     ord(Char.minChar)+1,
					     fn (c) => C.member (initChars,
								 chr(c+ord(Char.minChar))))
	    in
		Dfa {states=states,moves=moves,accepting=accept,
		     table=table,accTable=accTable,startTable=startTable}
	    end
	
	fun build r = build' (N.build (r,0))
		  
	fun buildPattern rs = build' (N.buildPattern rs)


    end
