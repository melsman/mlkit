(* Copyright (c) 2001, Ken Friis Larsen, Martin Elsman *)
(* NFA implementation of regular expression matching by Ken Friis
 * Larsen. Support for regular expression classes and parenthesis
 * extraction by Martin Elsman. mael 2001-09-29. *)

structure RegExp :> REG_EXP =
struct

structure Set :> 
sig type set 
    val union : set * set -> set
    val empty : set
    val sing  : int -> set
    val insert: int * set -> set
    val fold  : (int * 'a -> 'a) -> 'a -> set -> 'a
    val mem   : set -> int -> bool
    val list  : set -> int list
end =
struct
    type set        = Intset.intset
    val union       = Intset.union
    val empty       = Intset.empty
    val sing        = Intset.singleton
    fun insert(i,s) = union(sing i,s)
    val fold        = Intset.foldr
    fun mem s i     = Intset.member(s,i)
    fun list a      = fold (op ::) nil a
end

structure Map :>
sig
    type 'a map
    exception NotFound
    val empty  : unit -> 'a map
    val insert : 'a map -> int -> 'a -> 'a map
    val lookup : 'a map -> int -> 'a
    val fold   : (int * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
end =
struct
    type 'a map = 'a Intmap.intmap
    exception NotFound
    fun insert m i v      = Intmap.insert(m,i,v)
    val empty = Intmap.empty
    fun lookup m i = case Intmap.peek(m,i) of
                         SOME x => x
                       | NONE   => raise NotFound
    val fold = Intmap.foldl
end

datatype 'a atom =
    Simple of 'a -> bool
  | Unique (* a unique character used in the algorithme from the 
	      Dragon book chapter 3.9 *)

datatype 'a regexp_ =
    Atom   of 'a atom
  | Alt    of 'a regexp_ * 'a regexp_
  | Concat of 'a regexp_ * 'a regexp_
  | Iter   of 'a regexp_
  | Par    of 'a regexp_
  | Epsilon

(* Until I make something better ... *)
type regexp = char regexp_

fun add m i v = Map.insert m i (Set.union(Map.lookup m i, v)
                handle Map.NotFound => v)


(* This is the first step of the algorithm from the Dragon chapter 3.9
   Do a depth-first-traversal of the regexp:
   The arguments n, ass, fol are all accumulating arguments:
             n   is the next number to be assigned to an Atom
             ass is a mapping from a position to an Atom
             fol is the follow mapping
   
   [dft n ass fol reg] returns the tuple (null,first,last,n',ass',fol'), 
   where
          null denotes whether reg is nullable
	  first is the firstpos set for reg
	  last is the lastpos set for reg
	  n', ass', fol' as above
*)

fun dft n ass fol Epsilon = 
    (true , Set.empty , Set.empty, n, ass, fol) 
  | dft n ass fol (Par r) = dft n ass fol r
  | dft n ass fol (Atom a) = 
    (false, Set.sing n, Set.sing n, n+1, Map.insert ass n a, fol)
  | dft n ass fol (Alt(r1,r2)) =
    let val (null1,first1,last1,n,ass,fol) = dft n ass fol r1
	val (null2,first2,last2,n,ass,fol) = dft n ass fol r2
    in  
	(null1 orelse null2, Set.union(first1,first2), 
	 Set.union(last1,last2), n, ass, fol)
    end
  | dft n ass fol (Concat(r1,r2)) =
    let val (null1,first1,last1,n,ass,fol) = dft n ass fol r1
	val (null2,first2,last2,n,ass,fol) = dft n ass fol r2
    in  
	(null1 andalso null2, 
	 if null1 then Set.union(first1,first2) else first1,
	 if null2 then Set.union(last1,last2)   else last2, 
	 n, ass,
	 Set.fold (fn(i,m) => add m i first2) fol last1)
    end
  | dft n ass fol (Iter r) = 
    let val (_,first,last,n,ass,fol) = dft n ass fol r
    in  
	(true, first, last, n, ass,
	 Set.fold (fn(i,m) => add m i first) fol last) 
    end

(* Here is an extended version of the depth-first-traversal, which
 * also collects information about parentheses. The extra argument
 * `par' to dft is a pair (pL,pR) of two lists of node-pairs, where
 * pL denotes left-parentheses and pR denotes right-parentheses.
 *)

fun dftp n ass fol par Epsilon = 
    (true , Set.empty , Set.empty, n, ass, fol, par, (nil,nil)) 
  | dftp n ass fol par (Par r) = 
    let val (null,first,last,n,ass,fol,par,(Pl,Pr)) = dftp n ass fol par r
        val Pl = Set.list first @ Pl
        val Pr = Set.list last @ Pr
    in (null, first, last, n, ass, fol, par, (Pl,Pr))
    end
  | dftp n ass fol par (Atom a) = 
    (false, Set.sing n, Set.sing n, n+1, Map.insert ass n a, fol, par, (nil,nil))
  | dftp n ass fol par (Alt(r1,r2)) =
    let val (null1,first1,last1,n,ass,fol,par,(Pl1,Pr1)) = dftp n ass fol par r1
	val (null2,first2,last2,n,ass,fol,par,(Pl2,Pr2)) = dftp n ass fol par r2
    in  
	(null1 orelse null2, Set.union(first1,first2), 
	 Set.union(last1,last2), n, ass, fol, par,
	 (Pl1 @ Pl2, Pr1 @ Pr2))
    end
  | dftp n ass fol par (Concat(r1,r2)) =
    let val (null1,first1,last1,n,ass,fol,par,(Pl1,Pr1)) = dftp n ass fol par r1
	val (null2,first2,last2,n,ass,fol,par,(Pl2,Pr2)) = dftp n ass fol par r2
	val par = (Set.fold (fn (l1,p) => map (fn a => (l1,a)) Pl2 @ p) (#1 par) last1,   (* case: re1(re2) *)
		   Set.fold (fn (f2,p) => map (fn a => (a,f2)) Pr1 @ p) (#2 par) first2)  (* case: (re1)re2 *)
	val Pl = if null1 then Pl1 @ Pl2 else Pl1
	val Pr = if null2 then Pr1 @ Pr2 else Pr2
    in  
	(null1 andalso null2, 
	 if null1 then Set.union(first1,first2) else first1,
	 if null2 then Set.union(last1,last2)   else last2, 
	 n, ass,
	 Set.fold (fn(i,m) => add m i first2) fol last1,
	 par, (Pl,Pr))
    end
  | dftp n ass fol par (Iter r) = 
    let val (_,first,last,n,ass,fol,par,(Pl,Pr)) = dftp n ass fol par r
        val par = (Set.fold (fn (l,p) => map (fn a => (l,a)) Pl @ p) (#1 par) last,
		   Set.fold (fn (f,p) => map (fn a => (a,f)) Pr @ p) (#2 par) first)
    in  
	(true, first, last, n, ass,
	 Set.fold (fn(i,m) => add m i first) fol last,
	 par, (Pl,Pr)) 
    end

(* This is the type of the particular kind of NFA mentioned 
   in the Dragon book chapter 3.9.
   start: is the set of startstates.
   trans: is the transition relation, if j is in trans[i] it means 
          there is an edge from i to j, and the symbol/predicate on
          the edge is edges[j].
   final: is the only accepting state. *)
type 'a NFA = {start : Set.set,
	       edges : 'a vector,
	       trans : Set.set vector,
	       final : int}

(* How to construct an NFA from a follow set. *)
fun nfa n ass fol start : char atom NFA =
  {start = start, 
   edges = Vector.tabulate (n,  Map.lookup ass),
   trans = Vector.tabulate (n-1, Map.lookup fol),
   final = n-1}

(* Type of NFA that supports parenthesis matching *)
type 'a NFAP = 
  {nfa: 'a NFA,
   leftpar: int list Vector.vector,   (* left parenthesis between node a and b if b \in leftpar[a] *)
   rightpar: int list Vector.vector}  (* right parenthesis between node a and b if b \in rightpar[a] *)

fun parvector n par =
  let val A = Array.array (n, nil)
      fun add (a,b) = Array.update(A, a, b :: Array.sub(A,a))
  in  app add par
    ; Array.extract(A,0,NONE)
  end

fun parnum v (a,b) : int =
  foldl (fn (x,i) => if x = b then i+1 else i) 0 (Vector.sub(v,a))
  handle _ => raise Fail "parnum"

fun nfap n ass fol start (leftpar,rightpar) : char atom NFAP =
  {nfa      = nfa n ass fol start,
   leftpar  = parvector n leftpar,
   rightpar = parvector n rightpar}

local 
  val sub = Vector.sub 
  infix 9 sub
  fun app (Simple f) a = f a
    | app _ _          = false
in

  (* The boolean matcher *)
  fun nextstate {start,edges,trans,final} states a =
    let 
      fun one(x,res) = 
	if app (edges sub x) a then Set.union(trans sub x,res) 
	else res
    in Set.fold one Set.empty states 
    end

  (* P maps states to pairs of a list of already extracted strings and
   * a stack of char lists, which denotes matched characters for
   * currently unmatched right-parentheses. *)

   fun parpop 0 p = p
     | parpop n (acc,[chs]) = parpop (n-1) (chs::acc, nil)
     | parpop n (acc,chs1::chs2::chss) = parpop (n-1) (chs1::acc, (chs1@chs2)::chss)
     | parpop _ _ = raise Fail "parpop"
     
   fun parpush 0 p = p
     | parpush n (acc,chss) = parpush (n-1) (acc,nil::chss)
     
   fun paradd a (p as (_,nil)) = p
     | paradd a (acc,chs::chss) = (acc,(a::chs)::chss)
     
   fun nextstate_par {nfa={start,edges,trans,final},leftpar,rightpar} (states,P0) a =
     let 
       fun one(x,(res,P)) = 
	 if app (edges sub x) a then 
	   let val new = trans sub x
	       val P = Set.fold (fn (n,P) =>
				 let val r = parnum rightpar (x,n)
				   val l = parnum leftpar (x,n)
				   val p = Map.lookup P0 x handle NotFound => raise Fail "one"
				   val p = paradd a p
				   val p = parpop r p
				   val p = parpush l p
				 in Map.insert P n p
				 end) P new
	   in (Set.union(new,res), P)
	   end
	 else (res,P)
     in  Set.fold one (Set.empty, Map.empty()) states end
end

local
  fun nfarun (nfa as {start,final,...} : char atom NFA) string =
    let val next = nextstate nfa
        val endstates = CharVector.foldl (fn(a,res) => next res a) start string
    in  Set.mem endstates final 
    end 
  fun translate reg = 
    let val (_,first,last,n,ass,fol) = dft 0 (Map.empty()) (Map.empty()) reg 
    in  nfa n ass fol first 
    end
in
  fun match re = nfarun (translate (Concat(re, Atom Unique)))
end

local
  fun count a nil n = n
    | count a (x::xs) n = count a xs (if a = x then n+1 else n)

  fun nfarun_par (nfa as {nfa={start,final,...},...} : char atom NFAP, (Pl,Pr)) string =
    let val next = nextstate_par nfa
        val P = Set.fold (fn (s,P) => 
			  let val p = (nil,nil)
			      val l = count s Pl 0
			      val p = parpush l p
			  in Map.insert P s p
			  end) (Map.empty()) start
        val (endstates,P) = CharVector.foldl (fn(a,res) => next res a) (start,P) string
    in  if Set.mem endstates final then SOME (rev(map(implode o rev) (#1(Map.lookup P final))))
	else NONE
    end 

  fun translate_par reg = 
    let val (_,first,last,n,ass,fol,par,P) = dftp 0 (Map.empty()) (Map.empty()) (nil,nil) reg 
    in  (nfap n ass fol first par,P) end
in
  fun extract re = nfarun_par (translate_par (Concat(re, Atom Unique)))
end
	 
local
(* simple top-down parser for regexp's
   the reserved characters are: |, *, (, ), $, ., ?, +
   to match a reserved character put a \ just before it.
*)
    fun parse_err s = raise Fail ("Parse error: " ^ s)

    datatype token =
	ALT | STAR | LPAR | RPAR | DOLLAR 
      | CONST of char | DOT 
      | QMARK | PLUS | LBRA | RBRA
	
    structure C = Char
    structure SS = Substring

    fun reserved c = case c of
      #"(" => true | #")" => true | #"|" => true
    | #"$" => true | #"*" => true | #"." => true
    | #"?" => true | #"+" => true | #"[" => true
    | #"]" => true | #"\\" => true | _ => false

    fun special c = case c of 
      #"t" => SOME #"\t" | #"n" => SOME #"\n" | #"v" => SOME #"\v"
    | #"f" => SOME #"\f" | #"r" => SOME #"\r" | _ => NONE

    fun get ss =
      case SS.getc ss of 
	NONE         => NONE
      | SOME (c, ss) =>
	  let 
	    val res =
	      case c of
		#"\\" => (case SS.getc ss of
			    SOME(c, ss') => 
			      if reserved c then (CONST c, ss')
			      else (case special c of
				      SOME c => (CONST c, ss')
				    | NONE => parse_err ("Non-escapable character: " ^ str c))
			  | _ => parse_err "Expecting escapable character at end of string")
	      | #"(" => (LPAR, ss) 
	      | #")" => (RPAR, ss)
	      | #"|" => (ALT, ss)
	      | #"$" => (DOLLAR, ss)
	      | #"*" => (STAR, ss)
	      | #"." => (DOT, ss)
	      | #"?" => (QMARK, ss)
	      | #"+" => (PLUS, ss)
	      | #"[" => (LBRA, ss)
	      | #"]" => (RBRA, ss)
	      | c => (CONST c, ss)
	  in SOME res
	  end

    fun alt ts = altopt(conc ts)
    and altopt (inp,ts) =
	case get ts of
	    SOME(ALT, ts) => let val (re1,ts) = conc ts
			     in altopt (Alt(inp,re1),ts) end
	  | _             => (inp,ts)
			     
    and conc ts = concopt(iter ts)
    and concopt (inp,ts) =
	let fun rule ts = let val (re1,ts) = iter ts
			  in  concopt (Concat(inp,re1),ts) end
	in
	    case get ts of
		SOME(LPAR   , _) => rule ts
	      | SOME(CONST c, _) => rule ts
	      | SOME(DOLLAR , _) => rule ts
	      | SOME(DOT    , _) => rule ts 
	      | SOME(LBRA   , _) => rule ts 
	      | _                => (inp,ts)
	end
    
    and iter ts = iteropt(atom ts)
    and iteropt (inp,ts) =
	case get ts of
	    SOME(STAR, ts) => iteropt (Iter inp,ts)
	  | SOME(PLUS, ts) => iteropt (Concat(inp,Iter inp),ts)
	  | SOME(QMARK, ts) => iteropt (Alt(inp,Epsilon),ts)
	  | _              => (inp, ts)
		
    and atom ts =
	case get ts of
	    SOME(LPAR   , ts) => 
            let val (re,ts) = alt ts
	    in case get ts 
		 of SOME(RPAR, ts) => (Par re, ts)
		  | _              => 
		   parse_err "unbalanced parentheses" 
	    end
	  | SOME(CONST c, ts) => (Atom(Simple (fn c2 => c = c2)), ts)
	  | SOME(DOLLAR , ts) => (Epsilon, ts)
	  | SOME(DOT    , ts) => (Atom(Simple (fn _ => true)), ts)
	  | SOME(LBRA   , ts) =>
	    let val (f,ts) = class ts
	    in (Atom(Simple f), ts)
	    end
	  | _                 => parse_err "expecting atom"
    and class ts =  (* examples: [0-9a-zA-Zæ], [^*] *)
      let 
	fun read_rng ts c1 =
	  case SS.getc ts of
	    SOME(#"-",ts) =>
	      (case SS.getc ts of
		 SOME(c2,ts) => 
		   if c1 <= c2 then SOME (fn c => c1 <= c andalso c <= c2, ts)
		   else parse_err "invalid character range"
		 | NONE => parse_err "expecting character after '-' in class")
	  | _ => NONE

	fun class_reserved c =
	  case c of
	    #"-" => true
	  | #"^" => true
	  | #"[" => true
	  | #"]" => true
	  | #"\\" => true
	  | _ => false

	fun read ts f =
	  case SS.getc ts of 
	    SOME (#"]" , ts) => (f,ts)
	  | SOME (#"\\", ts) => 
	      (case SS.getc ts of
		 SOME (c2, ts) => 
		   if class_reserved c2 then
		     read ts (fn c => f c orelse c = c2)
		   else 
		     (case special c2 of
			SOME c3 => read ts (fn c => f c orelse c = c3)
		      | NONE => parse_err "invalid character escape")
		   | NONE => parse_err "expecting character")
	  | SOME (c1   , ts) => 
	      if class_reserved c1 then 
		parse_err ("invalid character '" ^ str c1 ^ "' in class")
	      else (case read_rng ts c1 of
		      SOME (g,ts) => read ts (fn c => f c orelse g c)
		    | NONE => read ts (fn c => f c orelse c = c1))
	  | NONE => parse_err "expecting ']'"

      in
	case SS.getc ts of
	  SOME(#"^",ts) => 
	    let val (f,ts) = read ts (fn _ => false)
	    in (not o f, ts)
	    end
	| _ => read ts (fn _ => false)
      end

    fun parse s = let val (re,ts) = alt (SS.all s)
		  in case get ts of
                         NONE => re
                       | _    => parse_err "expecting atom"
		  end
in
    fun fromString s = parse s
end
end
