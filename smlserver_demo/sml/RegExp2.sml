(* NFA implementation of regular expression matching by Ken Friis
 * Larsen with modifications and extensions by Martin Elsman. *)

signature RegExp2 =
sig
    type regexp
    val match      : regexp -> string -> bool
    val fromString : string -> regexp 
end

(*

 Grammar:
    regexp ::= regexp1 "|" regexp2         regexp1 or regexp2
            |  regexp1 regexp2             regexp1 followed by regexp2
            |  regexp "*"                  regexp repeated zero or more times
            |  regexp "+"                  regexp repeated one or more times
            |  regexp "?"                  regexp zero or one time
            |  "(" regexp ")"              regexp
            |  c                           specific character
            |  "[" class "]"               character class
            |  "[^" class "]"              negated character class
            |  $                           empty string
            |  .                           any character

    class ::=  c class                     Specific character
            |  c1 "-" c2 class             Ascii character range
            
 Whitespace is significant.  Special characters can be escaped by \                 

*)

structure RegExp2 :> RegExp2 =
struct

structure Set :> 
sig type set 
    val union : set * set -> set
    val empty : set
    val sing  : int -> set
    val fold  : (int * 'a -> 'a) -> 'a -> set -> 'a
    val mem   : set * int -> bool
end =
struct
    type set  = Intset.intset
    val union = Intset.union
    val empty = Intset.empty
    val sing  = Intset.singleton
    val fold  = Intset.foldr
    val mem   = Intset.member
end

structure Map :>
sig
    type 'a map
    exception NotFound
    val empty  : unit -> 'a map
    val update : 'a map -> int -> 'a -> 'a map
    val lookup : 'a map -> int -> 'a
    val fold   : (int * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
end =
struct
    type 'a map = 'a Intmap.intmap
    exception NotFound
    fun update m i v      = Intmap.insert(m,i,v)
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
  | Epsilon

(* Until I make something better ... *)
type regexp = char regexp_

fun add m i v = Map.update m i (Set.union(Map.lookup m i, v)
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
	  fist is the firstpos set for reg
	  last is the lastpos set for reg
	  n', ass', fol' as above
*)

fun dft n ass fol Epsilon  = 
    (true , Set.empty , Set.empty, n, ass, fol) 
  | dft n ass fol (Atom a) = 
    (false, Set.sing n, Set.sing n, n+1, Map.update ass n a, fol)
  | dft n ass fol (Alt(r1,r2)) =
    let val (null1,first1,last1,n1,ass1,fol1) = dft n ass fol r1
	val (null2,first2,last2,n2,ass2,fol2) = dft n1 ass1 fol1 r2
    in  
	(null1 orelse null2, Set.union(first1,first2), 
	 Set.union(last1,last2), n2, ass2, fol2)
    end
  | dft n ass fol (Concat(r1,r2)) =
    let val (null1,first1,last1,n1,ass1,fol1) = dft n ass fol r1
	val (null2,first2,last2,n2,ass2,fol2) = dft n1 ass1 fol1 r2
    in  
	(null1 andalso null2, 
	 if null1 then Set.union(first1,first2) else first1,
	 if null2 then Set.union(last1,last2)   else last2, 
	 n2, ass2,
	 Set.fold (fn(i,m) => add m i first2) fol2 last1)
    end
  | dft n ass fol (Iter r) = 
    let val (_,first,last,n', ass', fol') = dft n ass fol r
    in  
	(true, first, last, n', ass',
	 Set.fold (fn(i,m) => add m i first) fol' last) 
    end

(* This is the the type of the particular, kind of NFA mentioned 
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


(* How to construct a NFA from a follow set. *)
fun nfa n ass fol start =
    {start = start, 
     edges = Vector.tabulate (n,  fn i => Map.lookup ass i),
     trans = Vector.tabulate (n-1,fn i => Map.lookup fol i),
     final = n-1}

local val sub = Vector.sub infix 9 sub
      fun app (Simple f) a = f a
	| app _ _          = false
in
    fun nextstate {start,edges,trans,final} states a =
	let fun one(x,res) = 
		if app (edges sub x) a then Set.union(trans sub x,res) 
		else res
	in  Set.fold one Set.empty states end
end

fun nfarun (nfa as {start,final,...}) string =
    let val next = nextstate nfa
        val endstates = CharVector.foldl (fn(a,res) => next res a) start string
    in  Set.mem(endstates, final) end 

fun translate reg = 
    let	val (_,first,last,n,ass,fol) = dft 0 (Map.empty()) (Map.empty()) reg
    in  nfa n ass fol first end

fun match re = nfarun (translate (Concat(re, Atom Unique)))
		 
local
(* simple top-down parser for regexp's
   the reserved characters are: |, *, (, ), $, ., ?, +
   to mach a reserved character put a \ just before it.
*)

    fun parse_err s = raise Fail ("Parse error: " ^ s)

    datatype token =
	ALT | STAR | LPAR | RPAR | DOLLAR 
      | CONST of char | DOT 
      | QMARK | PLUS | LBRA | RBRA
	
    structure C = Char
    structure SS = Substring

    val reserved = C.contains "()|$*.?+[]"

    fun get ss =
        SOME(case Option.valOf(SS.getc ss) of
                 (#"\\", ss) => (case SS.getc ss of
                                     SOME(c, ss') => 
                                        if reserved c then (CONST c, ss')
                                        else (CONST#"\\", ss)
                                   | _            => (CONST#"\\", ss))
               | (#"(", ss) => (LPAR, ss) 
               | (#")", ss) => (RPAR, ss)
               | (#"|", ss) => (ALT, ss)
               | (#"$", ss) => (DOLLAR, ss)
               | (#"*", ss) => (STAR, ss)
               | (#".", ss) => (DOT, ss)
               | (#"?", ss) => (QMARK, ss)
               | (#"+", ss) => (PLUS, ss)
               | (#"[", ss) => (LBRA, ss)
               | (#"]", ss) => (RBRA, ss)
               | (c, ss)    => (CONST c, ss))
        handle Option => NONE

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
	    in  (re, case get ts of
			 SOME(RPAR, ts) => ts
	               | _              => 
			 parse_err "unbalanced parentheses") 
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

	fun invalid_char c =
	  case c of
	    #"-" => true
	  | #"^" => true
	  | #"[" => true
	  | #"]" => true
	  | _ => false

	fun read ts f =
	  case SS.getc ts of 
	    SOME (#"]" , ts) => (f,ts)
	  | SOME (c1   , ts) => 
	      if invalid_char c1 then 
		parse_err ("invalid character '" ^ str c1 ^ "' in class")
	      else if c1 = #"\\" then
		(case SS.getc ts of
		   SOME (c2, ts) => 
		     if invalid_char c2 orelse c2 = #"\\" then
		       read ts (fn c => f c orelse c = c2)
		     else parse_err "invalid character escape"
		     | _ => parse_err "expecting character")
	      else			   
	      (case read_rng ts c1 of
		 SOME (g,ts) => 
		   read ts (fn c => f c orelse g c)
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
