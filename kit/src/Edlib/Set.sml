(*$Set: SET List *)

structure Set: SET =

(* POLYMORPHIC SETS.

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           23 Jan 1991

Maintenance:    Author


DESCRIPTION

   A straightforward implementation in terms of lists.


NOTES

   Many functions in the SET signature require an equality function as
   parameter.  Often MONO_SET or EQ_SET will be preferable.


SEE ALSO

   EqSet, MonoSet.

*)

struct


(* ABSTYPE *)

  abstype 'a Set = Set of 'a list
  with

(* TYPES *)

(*    type 'a T = 'a Set *)

(* LOCAL *)

    fun memberEq _  _ [] = false
    |   memberEq eq x (h::t) =
  	  eq x h orelse memberEq eq x t
  
    fun dropRepeats _ []  = []
    |   dropRepeats _ [x] = [x]
    |   dropRepeats eq (x::xs) =
          if memberEq eq x xs then dropRepeats eq xs
          else x :: (dropRepeats eq xs)


(* CONSTANTS *)

    val empty = Set []
    

(* CREATORS *)

    fun singleton elem = Set [elem]

    
(* CONVERTERS *)

    fun list (Set l) = l
    
    fun fromList elemEq l = Set (dropRepeats elemEq l)
    

(* OBSERVERS *)

    fun isEmpty (Set []) = true
    |   isEmpty _ = false
    
    fun member eq elem (Set []) = false
    |   member eq elem (Set (h::t)) =
    	  eq elem h orelse member eq elem (Set t)
    
    fun size (Set l) = length l
    
    local
         fun allContained elemEq [] _ = true
         |   allContained elemEq (h::t) s =
               member elemEq h s andalso allContained elemEq t s
    in
      fun eq elemEq (s1 as Set l) s2 =
	    size s1 = size s2 andalso
	    allContained elemEq l s2
    end
    

(* SELECTORS *)

    exception Empty of string
    
    fun select (Set []) = raise Empty "select"
    |   select (Set (h::t)) = (h, Set t)


(* MANIPULATORS *)

    fun insert elemEq elem (s as Set l) =
  	  if member elemEq elem s then s
  	  else Set (elem :: l)
    
    fun intersect eq s (Set []) = empty
    |   intersect eq s (Set (h::t)) =
  	  if member eq h s
  	  then insert eq h (intersect eq s (Set t))
  	  else intersect eq s (Set t)
  
    local
      fun dummyEq x y = false 	(* We know that there aren't any duplicates *)
      fun partition' (f, Set [], yes, no) = (yes, no)
      |   partition' (f, Set (h::t), yes, no) =
            if f h
    	    then partition' (f, Set t, insert dummyEq h yes, no)
            else partition' (f, Set t, yes, insert dummyEq h no)
    in
      fun partition f s = partition' (f, s, empty, empty)
    end
    
    fun remove eq elem set =
          #1 (partition (fn a => not (eq elem a)) set)
    
    fun difference eq s (Set []) = s
    |   difference eq s (Set (h::t)) =
           let val s' = remove eq h s
           in difference eq s' (Set t)
    	 end
    
    fun union elemEq (Set l1) (Set l2) = Set (dropRepeats elemEq (l1 @ l2))
    
    local
      fun closure' (eq, [], f, result) = result
      |   closure' (eq, h::t, f, result) =
            let val more = f h
                val (new as Set l) = difference eq more result
            in closure' (eq, t @ l, f, union eq result new)
            end
    in
      fun closure eq f (s as Set l) = closure' (eq, l, f, s)
    end

(* ITERATORS *)

    fun map f (Set l) = Set (List.map f l)

    fun apply f (Set l) = List.apply f l


(* REDUCERS *)

    fun fold f base (Set l) = List.foldL f base l

    fun fold' f (Set []) = raise Empty "fold'"
    |   fold' f (Set l)  = List.foldL' f l

  end (* abstype *)
end;

