(*$EqSet: EQ_SET List *)

structure EqSet: EQ_SET =

(* SETS OVER EQUALITY TYPES

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           23 Jan 1991

Maintenance:    Author


DESCRIPTION

   A straightforward implementation in terms of lists.


SEE ALSO

   MonoSet, Set.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:06  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.6  1991/10/22  18:33:26  db
Added map, apply, fold and fold' functions.

Revision 1.5  91/02/12  17:18:27  17:18:27  db (Dave Berry)
This is really embarrassing!  I had implemented set equality as
list equality, but although the lists have no repeated elements
they can be in arbitrary order.  I've fixed this.

Revision 1.4  91/02/12  12:55:07  12:55:07  db (Dave Berry)
Changed datatype to abstype.  Also improved the presentation.

Revision 1.3  91/01/25  15:44:22  db
Changed CoreUtils.eqMember to CoreUtils.member, reflecting the corresponding
change in the CoreUtils structure.

Revision 1.2  91/01/24  17:21:08  17:21:08  db (Dave Berry)
Removed version value.

Revision 1.1  91/01/24  15:39:44  15:39:44  db (Dave Berry)
Initial revision


*)

struct

(* ABSTYPE *)

  abstype 'a Set = Set of 'a list
  with
  

(* TYPE *)

(*    type 'a T = 'a Set *)


(* LOCAL *)

    fun dropRepeats []  = []
    |   dropRepeats [x] = [x]
    |   dropRepeats (x::xs) =
          if CoreUtils.member x xs then dropRepeats xs
          else x :: (dropRepeats xs)


(* CONSTANTS *)

    val empty = Set []


(* CREATORS *)

    fun singleton elem = Set [elem]
    

(* CONVERTORS *)

    fun list (Set l) = l
    
    fun fromList l = Set (dropRepeats l)
    

(* OBSERVERS *)

    fun isEmpty (Set []) = true
    |   isEmpty _ = false
    
    fun member elem (Set []) = false
    |   member elem (Set (h::t)) =
    	elem = h orelse member elem (Set t)
    
    fun size (Set l) = CoreUtils.length l
    
    local
      fun allContained [] _ = true
      |   allContained (h::t) s =
            member h s andalso allContained t s
    in
      fun eq (s1 as Set l) s2 =
            size s1 = size s2 andalso
            allContained l s2
    end


(* SELECTORS *)

    exception Empty of string
    
    fun select (Set []) = raise Empty "select"
    |   select (Set (h::t)) = (h, Set t)
    

(* MANIPULATORS *)
    
    fun insert elem (s as Set l) =
  	if member elem s then s
  	else Set (elem :: l)
    
    fun intersect s (Set []) = empty
    |   intersect s (Set (h::t)) =
          if member h s
          then insert h (intersect s (Set t))
          else intersect s (Set t)
  
    local
      fun partition' (f, Set [], yes, no) = (yes, no)
      |   partition' (f, Set (h::t), yes, no) =
            if f h
    	  then partition' (f, Set t, insert h yes, no)
            else partition' (f, Set t, yes, insert h no)
    in
      fun partition f s = partition' (f, s, empty, empty)
    end
    
    fun remove elem set =
          #1 (partition (fn a => not (elem = a)) set)
    
    fun difference s (Set []) = s
    |   difference s (Set (h::t)) =
           let val s' = remove h s
           in difference s' (Set t)
    	 end
    
    fun union (Set l1) (Set l2) = Set (dropRepeats (l1 @ l2))
    
    local
      fun closure' ([], f, result) = result
      |   closure' (h::t, f, result) =
            let val more = f h
                val (new as Set l) = difference more result
            in closure' (t @ l, f, union result new)
            end
    in
      fun closure f (s as Set l) = closure' (l, f, s)
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

