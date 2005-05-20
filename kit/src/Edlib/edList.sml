structure List: LIST =

(* BASIC LIST FUNCTIONS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author 


DESCRIPTION

   A straightforward implementation; not always the most efficient.
*)

struct

  open EdlibGeneral

(* PERVASIVES *)

  (* The type declaration has been moved to the end of the file to get
     around a bug in Poplog 13.8. *)

  val map = map
  val rev = rev

  infixr 5 @
  val op @ = op @


(* TYPES *)

  type 'a T = 'a list


(* CONSTANTS *)

  val empty = []


(* CREATORS *)

  exception Size of string * int

  fun create 0 _ = []
  |   create n v =
	if n < 0 then raise Size ("create", n)
	else v :: create (n - 1) v

  local
    fun gen 0 _ _ = []
    |   gen n f i = f i :: gen (n - 1) f (i + 1)
  in
    fun tabulate (n, f) =
	  if n < 0 then raise Size ("tabulate", n)
	  else gen n f 0
  end

  local
    fun gen' 0 _ _ = []
    |   gen' n f z =
	  let val (x, y) = f z
	   in x :: gen' (n - 1) f y
	  end
  in
    fun tabulate' (0, _, _) = []
    |   tabulate' (n, f, base) =
	  if n < 0 then raise Size ("tabulate'", n)
	  else gen' n f base
  end


(* CONVERTORS *)

  local
    fun str _ nil _ _ = ""
    |   str p (h::t) sep needSep =
          let val s = p h ^ (str p t sep true)
           in if needSep then sep ^ s else s
          end
  in
    fun stringSep start finish sep p l = start ^ (str p l sep false) ^ finish
    fun string p l = stringSep ("[") ("]") (", ") p l
  end

  local
    fun pr os _ nil _ _ = ()
    |   pr os p (h::t) sep needSep =
          if needSep then
	    (TextIO.output (os, sep); p os h; pr os p t sep true)
	  else
	    (p os h; pr os p t sep true)
  in
    fun printSep os start finish sep p l =
	  (TextIO.output (os, start); pr os p l sep false; TextIO.output (os, finish))
    fun print os p l = printSep os ("[") ("]") (", ") p l
  end


(* OBSERVERS *)


  fun size [] = 0
  |   size (_::t) = 1 + size t

  fun eq _ [] [] = true
  |   eq _ [] _  = false
  |   eq _ _  [] = false
  |   eq p (h::t) (h'::t') = p h h' andalso eq p t t'

  fun ne _ [] [] = false
  |   ne _ [] _  = true
  |   ne _ _  [] = true
  |   ne p (h::t) (h'::t') = p h h' orelse ne p t t'

  fun eq' l1 l2 = (l1 = l2)

  fun ne' l1 l2 = (l1 <> l2)

  fun isEmpty [] = true
  |   isEmpty _  = false

  fun exists p []      = false
  |   exists p (x::xs) = (p x) orelse (exists p xs)

  fun forAll p []      = true
  |   forAll p (x::xs) = (p x) andalso (forAll p xs)

  fun member _ [] = false
  |   member x (h::t) =
	x = h orelse member x t

  local
    fun count _ [] _ = Fail ()
    |   count p (x::xs) n =
          if p x then OK n
          else count p xs (n + 1)
  in
    fun index p l = count p l 0
  end

  fun prefixes [] _ = true
  |   prefixes (h::t) (h'::t') =
        if h = h' then prefixes t t' else false
  |   prefixes _ [] = false;


(* MANIPULATING THE LAST ELEMENT *)

  exception Empty of string

  fun last []  = raise Empty "last"
  |   last [x] = x
  |   last (x::xs) = last xs

  fun dropLast []  = raise Empty "dropLast"
  |   dropLast [x] = []
  |   dropLast (x::xs) = x :: dropLast xs

  fun removeLast l = (last l, dropLast l)
      handle Empty _ => raise Empty "removeLast"

  fun changeLast _ [] = raise Empty "changeLast"
  |   changeLast f [x] = [f x]
  |   changeLast f (h::t) = h :: changeLast f t

  fun updateLast _ [] = raise Empty "updateLast"
  |   updateLast v [x] = [v]
  |   updateLast v (h::t) = h :: updateLast v t

  fun insertLast _ [] = raise Empty "insertLast"
  |   insertLast a [x] = a @ [x]
  |   insertLast a (h::t) = h :: insertLast a t

  fun appendLast l l' = l @ l'

  fun spliceLast _ [] = raise Empty "spliceLast"
  |   spliceLast a [x] = a
  |   spliceLast a (h::t) = h :: spliceLast a t


(* MANIPULATING THE NTH ELEMENT *)

  infix 9 sub
  exception Subscript of string * int
  exception Sub'
  local
    fun count (x::_,  0) = x
    |   count (_::xs, n) = count (xs, n - 1)
    |   count ([], n) = raise Sub'
  in
    fun l sub n =
          if n < 0 then raise Subscript ("sub", n)
          else count (l, n)
	  handle Sub' => raise Subscript ("sub", n)
  end

  fun nth n l = l sub n
		handle Subscript _ => raise Subscript ("nth", n)

  local
    fun remove (h::t, 0) = (h, t)
    |   remove (h::t, m) =
	  let val (x, l) = remove (t, m - 1)
	   in (x, h :: l)
	  end
    |   remove ([], _) = raise Sub'
  in
    fun removeNth n l =
	  if n < 0 then raise Subscript ("removeNth", n)
          else remove (l, n)
	  handle Sub' => raise Subscript ("removeNth", n)
  end

  local
    fun split (l, 0) = ([], l)
    |   split (x::xs, m) =
	  let val (l, l') = split (xs, m - 1)
	   in (x :: l, l')
	  end
    |   split ([], _) = raise Sub'
  in
    fun splitNth n l =
	  if n < 0 then raise Subscript ("splitNth", n)
          else split (l, n)
	  handle Sub' => raise Subscript ("splitNth", n)
  end

  local
    fun drop (_::xs, 0) = xs
    |   drop (x::xs, m) = x :: drop (xs, m - 1)
    |   drop ([], _) = raise Sub'
  in
    fun dropNth n l =
	  if n < 0 then raise Subscript ("dropNth", n)
          else drop (l, n)
	  handle Sub' => raise Subscript ("dropNth", n)
  end

  local
    exception Sub'
    fun changeNth' _ _ [] = raise Sub'
    |   changeNth' 0 f (h::t) = f h :: t
    |   changeNth' n f (h::t) = h :: changeNth' (n-1) f t
  in
    fun changeNth n f l =
	  changeNth' n f l
	  handle Sub' => raise Subscript ("changeNth", n)
  end

  local
    exception Sub'
    fun updateNth' _ _ [] = raise Sub'
    |   updateNth' 0 v (h::t) = v :: t
    |   updateNth' n v (h::t) = h :: updateNth' (n-1) v t
  in
    fun updateNth n f l =
	  updateNth' n f l
	  handle Sub' => raise Subscript ("updateNth", n)
  end

  local
    exception Sub'
    fun insertNth' _ _ [] = raise Sub'
    |   insertNth' 0 a l = a @ l
    |   insertNth' n a (x::l) = x :: insertNth' (n - 1) a l
  in
    fun insertNth n f l =
	  insertNth' n f l
	  handle Sub' => raise Subscript ("insertNth", n)
  end

  local
    exception Sub'
    fun appendNth' _ _ [] = raise Sub'
    |   appendNth' 0 a (h::t) = h :: a @ t
    |   appendNth' n a (x::l) = x :: appendNth' (n - 1) a l
  in
    fun appendNth n f l =
	  appendNth' n f l
	  handle Sub' => raise Subscript ("appendNth", n)
  end

  local
    exception Sub'
    fun spliceNth' _ _ [] = raise Sub'
    |   spliceNth' 0 a (h::t) = a @ t
    |   spliceNth' n a (x::l) = x :: spliceNth' (n - 1) a l
  in
    fun spliceNth n f l =
	  spliceNth' n f l
	  handle Sub' => raise Subscript ("spliceNth", n)
  end


(* ACCESSING A RANGE OF ELEMENTS *)

  exception ExtractLast of int
  local
    exception ExtractLast'
    fun extractLast' _ [] = raise ExtractLast'
    |   extractLast' 0 l = l
    |   extractLast' start (x::l) = extractLast' (start - 1) l
  in
    fun extractLast start [] = raise ExtractLast start
    |   extractLast start l =
	  if start < 0 then raise ExtractLast start
	  else extractLast' start l
	  handle ExtractLast' => raise ExtractLast start
  end

  exception Extract of int * int
  local
     (* extract' l start finish; as extract, except that start <= finish *)
    exception Extract'
    fun extract' 0 0 _ = []
    |   extract' _ _ [] = raise Extract'
    |   extract' 0 finish (x::l) = x :: extract' 0 (finish - 1) l
    |   extract' start finish (x::l) = extract' (start - 1) (finish - 1) l
  in
    fun extract start finish l =
          if start > finish orelse start < 0 then raise Extract (start, finish)
	  else extract' start finish l
	  handle Extract' => raise Extract (start, finish)
  end


(* MANIPULATING THE FIRST ELEMENT THAT SATISFIES A PREDICATE *)

  exception First of string

  fun first p [] = raise First "first"
  |   first p (x::xs) = if p x then x else first p xs

  fun dropFirst p [] = []
  |   dropFirst p (x::xs) = if p x then xs else x :: dropFirst p xs

  fun removeFirst p [] = raise First "removeFirst"
  |   removeFirst p (h::t) =
	if p h then (h, t)
	else
	  let val (x, l) = removeFirst p t
	   in (x, h::l)
	  end

  fun splitFirst p [] = raise First "splitFirst"
  |   splitFirst p (l as x::xs) =
	if p x then ([], l)
	else
	  let val (l1, l2) = splitFirst p xs
	   in (x::l1, l2)
	  end

  fun changeFirst _ _ [] = raise First "changeFirst"
  |   changeFirst p f (h::t) =
	if p h then (f h) :: t else h :: (changeFirst p f t)

  fun updateFirst _ _ [] = raise First "updateFirst"
  |   updateFirst p v (h::t) =
	if p h then v :: t else h :: updateFirst p v t

  fun insertFirst _ _ [] = raise First "insertFirst"
  |   insertFirst p a (l as h::t) =
         if p h then a @ l else h :: insertFirst p a t

  fun appendFirst _ _ [] = raise First "appendFirst"
  |   appendFirst p a (h::t) =
         if p h then h :: a @ t
	 else h :: appendFirst p a t

  fun spliceFirst _ _ [] = raise First "spliceFirst"
  |   spliceFirst p a (h::t) =
         if p h then a @ t
	 else h :: spliceFirst p a t


(* TAKING A PREFIX OF ELEMENTS THAT SATISFY A PREDICATE *)

  fun prefix p [] = []
  |   prefix p (x::xs) = if p x then x :: prefix p xs else []

  fun dropPrefix p [] = []
  |   dropPrefix p (l as x::xs) = if p x then dropPrefix p xs else l

  fun removePrefix p [] = ([], [])
  |   removePrefix p (l as x::xs) =
	if p x then
	  let val (l1, l2) = removePrefix p xs
	   in (x::l1, l2)
	  end
	else ([], l)


(* MANIPULATING ALL ELEMENTS THAT SATISFY A PREDICATE *)

  fun all p [] = []
  |   all p (x::xs) =
	if p x then x :: all p xs
        else all p xs

  fun dropAll p = all (not o p);

  fun removeAll p [] = ([], [])
  |   removeAll p (x::xs) =
         let val (yes, no) = removeAll p xs in
            if p x then (x :: yes, no)
            else (yes, x :: no)
         end

  fun updateAll _ _ [] = []
  |   updateAll p v (h::t) =
	 let val l = updateAll p v t
	 in if p h then v :: l else h :: l
	 end

  fun changeAll _ _ [] = []
  |   changeAll p f (h::t) =
	 let val l = changeAll p f t
	 in if p h then (f h) :: l else h :: l
	 end

  fun insertAll _ _ [] = []
  |   insertAll p a (h::t) =
	 let val l = h :: insertAll p a t
	 in if p h then a @ l else l
	 end

  fun appendAll _ _ [] = []
  |   appendAll p a (h::t) =
	 let val l = appendAll p a t
	 in if p h then h :: a @ l else h :: l
	 end

  fun spliceAll _ _ [] = []
  |   spliceAll p a (h::t) =
	 let val l = spliceAll p a t
	 in if p h then a @ l else h :: l
	 end


(* REDUCERS *)

  fun foldR f base []     = base
  |   foldR f base (x::xs) = f x (foldR f base xs)

  fun foldL f base []      = base
  |   foldL f base (x::xs) = foldL f (f x base) xs

  fun foldR' f [] = raise Empty "foldR'"
  |   foldR' f l  =
         let val (last, front) = removeLast l
	  in foldR f last front
         end

  fun foldL' f [] = raise Empty "foldL'"
  |   foldL' f (h::t) = foldL f h t

  fun pairwise f (h :: (t as h' :: _)) =
	f h h' andalso pairwise f t
  |   pairwise f _ = true


(* OTHER MANIPULATORS *)

  fun appendIfAll p a [] = a
  |   appendIfAll p a (l as h::t) =
         if p h then h :: appendIfAll p a t else l


(* ITERATORS *)

(* map is pervasive *)

  fun mapAll p f [] = []
  |   mapAll p f (x::xs) =
	if p x then f x :: mapAll p f xs
        else mapAll p f xs

  local
    fun iterate' _ [] _  = []
    |   iterate' f (x::xs) n = (f (x, n)) :: (iterate' f xs (n+1))
  in
    fun iterate f l = iterate' f l 0
  end

  fun apply f [] = ()
  |   apply f (x::xs) = (f x; apply f xs)

  fun applyAll p f [] = ()
  |   applyAll p f (x::xs) =
	if p x then (f x; applyAll p f xs)
        else applyAll p f xs

  local
    fun iterate' _ [] _  = ()
    |   iterate' f (x::xs) n = (f (x, n); iterate' f xs (n+1))
  in
    fun iterateApply f l = iterate' f l 0
  end

(* PERVASIVES *)

  type 'a list = 'a list

end
 
