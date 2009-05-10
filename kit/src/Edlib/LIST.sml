(*$LIST: GeneralTypes *)

signature LIST =
sig

(* BASIC FUNCTIONS ON LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:		3 Oct 1989

Maintenance:	Author


DESCRIPTION

   Functions on the built-in type 'a list.

   Many of these functions use an orthogonal naming scheme.  They can be
   grouped as follows:

   f;
     if f is "last", return the last element.
     if f is "nth", return the nth element (counting from zero).
     if f is "first", return the first element that satisfies a predicate p.
     if f is "all", return the list of elements that satisfy p.
     if f is "prefix", return the prefix of elements that satisfy p.

   dropF ... l; l with (f l) removed.
   splitF ... l; a pair of lists:
		 (elements before f l, elements from f l to last l (inc.)).
   removeF ... l; a pair of (f l, dropF l).
   updateF ... l; l with (f l) replaced with a value.
   changeF ... l; l with (f l) replaced with (g (f l)).
   spliceF ... l; l with (f l) replaced with the elements in another list.
   insertF ... l; l with the elements in another list inserted before (f l).
   appendF ... l; l with the elements in another list inserted after (f l).

   splitLast, splitAll, and splitPrefix are not provided because "all" and
   "prefix" return lists and because splitLast would be the same as removeLast.

   changePrefix, updatePrefix, insertPrefix, appendPrefix, and splicePrefix
   are not provided, because "prefix" returns a list.
   
   The "last" functions (except appendLast) raise "Empty" if l is empty.
   The "nth" functions raise "Subscript" if not (0 <= n < size l).
   The "first" functions (except dropFirst) raise "First" if there aren't
	any elements that satisfy p.


SEE ALSO

   LIST_PARSE


NOTES

   Possibly there should be a dropExtract (aka delete?) function.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:12  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.16  91/03/26  13:37:15  13:37:15  db (Dave Berry)
Changed appendLast; will now append a list to the empty list.

Revision 1.15  91/03/06  16:28:59  16:28:59  db (Dave Berry)
Added print function(s).

Revision 1.14  91/02/22  19:03:00  19:03:00  db (Dave Berry)
Added Size exception, which replaces occurrences of General.Nat.
Renamed Sub to Subscript, to match existing convention.
Renamed generate and generate' to tabulate and tabulate', and uncurried them,
  to match the standard agreed for arrays and vectors by the SML implementers.

Revision 1.13  91/02/22  15:35:42  15:35:42  db (Dave Berry)
Added prefixes function.

Revision 1.12  91/02/12  12:18:04  12:18:04  db (Dave Berry)
Changed type to eqtype.

Revision 1.11  91/02/11  18:41:15  18:41:15  db (Dave Berry)
Changed the name of this signature from LIST' to LIST.
Moved sort and perms functions to LIST_SORT.sml.
Removed comparison functions altogether.
All this forms part of the major reorganisation of the library.

Revision 1.10  91/01/31  17:48:33  17:48:33  db (Dave Berry)
Added type.

Revision 1.9  91/01/30  18:43:58  18:43:58  db (Dave Berry)
Renamed null function to isEmpty.

Revision 1.8  91/01/26  13:41:50  13:41:50  db (Dave Berry)
Changed signature names in SEE ALSO section to all upper case - I missed
this when doing the main change.

Revision 1.7  91/01/25  19:14:34  19:14:34  db (Dave Berry)
Added dependence on GeneralTypes and/or InStreamType.

Revision 1.6  91/01/25  16:55:15  16:55:15  db (Dave Berry)
Changed signature name to all upper case, added make tag.

Revision 1.5  91/01/25  16:05:00  16:05:00  db (Dave Berry)
Removed dropRepeats and dropRepeats' functions.  This functionality
is redundant now that sets have been added.

Revision 1.4  91/01/24  17:06:26  17:06:26  db (Dave Berry)
Removed version value.

Revision 1.3  91/01/15  11:30:24  11:30:24  db (Dave Berry)
Renamed "empty" function to "null"; added "empty" constant.

Revision 1.2  91/01/10  17:55:59  17:55:59  db (Dave Berry)
Changed ge and le to expect le and ge functions as parameters instead
of lt and gt (which people found confusing).

Revision 1.1  90/12/17  16:49:43  16:49:43  db (Dave Berry)
Initial revision


*)

(* PERVASIVES *)

  eqtype 'a list

  val map: ('a -> 'b) -> 'a list -> 'b list
  val rev: 'a list -> 'a list
  (* infixr 5 @ *)
  val @ : 'a list * 'a list -> 'a list


(* TYPES *)

  type 'a T = 'a list


(* CONSTANTS *)

  val empty: 'a list
   (* empty = []. *)


(* CREATORS *)

  exception Size of string * int
   (* Size (fn, i); raised by the creation functions when they are called
      with a negative size argument. *)

  val create: int -> 'a -> 'a list
   (* create n e; create a list of size n, each element being e. *)

  val tabulate: int * (int -> 'a) -> 'a list
  (* tabulate (n, f); create a list of size n, sych that the element with
     index i is initialised to (f i). *)

  val tabulate': int * ('b -> 'a * 'b) * 'b -> 'a list
  (* tabulate' (n, f, base); create a list l of size n, with (l sub 0)
     initialised to (# 1 (f base)) and (l sub i (i > 0)) initialised to
     (# 1 (f (# 2 f_i))), where f_i is the result of the i^th application
     of f. *)


(* CONVERTORS *)

  val stringSep: string -> string -> string ->
                 ('a -> string) -> 'a list -> string
   (* stringSep start finish sep p l; returns the string representation of l,
      beginning with start, ending with finish, and with the elements
      separated by sep. *)

  val string: ('a -> string) -> 'a list -> string
   (* string p l; returns the canonical string representation of l. *)

(*
  val printSep: TextIO.outstream -> string -> string -> string ->
                (TextIO.outstream -> 'a -> unit) -> 'a list -> unit
   (* printSep os start finish sep p l; sends the string representation of l
      to the stream os, beginning with start, ending with finish, and with
      the elements separated by sep. *)

  val print: TextIO.outstream -> (TextIO.outstream -> 'a -> unit) -> 'a list -> unit
   (* print os p l; sends the canonical string representation of l to
      the stream os. *)
*)

(* OBSERVERS *)

  exception Subscript of string * int

  val size: 'a list -> int
   (* size l; returns the number of elements in l. *)

  val isEmpty: 'a list -> bool
   (* isEmpty l; returns true if l is the empty list. *)

  val exists: ('a -> bool) -> 'a list -> bool
   (* exists p l; true if there exists an element of l satisfying p. *)

  val forAll: ('a -> bool) -> 'a list -> bool
   (* forAll p l; true if every element of l satisfies p. *)

  val member: ''a -> ''a list -> bool
   (* member a l; true if a is an element of l. *)

  val eq: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
   (* eq x y; returns true if (size x = size y) and for all i,
      0 <= i < size x, (p (x sub i) (y sub i)). *)

  val ne: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
   (* ne x y; returns true if (size x <> size y) or there exists
      an i such that 0 <= i < size x and (p (x sub i) (y sub i)). *)

  val eq': ''a list -> ''a list -> bool
   (* eq' x y; returns true if (x = y). *)

  val ne': ''a list -> ''a list -> bool
   (* ne' x y; returns true (x <> y). *)

  val index: ('a -> bool) -> 'a list -> (int, unit) EdlibGeneral.Result
   (* index p l; returns the position in l of the first element
      of l satisfying p. *)

  val prefixes: ''a list -> ''a list -> bool
   (* prefixes l1 l2; returns true if l1 is a prefix of l2.
      Raises (Subscript ("prefixes", n)) if not (0 <= n < size s2). *)


(* MANIPULATING THE LAST ELEMENT *)

  exception Empty of string

  val last: 'a list -> 'a
   (* last l; returns the last element of l.  Raises (Empty "last") if l
      is empty. *)

  val dropLast: 'a list -> 'a list
   (* dropLast l; returns l without its last element.
      Raises (Empty "dropLast") if l is empty. *)

  val removeLast: 'a list -> ('a * 'a list)
   (* removeLast l = (lastl, dropLast l).
      Raises (Empty "removeLast") if l is empty. *)

  val updateLast: 'a -> 'a list -> 'a list
   (* updateLast v l; returns l with its last element replaced by v.
      Raises (Empty "updateLast") if l is empty. *)

  val changeLast: ('a -> 'a) -> 'a list -> 'a list
   (* changeLast f l; returns l with (last l) replaced by (f (last l)).
      Raises (Empty "changeLast") if l is empty. *)

  val insertLast: 'a list -> 'a list -> 'a list
   (* insertLast l' l; returns l with the elements of l' inserted before
      (last l).  Raises (Empty "insertLast") if l is empty. *)

  val appendLast: 'a list -> 'a list -> 'a list
   (* appendLast l' l; returns l with the elements of l' appended after
      (last l). *)

  val spliceLast: 'a list -> 'a list -> 'a list
   (* spliceLast l' l; returns l with (last l) replaced by the elements of l'.
      Raises (Empty "spliceLast") if l is empty. *)


(* MANIPULATING THE NTH ELEMENT *)

  (* infix 9 sub *)
  val sub: 'a list * int -> 'a
   (* l sub n; returns the n-1'th element of l.
      Raises (Subscript ("sub", n)) if not (0 <= n < size l). *)

  val nth: int -> 'a list -> 'a
   (* nth n l; returns the n-1'th element of l.
      Raises (Subscript ("nth", n)) if not (0 <= n < size l). *)

  val removeNth: int -> 'a list -> ('a * 'a list)
   (* removeNth n l= (l sub n, dropNth n l).
      Raises (Subscript ("removeNth", n)) if not (0 <= n < size l). *)

  val splitNth: int -> 'a list -> ('a list * 'a list)
   (* splitNth n l; returns ([l sub 0, ... l sub (n-1)], [l sub n, ... last l]).
      Raises (Subscript ("splitNth", n)) if not (0 <= n <= size l - 1). *)

  val dropNth: int -> 'a list -> 'a list
   (* dropNth n l; returns l without (l sub n).
      Raises (Subscript ("dropNth", n)) if not (0 <= n < size l). *)

  val updateNth: int -> 'a -> 'a list -> 'a list
   (* updateNth n v l; returns l with (l sub n) replaced by v.
      Raises (Subscript ("updateNth", n)) if not (0 <= n < size l). *)

  val changeNth: int -> ('a -> 'a) -> 'a list -> 'a list
   (* changeNth n f l; returns l with (l sub n) replaced by (f (l sub n)).
      Raises (Subscript ("changeNth", n)) if not (0 <= n < size l). *)

  val insertNth: int -> 'a list -> 'a list -> 'a list
   (* insertNth n l' l; returns l with the elements of l' inserted before
      (l sub n).
      Raises (Subscript ("insertNth", n)) if not (0 <= n < size l). *)

  val appendNth: int -> 'a list -> 'a list -> 'a list
   (* appendNth n l' l; returns l with the elements of l' appended after
      (l sub n).
      Raises (Subscript ("insertNth", n)) if not (0 <= n < size l). *)

  val spliceNth: int -> 'a list -> 'a list -> 'a list
   (* spliceNth n l' l; returns l with (l sub n) replaced by the elements of
      l'.  Raises (Subscript ("spliceNth", n)) if not (0 <= n < size l). *)


(* ACCESSING A RANGE OF ELEMENTS *)

  exception ExtractLast of int
  val extractLast: int -> 'a list -> 'a list
   (* extractLast l start ; returns the elements of l from (l sub start) to
      last l.  Raises (ExtractLast start) if not (0 <= start < size l). *)

  exception Extract of int * int
  val extract: int -> int -> 'a list -> 'a list
   (* extract start finish l; returns the elements of l from (l sub start) to
      (l sub (finish - 1)).  Returns [] if (start = finish).  Raises
      (Extract (start, finish)) if not (0 <= start <= finish <= size l). *)


(* MANIPULATING THE FIRST ELEMENT THAT SATISFIES A PREDICATE *)

  exception First of string

  val first: ('a -> bool) -> 'a list -> 'a
   (* first p l; returns the first element in l satisfying p.
      Raises (First "first") if p doesn't hold for any element of l. *)

  val dropFirst: ('a -> bool) -> 'a list -> 'a list
   (* dropFirst p l; returns l without the first of its elements (if any)
      that satisfy p. *)

  val removeFirst: ('a -> bool) -> 'a list -> ('a * 'a list)
   (* removeFirst p l = (first l, dropFirst l).
      Raises (First "removeFirst") if p doesn't hold for any element of l. *)

  val splitFirst: ('a -> bool) -> 'a list -> ('a list * 'a list)
   (* splitFirst p l; returns (extract 0 n l, extractLast n l),
      where (l sub n) is the first element of l that satisfies p.
      Raises (First "splitFirst") if p doesn't hold for any element of l. *)

  val updateFirst: ('a -> bool) -> 'a -> 'a list -> 'a list
   (* updateFirst p v l; returns l with (first p l) replaced by v.
      Raises (First "updateFirst") if there is no (first p l). *)

  val changeFirst: ('a -> bool) -> ('a -> 'a) -> 'a list -> 'a list
   (* changeFirst p f l; returns l with (first p l) replaced by
      (f (first p l)).  Raises (First "changeFirst") if there is no
      (first p l). *)

  val insertFirst: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* insertFirst p l' l; returns l with the elements of l' inserted before
      (first p l).  Raises (First "insertFirst") if there is no (first p l). *)

  val appendFirst: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* appendFirst p l' l; returns l with the elements of l' appended after
      (first p l).  Raises (First "insertFirst") if there is no (first p l). *)

  val spliceFirst: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* spliceFirst p l' l; returns l with (first p l) replaced by the elements
      of l'.  Raises (First "spliceFirst") if there is no (first p l). *)


(* TAKING A PREFIX OF ELEMENTS THAT SATISFY A PREDICATE *)

  val prefix: ('a -> bool) -> 'a list -> 'a list
   (* prefix p l; returns the largest prefix of l each of whose
      elements satisfies p *)

  val dropPrefix: ('a -> bool) -> 'a list -> 'a list
   (* dropPrefix p l; returns l without the largest prefix of l
      each of whose elements satisfies p *)

  val removePrefix: ('a -> bool) -> 'a list -> ('a list * 'a list)
   (* removePrefix p l = (prefix p l, dropPrefix p l). *)


(* MANIPULATING ALL ELEMENTS THAT SATISFY A PREDICATE *)

  val all: ('a -> bool) -> 'a list -> 'a list
   (* all p l: returns a list of the elements of l that satisfy p. *)

  val dropAll: ('a -> bool) -> 'a list -> 'a list
   (* dropAll p l: returns a list of the elements of l that don't satisfy p. *)

  val removeAll: ('a -> bool) -> 'a list -> ('a list * 'a list)
   (* removeAll p l = (all p l, dropAll p l). *)

  val updateAll: ('a -> bool) -> 'a -> 'a list -> 'a list
   (* updateAll p v l; returns l with each element that satisfies p
      replaced by v. *)

  val changeAll: ('a -> bool) -> ('a -> 'a) -> 'a list -> 'a list
   (* changeAll p f l; returns l with each element e that satisfies p
      replaced by (f e). *)

  val insertAll: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* insertAll p l' l; returns l with the elements of l' inserted before each
      element of l that satisfies p. *)

  val appendAll: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* appendAll p l' l; returns l with the elements of l' appended after each
      element of l that satisfies p. *)

  val spliceAll: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* spliceAll p l' l; returns l with each element that satisfies p
      replaced by the elements of l'. *)


(* OTHER MANIPULATORS *)

  val appendIfAll: ('a -> bool) -> 'a list -> 'a list -> 'a list
   (* appendIfAll p l' l; appends l' at the end of l if every element of
      l satisfies p. *)


(* ITERATORS *)

(* map is pervasive
  val map: ('a -> 'b) -> 'a list -> 'b list
   (* map f l; builds a list by applying f to each element of l. *)
*)

  val mapAll: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
   (* mapAll p f l; builds a list by applying f to each element of l that
      satisfies p. *)

  val iterate: ('a * int -> 'b) -> 'a list -> 'b list
   (* iterate f l; builds a list by applying f to each element of l paired
      with its index. *)

  val apply: ('a -> unit) -> 'a list -> unit
   (* apply f l; applies f to each element of l. *)

  val applyAll: ('a -> bool) -> ('a -> unit) -> 'a list -> unit
   (* applyAll p f l; applies f to each element of l that satisfies p. *)

  val iterateApply: ('a * int -> unit) -> 'a list -> unit
   (* iterateApply f l; applies f to each element of l paired
      with its index. *)


(* REDUCERS *)

  val foldR: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
   (* foldR f base l; folds using f associating to the right over the
      base element.
      foldR f [a1,a2,...,an] base = f a1 (f a2 ... (op an base)...). *)

  val foldL: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
   (* foldL f l base; folds using f associating to the left over the
      base element.
      foldL f [a1,a2,...,an] base = f an ... (f a2 (f a1 base))... . *)

  val foldR': ('a -> 'a -> 'a) -> 'a list -> 'a
   (* foldR' f l; folds using f associating to the right over the
      last element of l.  Raises (Empty "foldR'") if l is empty. *)

  val foldL': ('a -> 'a -> 'a) -> 'a list -> 'a
   (* foldL' f l; folds using f associating to the left over the
      first element of l.  Raises (Empty "foldL'") if l is empty. *)

  val pairwise: ('a -> 'a -> bool) -> 'a list -> bool
   (* pairwise f l; true if (f (l sub i) (l sub (i + 1))) is true for all
      0 <= i < size l, or if l is empty. *)
end

