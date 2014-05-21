signature LIST =
  sig
    datatype list = datatype list
    exception Empty

    val null : 'a list -> bool
    val length : 'a list -> int
    val @ : 'a list * 'a list -> 'a list
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val last : 'a list -> 'a
    val getItem : 'a list -> ('a * 'a list) option
    val nth : 'a list * int -> 'a
    val take : 'a list * int -> 'a list
    val drop : 'a list * int -> 'a list
    val rev : 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val revAppend : 'a list * 'a list -> 'a list
    val app : ('a -> unit) -> 'a list -> unit
    val map : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
    val find : ('a -> bool) -> 'a list -> 'a option
    val filter : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool)
                      -> 'a list -> 'a list * 'a list
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val exists : ('a -> bool) -> 'a list -> bool
    val all : ('a -> bool) -> 'a list -> bool
    val tabulate : int * (int -> 'a) -> 'a list
    val collate : ('a * 'a -> order)
                    -> 'a list * 'a list -> order
  end

(*
Description

exception Empty

    This exception indicates that an empty list was given as an
    argument to a function requiring a non-empty list.

null l

    returns true if the list l is empty.

length l

    returns the number of elements in the list l.

l1 @ l2

    returns the list that is the concatenation of l1 and l2.

hd l

    returns the first element of l. It raises Empty if l is nil.

tl l

    returns all but the first element of l. It raises Empty if l is
    nil.

last l

    returns the last element of l. It raises Empty if l is nil.

getItem l

    returns NONE if the list is empty, and SOME(hd l,tl l)
    otherwise. This function is particularly useful for creating value
    readers from lists of characters. For example, Int.scan
    StringCvt.DEC getItem has the type

      (int,char list) StringCvt.reader

    and can be used to scan decimal integers from lists of characters.

nth (l, i)

    returns the i(th) element of the list l, counting from 0. It
    raises Subscript if i < 0 or i >= length l. We have nth(l,0) = hd
    l, ignoring exceptions.

take (l, i)

    returns the first i elements of the list l. It raises Subscript if
    i < 0 or i > length l. We have take(l, length l) = l.

drop (l, i)

    returns what is left after dropping the first i elements of the
    list l. It raises Subscript if i < 0 or i > length l. It holds
    that take(l, i) @ drop(l, i) = l when 0 <= i <= length l. We also
    have drop(l, length l) = [].

rev l

    returns a list consisting of l's elements in reverse order.

concat l

    returns the list that is the concatenation of all the lists in l
    in order.  concat[l1,l2,...ln] = l1 @ l2 @ ... @ ln

revAppend (l1, l2)

    returns (rev l1) @ l2.

app f l

    applies f to the elements of l, from left to right.

map f l

    applies f to each element of l from left to right, returning the
    list of results.

mapPartial f l

    applies f to each element of l from left to right, returning a
    list of results, with SOME stripped, where f was defined. f is not
    defined for an element of l if f applied to the element returns
    NONE.  The above expression is equivalent to:

      ((map valOf) o (filter isSome) o (map f)) l

find f l

    applies f to each element x of the list l, from left to right,
    until f x evaluates to true. It returns SOME(x) if such an x
    exists; otherwise it returns NONE.

filter f l

    applies f to each element x of l, from left to right, and returns
    the list of those x for which f x evaluated to true, in the same
    order as they occurred in the argument list.

partition f l

    applies f to each element x of l, from left to right, and returns
    a pair (pos, neg) where pos is the list of those x for which f x
    evaluated to true, and neg is the list of those for which f x
    evaluated to false. The elements of pos and neg retain the same
    relative order they possessed in l.

foldl f init [x1, x2, ..., xn]

    returns

      f(xn,...,f(x2, f(x1, init))...)

    or init if the list is empty.

foldr f init [x1, x2, ..., xn]

    returns

      f(x1, f(x2, ..., f(xn, init)...))
 
    or init if the list is empty.

exists f l

    applies f to each element x of the list l, from left to right,
    until f x evaluates to true; it returns true if such an x exists
    and false otherwise.

all f l

    applies f to each element x of the list l, from left to right,
    until f x evaluates to false; it returns false if such an x exists
    and true otherwise. It is equivalent to not(exists (not o f) l)).

tabulate (n, f)

    returns a list of length n equal to [f(0), f(1), ..., f(n-1)],
    created from left to right. It raises Size if n < 0.

collate f (l1, l2)

    performs lexicographic comparison of the two lists using the given
    ordering f on the list elements.

Discussion

    The list type is considered primitive and is defined in the
    top-level environment. It is rebound here for consistency.
*)
