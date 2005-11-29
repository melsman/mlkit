signature LIST_PAIR = 
  sig
    exception UnequalLengths
    val zip   : 'a list * 'b list -> ('a * 'b) list
    val zipEq : 'a list * 'b list -> ('a * 'b) list
    val unzip : ('a * 'b) list -> 'a list * 'b list
    val app   : ('a * 'b -> unit) -> 'a list * 'b list -> unit
    val appEq : ('a * 'b -> unit) -> 'a list * 'b list -> unit
    val map   : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val foldl   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a list * 'b list -> 'c
    val foldr   : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a list * 'b list -> 'c
    val foldlEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a list * 'b list -> 'c
    val foldrEq : ('a * 'b * 'c -> 'c)
                    -> 'c -> 'a list * 'b list -> 'c
    val all    : ('a * 'b -> bool) -> 'a list * 'b list -> bool
    val exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool
    val allEq : ('a * 'b -> bool) -> 'a list * 'b list -> bool 

  end

(*
exception UnequalLengths
    This exception is raised by those functions that require arguments of
    identical length.

zip (l1, l2)
zipEq (l1, l2)
    These functions combine the two lists l1 and l2 into a list of pairs, with
    the first element of each list comprising the first element of the result,
    the second elements comprising the second element of the result, and so on.
    If the lists are of unequal lengths, zip ignores the excess elements from
    the tail of the longer one, while zipEq raises the exception
    UnequalLengths.

unzip l
    returns a pair of lists formed by splitting the elements of l. This is the
    inverse of zip for equal length lists.

app f (l1, l2)
appEq f (l1, l2)
    These apply the function f to the list of pairs of elements generated from
    left to right from the lists l1 and l2. If the lists are of unequal
    lengths, the former ignores the excess elements from the tail of the longer
    one, and the latter raises UnequalLengths. The above expressions are
    respectively equivalent to:

      List.app f (zip (l1, l2))
      List.app f (zipEq (l1, l2))
      

    ignoring possible side-effects of the function f.

map f (l1, l2)
mapEq f (l1, l2)
    These map the function f over the list of pairs of elements generated from
    left to right from the lists l1 and l2, returning the list of results. If
    the lists are of unequal lengths, the former ignores the excess elements
    from the tail of the longer one, and the latter raises UnequalLengths. The
    above expressions are respectively equivalent to:

      List.map f (zip (l1, l2))
      List.map f (zipEq (l1, l2))
      

    ignoring possible side-effects of the function f.

foldl f init (l1, l2)
foldr f init (l1, l2)
foldlEq f init (l1, l2)
foldrEq f init (l1, l2)
    These return the result of folding the function f in the specified
    direction over the pair of lists l1 and l2 starting with the value init.
    They are respectively equivalent to:

      List.foldl f' init (zip (l1, l2))
      List.foldr f' init (zip (l1, l2))
      List.foldl f' init (zipEq (l1, l2))
      List.foldr f' init (zipEq (l1, l2))
      
    where f' is fn ((a,b),c) => f(a,b,c) and ignoring possible side-effects of
    the function f.

all f (l1, l2)
exists f (l1, l2)
    These functions provide short-circuit testing of a predicate over a pair of
    lists. They are respectively equivalent to:

      List.all f (zip (l1, l2))
      List.exists f (zip (l1, l2))

allEq f (l1, l2)
    returns true if l1 and l2 have equal length and all pairs of elements
    satisfy the predicate f. That is, the expression is equivalent to:

        (List.length l1 = List.length l2) andalso
      (List.all f (zip (l1, l2)))
      

    This function does not appear to have any nice algebraic relation with the
    other functions, but it is included as providing a useful notion of
    equality, analogous to the notion of equality of lists over equality types.

        Implementation note:

        The implementation is simple:

        fun allEq p ([], []) = true
          | allEq p (x::xs, y::ys) = p(x,y) andalso allEq p (xs,ys)
          | allEq _ _ = false
      
Discussion
    Note that a function requiring equal length arguments should determine this
    lazily, i.e., it should act as though the lists have equal length and invoke
    the user-supplied function argument, but raise the exception if it arrives at
    the end of one list before the end of the other. 

*)
