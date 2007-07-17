signature MONO_VECTOR = 
  sig
    type vector
    type elem
    val maxLen   : int
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector
    val length   : vector -> int
    val sub      : vector * int -> elem
    val update   : vector * int * elem -> vector
    val concat   : vector list -> vector
    val appi     : (int * elem -> unit) -> vector -> unit
    val app      : (elem -> unit) -> vector -> unit
    val mapi     : (int * elem -> elem) -> vector -> vector
    val map      : (elem -> elem) -> vector -> vector
    val foldli   : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri   : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl    : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr    : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val findi    : (int * elem -> bool) -> vector -> (int * elem) option
    val find     : (elem -> bool) -> vector -> elem option
    val exists   : (elem -> bool) -> vector -> bool
    val all      : (elem -> bool) -> vector -> bool
    val collate  : (elem * elem -> order) -> vector * vector -> order
end

(*
Description

val maxLen : int

    The maximum length of vectors supported by this
    implementation. Attempts to create larger vectors will result in
    the Size exception being raised.

fromList l

    creates a new vector from l, whose length is length l and with the
    i(th) element of l used as the i(th) element of the vector. If the
    length of the list is greater than maxLen, then the Size exception
    is raised.

tabulate (n, f)

    creates a vector of n elements, where the elements are defined in
    order of increasing index by applying f to the element's
    index. This is equivalent to the expression:

fromList (List.tabulate (n, f))

    If n < 0 or maxLen < n, then the Size exception is raised.

length vec

    returns |vec|, the length (i.e., the number of elements) of the
    vector vec.

sub (vec, i)

    returns the i(th) element of the vector vec. If i < 0 or |vec| <=
    i, then the Subscript exception is raised.

update (vec, i, x)

    returns a new vector, identical to vec, except the i(th) element
    of vec is set to x. If i < 0 or |vec| <= i, then the Subscript
    exception is raised.

concat l

    returns the vector that is the concatenation of the vectors in the
    list l. If the total length of these vectors exceeds maxLen, then
    the Size exception is raised.

appi f vec
app f vec

    These apply the function f to the elements of a vector in left to
    right order (i.e., increasing indices). The more general appi
    function supplies both the element and the element's index to the
    function f. The expression app f vec is equivalent to:

      appi (f o #2) vec

mapi f vec
map f vec

    These functions produce new vectors by mapping the function f from
    left to right over the argument vector. The more general mapi
    function supplies both the element and the element's index to the
    function f. The expression mapi f vec is equivalent to:

      fromList (List.map f (foldri (fn (i,a,l) => (i,a)::l) [] vec))
      
    The expression map f vec is equivalent to:

      mapi (f o #2) vec
      
foldli f init vec
foldri f init vec
foldl f init vec
foldr f init vec

    These fold the function f over all the elements of a vector, using
    the value init as the initial value. The functions foldli and
    foldl apply the function f from left to right (increasing
    indices), while the functions foldri and foldr work from right to
    left (decreasing indices). The more general functions foldli and
    foldri supply both the element and the element's index to the
    function f.

    Refer to the MONO_ARRAY manual pages for reference implementations
    of the indexed versions.

    The expression foldl f is equivalent to:

      foldli (fn (_, a, x) => f(a, x))
      
    A similar relation holds between foldr and foldri.

findi f vec
find f vec

    These apply f to each element of the vector vec, from left to
    right (i.e., increasing indices), until a true value is
    returned. If this occurs, the functions return the element;
    otherwise, they return NONE. The more general version findi also
    supplies f with the vector index of the element and, upon finding
    an entry satisfying the predicate, returns that index with the
    element.

exists f vec

    applies f to each element x of the vector vec, from left to right
    (i.e., increasing indices), until f x evaluates to true; it
    returns true if such an x exists and false otherwise.

all f vec

    applies f to each element x of the vector vec, from left to right
    (i.e., increasing indices), until f x evaluates to false; it
    returns false if such an x exists and true otherwise. It is
    equivalent to not(exists (not o f ) vec)).

collate f (v1, v2)

    performs lexicographic comparison of the two vectors using the
    given ordering f on elements.
*)
