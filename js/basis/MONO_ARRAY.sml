(**
The MONO_ARRAY signature is a generic interface to monomorphic arrays,
mutable sequences with constant-time access and update. Monomorphic
arrays allow more compact representations than the analogous
polymorphic arrays over the same element type.

Arrays have a special equality property: two arrays are equal if they
are the same array, i.e., created by the same call to a primitive
array constructor such as array, fromList, etc.; otherwise they are
not equal. This also holds for arrays of zero length.
*)
signature MONO_ARRAY = 
  sig
    eqtype array
    type elem
    type vector
    val maxLen   : int
    val array    : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array
    val length   : array -> int
    val sub      : array * int -> elem
    val update   : array * int * elem -> unit
    val vector   : array -> vector
    val copy     : {src : array, dst : array, di : int} -> unit
    val copyVec  : {src : vector, dst : array, di : int} -> unit	
    val appi     : (int * elem -> unit) -> array -> unit
    val app      : (elem -> unit) -> array -> unit
    val modifyi  : (int * elem -> elem) -> array -> unit
    val modify   : (elem -> elem) -> array -> unit
    val foldli   : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldri   : (int * elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldl    : (elem * 'b -> 'b) -> 'b -> array -> 'b
    val foldr    : (elem * 'b -> 'b) -> 'b -> array -> 'b
    val findi    : (int * elem -> bool) -> array -> (int * elem) option
    val find     : (elem -> bool) -> array -> elem option
    val exists   : (elem -> bool) -> array -> bool
    val all      : (elem -> bool) -> array -> bool
    val collate  : (elem * elem -> order) -> array * array -> order
  end

(**

[type vector] The corresponding monomorphic vector type. We denote the
length of a vector vec of type vector by |vec|.

[maxLen] The maximum length of arrays supported by this
implementation. Attempts to create larger arrays will result in the
Size exception being raised.

[array (n, init)] creates a new array of length n; each element is
initialized to the value init. If n < 0 or maxLen < n, then the Size
exception is raised.

[fromList l] creates a new array from l, whose length is length l and
with the i(th) element of l used as the i(th) element of the array. If
the length of the list is greater than maxLen, then the Size exception
is raised.

[tabulate (n, f)] creates an array of n elements, where the elements
are defined in order of increasing index by applying f to the
element's index. This is equivalent to the expression (fromList
(List.tabulate (n, f))). If n < 0 or maxLen < n, then the Size
exception is raised.

[length arr] returns |arr|, the number of elements in the array arr.

[sub (arr, i)] returns the i(th) element of the array arr. If i < 0 or
|arr| <= i, then the Subscript exception is raised.

[update (arr, i, x)] sets the i(th) element of the array arr to x. If
i < 0 or |arr| <= i, then the Subscript exception is raised.

[vector arr] generates a vector from arr. Specifically, if vec is the
resulting vector, we have |vec| = |arr| and, for 0 <= i < |arr|,
element i of vec is sub (arr, i).

[copy {src, dst, di}]

[copyVec {src, dst, di}] These functions copy the entire array or
vector src into the array dst, with the i(th) element in src, for 0 <=
i < |src|, being copied to position di + i in the destination
array. If di < 0 or if |dst| < di+|src|, then the Subscript exception
is raised. In copy, if dst and src are equal, we must have di = 0 to
avoid an exception, and copy is then the identity.

[appi f arr]

[app f arr] These apply the function f to the elements of an array in
left to right order (i.e., increasing indices). The more general appi
function supplies both the element and the element's index to the
function f. The expression app f arr is equivalent to (appi (f o #2)
arr).
      
[modifyi f arr]

[modify f arr] These apply the function f to the elements of an array
in left to right order (i.e., increasing indices), and replace each
element with the result of applying f. The more general modifyi
function supplies both the element and the element's index to the
function f. The expression modify f arr is equivalent to (modifyi (f o
#2) arr).
      
[foldli f init arr]

[foldri f init arr]

[foldl f init arr]

[foldr f init arr] These fold the function f over all the elements of
an array, using the value init as the initial value. The functions
foldli and foldl apply the function f from left to right (increasing
indices), while the functions foldri and foldr work from right to left
(decreasing indices). The more general functions foldli and foldri
supply f with the array index of the corresponding element. The
indexed versions could be implemented as:

    fun foldli f init seq = let
      val len = length seq
      fun loop (i, b) =
            if i = len then b
            else loop(i+1,f(i,sub(seq,i),b))
      in
        loop(0,init)
      end

    fun foldri f init seq = let
      val len = length seq
      fun loop (i, b) =
            if i = ~1 then b
            else loop(i-1,f(i,sub(seq,i),b))
      in
        loop(len-1,init)
      end

The expression foldl f init arr is equivalent to

      foldli (fn (_, a, x) => f(a, x)) init arr

The analogous equivalences hold for foldri and foldr.

[findi f arr]

[find f arr] These apply f to each element of the array arr, from left
to right (i.e., increasing indices), until a true value is
returned. If this occurs, the functions return the element; otherwise,
they return NONE. The more general version findi also supplies f with
the array index of the element and, upon finding an entry satisfying
the predicate, returns that index with the element.

[exists f arr] applies f to each element x of the array arr, from left
to right (i.e., increasing indices), until f x evaluates to true; it
returns true if such an x exists and false otherwise.

[all f arr] applies f to each element x of the array arr, from left to
right (i.e., increasing indices), until f x evaluates to false; it
returns false if such an x exists and true otherwise. It is equivalent
to not(exists (not o f) arr)).

[collate f (a1, a2)] performs lexicographic comparison of the two
arrays using the given ordering f on elements.

*)
