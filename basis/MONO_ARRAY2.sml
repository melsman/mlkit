(** Monomorphic 2-dimensional arrays.

The MONO_ARRAY2 signature is a generic interface to mutable
2-dimensional arrays. Two arrays are equal only if they are the same
array, i.e., created by the same call to a primitive array constructor
such as array, fromList, etc.; otherwise they are not equal. This also
holds for arrays of zero length.

The elements of 2-dimensional arrays are indexed by pair of integers
(i,j) where i gives the row index, and i gives the column index. As
usual, indices start at 0, with increasing indices going from left to
right and, in the case of rows, from top to bottom.

*)

signature MONO_ARRAY2 = sig
  eqtype array
  type elem
  type vector
  type region = {base  : array,
                 row   : int,
                 col   : int,
                 nrows : int option,
                 ncols : int option}

  datatype traversal = datatype Array2.traversal

  val array    : int * int * elem -> array
  val fromList : elem list list -> array
  val tabulate : traversal -> int * int * (int * int -> elem) -> array

  val sub      : array * int * int -> elem
  val update   : array * int * int * elem -> unit

  val dimensions : array -> int * int
  val nCols      : array -> int
  val nRows      : array -> int

  val row    : array * int -> vector
  val column : array * int -> vector

  val copy : {src : region,
              dst : array,
              dst_row : int,
              dst_col : int} -> unit

  val appi    : traversal -> (int * int * elem -> unit) -> region -> unit
  val app     : traversal -> (elem -> unit) -> array -> unit
  val foldi   : traversal -> (int * int * elem * 'b -> 'b) -> 'b -> region -> 'b
  val fold    : traversal -> (elem * 'b -> 'b) -> 'b -> array -> 'b
  val modifyi : traversal -> (int * int * elem -> elem) -> region -> unit
  val modify  : traversal -> (elem -> elem) -> array -> unit
end

(**

[type vector] The type of one-dimensional immutable vectors of the
underlying element type.

[type region] This type specifies a rectangular subregion of a
2-dimensional array. If ncols = SOME(w), the region includes only
those elements in columns with indices in the range from col to col + (w
- 1), inclusively. If ncols = NONE, the region includes only those
elements lying on or to the right of column col. A similar
interpretation holds for the row and nrows fields. Thus, the region
corresponds to all those elements with position (i,j) such that i lies
in the specified range of rows and j lies in the specified range of
columns.

A region reg is said to be valid if it denotes a legal subarray of its
base array. More specifically, reg is valid if

  0 <= #row reg <= nRows (#base reg)
  when #nrows reg = NONE, or
  0 <= #row reg <= (#row reg)+nr <= nRows (#base reg)
  when #nrows reg = SOME(nr), and the analogous conditions hold for columns.

[datatype traversal] This type specifies ways of traversing an array
or region. For more complete information, see the entry for
Array2.traversal.

[array (r, c, init)] creates a new array with r rows and c columns,
with each element initialized to the value init. If r < 0, c < 0 or
the resulting array size is too large, the Size exception is raised.

[fromList l] creates a new array from a list of a list of
elements. The elements should be presented in row major form, i.e., hd
l gives the first row, hd (tl l) gives the second row, etc. It raises
the Size exception if the the resulting array size is too large, or if
the lists in l do not all have the same length.

[tabulate tr (r, c, f)] creates a new array with r rows and c columns,
with the (i,j)(th) element initialized to f (i,j). The elements are
initialized in the traversal order specified by tr. If r < 0, c < 0 or
the resulting array size is too large, the Size exception is raised.

[sub (arr, i, j)] returns the (i,j)(th) element of the array arr. If i
< 0, j < 0, nRows arr <= i or nCols arr <= j, then the Subscript
exception is raised.

[update (arr, i, j, a)] sets the (i,j)(th) element of the array arr to
a. If i < 0, j < 0, nRows arr <= i or nCols arr <= j, then the
Subscript exception is raised.

[dimensions arr]
[nCols arr]
[nRows arr] These functions return size information concerning the
array arr. nCols returns the number of columns, nRows returns the
number of rows and dimension returns a pair containing the number of
rows and columns of the array. The functions nRows and nCols are
respectively equivalent to #1 o dimensions and #2 o dimensions

[row (arr, i)] returns row i of arr. It raises Subscript if i < 0 or
nRows arr <= i.

[column (arr, j)] returns column j of arr. It raises Subscript if j <
0 or nCols arr <= j.

[copy {src, dst, dst_row, dst_col}] copies the region src into the
array dst, with the (#row src,#col src)(th) element being copied into
the destination array at position (dst_row,dst_col). If the source
region is not valid, then the Subscript exception is
raised. Similarly, if the derived destination region (the source
region src translated to (dst_row,dst_col)) is not valid in dst, then
the Subscript exception is raised.

Implementation note:

The copy function must correctly handle the case in which src and dst
are equal, and the source and destination regions overlap.

[appi tr f reg]
[app tr f arr] These functions apply the function f to the elements of
an array in the order specified by tr. The more general appi function
applies f to the elements of the region reg and supplies both the
element and the element's coordinates in the base array to the
function f. If reg is not valid, then the exception Subscript is
raised.  The function app applies f to the whole array and does not
supply the element's coordinates to f. Thus the expression app tr f
arr is equivalent to:

  appi tr (f o #3) (arr, {row=0,col=0,nrows=NONE,ncols=NONE})

[foldi tr f init reg]
[fold tr f init arr] These functions fold the function f over the
elements of an array arr, traversing the elements in tr order, and
using the value init as the initial value. The more general foldi
function applies f to the elements of the region reg and supplies both
the element and the element's coordinates in the base array to the
function f. If reg is not valid, then the exception Subscript is
raised.

The function fold applies f to the whole array and does not supply the
element's coordinates to f. Thus the expression fold tr f init arr is
equivalent to:

	  foldi tr (fn (_,_,a,b) => f (a,b)) init
            (arr, {row=0, col=0, nrows=NONE, ncols=NONE})

[modifyi tr f reg]
[modify tr f arr] These functions apply the function f to the elements
of an array in the order specified by tr, and replace each element
with the result of f. The more general modifyi function applies f to
the elements of the region reg and supplies both the element and the
element's coordinates in the base array to the function f. If reg is
not valid, then the exception Subscript is raised.

The function modify applies f to the whole array and does not supply
the element's coordinates to f. Thus the expression modify f arr is
equivalent to:

  modifyi (f o #3) (arr, {row=0,col=0,nrows=NONE,ncols=NONE})
*)
