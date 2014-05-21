signature ARRAY2 =
  sig
    eqtype 'a array
    type 'a region = {
                       base : 'a array,
                       row : int,
                       col : int,
                       nrows : int option,
                       ncols : int option
                     }
    datatype traversal = RowMajor | ColMajor

    val array    : int * int * 'a -> 'a array
    val fromList : 'a list list -> 'a array
    val tabulate : traversal 
                   -> int * int * (int * int -> 'a) -> 'a array

    val sub : 'a array * int * int -> 'a
    val update : 'a array * int * int * 'a -> unit

    val dimensions : 'a array -> int * int
    val nCols      : 'a array -> int
    val nRows      : 'a array -> int

    val row    : 'a array * int -> 'a Vector.vector
    val column : 'a array * int -> 'a Vector.vector

    val copy : {
                   src : 'a region,
                   dst : 'a array,
                   dst_row : int,
                   dst_col : int
                 } -> unit

    val appi : traversal
                 -> (int * int * 'a -> unit)
                   -> 'a region -> unit
    val app  : traversal -> ('a -> unit) -> 'a array -> unit
    val foldi : traversal
                  -> (int * int * 'a * 'b -> 'b)
                    -> 'b -> 'a region -> 'b
    val fold  : traversal
                  -> ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val modifyi : traversal
                    -> (int * int * 'a -> 'a)
                      -> 'a region -> unit
    val modify  : traversal -> ('a -> 'a) -> 'a array -> unit 
end

(*
Description

type 'a region = {
                   base : 'a array,
                   row : int,
                   col : int,
                   nrows : int option,
                   ncols : int option
                 }

    This type specifies a rectangular subregion of a 2-dimensional
    array. If ncols equals SOME(w), with 0 <= w, the region includes
    only those elements in columns with indices in the range from col
    to col + (w - 1), inclusively. If ncols is NONE, the region
    includes only those elements lying on or to the right of column
    col. A similar interpretation holds for the row and nrows
    fields. Thus, the region corresponds to all those elements with
    position (i,j) such that i lies in the specified range of rows and
    j lies in the specified range of columns.

    A region reg is said to be valid if it denotes a legal subarray of
    its base array. More specifically, reg is valid if

        0 <= #row reg <= nRows (#base reg) 

    when #nrows reg = NONE, or

        0 <= #row reg <= (#row reg)+nr <= nRows (#base reg) 

    when #nrows reg = SOME(nr), and the analogous conditions hold for
    columns.

datatype traversal = RowMajor | ColMajor

    This type specifies a way of traversing a region. Specifically,
    RowMajor indicates that, given a region, the rows are traversed
    from left to right (smallest column index to largest column
    index), starting with the first row in the region, then the
    second, and so on until the last row is traversed. ColMajor
    reverses the roles of row and column, traversing the columns from
    top down (smallest row index to largest row index), starting with
    the first column, then the second, and so on until the last column
    is traversed.

array (r, c, init)

    creates a new array with r rows and c columns, with each element
    initialized to the value init. If r < 0, c < 0 or the resulting
    array would be too large, the Size exception is raised.

fromList l

    creates a new array from a list of a list of elements. The
    elements should be presented in row major form, i.e., hd l gives
    the first row, hd (tl l) gives the second row, etc. This raises
    the Size exception if the resulting array would be too large or if
    the lists in l do not all have the same length.

tabulate trv (r, c, f)

    creates a new array with r rows and c columns, with the (i,j)(th)
    element initialized to f (i,j). The elements are initialized in
    the traversal order specified by trv. If r < 0, c < 0 or the
    resulting array would be too large, the Size exception is raised.

sub (arr, i, j)

    returns the (i,j)(th) element of the array arr. If i < 0, j < 0,
    nRows arr <= i, or nCols arr <= j, then the Subscript exception is
    raised.

update (arr, i, j, a)
    sets the (i,j)(th) element of the array arr to a. If i < 0, j < 0, nRows arr <= i, or nCols arr <= j, then the Subscript exception is raised.

val dimensions : 'a array -> int * int
val nCols : 'a array -> int
val nRows : 'a array -> int

    These functions return size information concerning an array. nCols
    returns the number of columns, nRows returns the number of rows,
    and dimension returns a pair containing the number of rows and the
    number of columns of the array. The functions nRows and nCols are
    respectively equivalent to #1 o dimensions and #2 o dimensions

row (arr, i)

    returns row i of arr. If (nRows arr) <= i or i < 0, this raises
    Subscript.

column (arr, j)

    returns column j of arr. This raises Subscript if j < 0 or nCols
    arr <= j.

copy {src, dst, dst_row, dst_col}

    copies the region src into the array dst, with the element at
    position (#row src, #col src) copied into the destination array at
    position (dst_row,dst_col). If the source region is not valid,
    then the Subscript exception is raised. Similarly, if the derived
    destination region (the source region src translated to
    (dst_row,dst_col)) is not valid in dst, then the Subscript
    exception is raised.

        Implementation note:

        The copy function must correctly handle the case in which the
        #base src and the dst arrays are equal, and the source and
        destination regions overlap.

appi tr f reg
app tr f arr

    These functions apply the function f to the elements of an array
    in the order specified by tr. The more general appi function
    applies f to the elements of the region reg and supplies both the
    element and the element's coordinates in the base array to the
    function f. If reg is not valid, then the exception Subscript is
    raised.

    The function app applies f to the whole array and does not supply
    the element's coordinates to f. Thus, the expression app tr f arr
    is equivalent to:

       let
	 val range = {base=arr,row=0,col=0,nrows=NONE,ncols=NONE}
       in
	 appi tr (f o #3) range
       end

foldi tr f init reg
fold tr f init arr

    These functions fold the function f over the elements of an array
    arr, traversing the elements in tr order, and using the value init
    as the initial value. The more general foldi function applies f to
    the elements of the region reg and supplies both the element and
    the element's coordinates in the base array to the function f. If
    reg is not valid, then the exception Subscript is raised.

    The function fold applies f to the whole array and does not supply
    the element's coordinates to f. Thus, the expression fold tr f
    init arr is equivalent to:

       foldi tr (fn (_,_,a,b) => f (a,b)) init 
		  {base=arr, row=0, col=0, nrows=NONE, ncols=NONE}

modifyi tr f reg
modify tr f arr

    These functions apply the function f to the elements of an array
    in the order specified by tr, and replace each element with the
    result of f. The more general modifyi function applies f to the
    elements of the region reg and supplies both the element and the
    element's coordinates in the base array to the function f. If reg
    is not valid, then the exception Subscript is raised.

    The function modify applies f to the whole array and does not
    supply the element's coordinates to f. Thus, the expression modify
    tr f arr is equivalent to:

	let
	  val range = {base=arr,row=0,col=0,nrows=NONE,ncols=NONE}
	in
	  modifyi tr (f o #3) 
	end
*)