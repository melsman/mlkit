(* array2.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Two-dimensional arrays.
 *)

structure Array2 : ARRAY2 =
  struct
    datatype 'a array2 = A2 of {
	nrows : int,
	ncols : int,
	elems : 'a array
      }

    fun index (A2{nrows, ncols, ...}, i, j) =
	  if ((i < 0) orelse (nrows <= i) orelse (j < 0) orelse (ncols <= j))
	    then raise Subscript
	    else (i*ncols + j)

  (* array(n,m,x) creates an n*m array initialized to x.
   * Raises Size, if m or n is < 0.
   *)
    fun array (nRows, nCols, initVal) =
	  if (nCols < 0) orelse (nRows < 0)
	    then raise Size
	    else A2{
		nrows = nRows, ncols = nCols,
		elems = Array.array(nRows*nCols, initVal)
	      }

  (* tabulate(n,m,f) creates an n*m array, where the (i,j) element
   * is initialized to f(i,j).  Raises Size, if m or n is < 0.
   *)
    fun tabulate (nRows, nCols, f) =
	  if (nCols < 0) orelse (nRows < 0)
            then raise Size
	    else let
	      fun mkElems (i, j, elems) = if (j < nCols)
		      then mkElems (i, j+1, f(i,j) :: elems)
		    else let val i = i+1
		      in
			if (i < nRows)
			  then mkElems (i, 0, elems)
			  else Array.fromList(rev elems)
		      end
	      in
		A2{nrows = nRows, ncols = nCols, elems = mkElems(0, 0, [])}
	      end

  (* sub(a,i,j) returns the (i,j) element. Raises Subscript if i or j
   * is out of range.
   *)
    fun sub (arr as A2{elems, ...}, i, j) = Array.sub(elems, index(arr, i, j))

  (* update(a,i,j,x) sets the (i,j) element to x. Raises Subscript if
   * i or j is out of range.
   *)
    fun update (arr as A2{elems, ...}, i, j, x) =
	  Array.update(elems, index(arr, i, j), x)

  (* return the size of the array *)
    fun dimensions (A2{nrows, ncols, ...}) = (nrows, ncols)

  (* project a column of the array. *)
    fun column (arr as A2{elems, nrows, ncols, ...}, j) = let
	  val k = index(arr, 0, j)
	  in
	    Array.tabulate(nrows, fn n => Array.sub(elems, k+(n*ncols)))
	  end

  (* project a row of the array. *)
    fun row (arr as A2{elems, ncols, ...}, i) = let
	  val k = index(arr, i, 0)
	  in
	    Array.tabulate(ncols, fn n => Array.sub(elems, k+n))
	  end

  end (* Array2 *)

