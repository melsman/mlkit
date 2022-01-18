(* VectorSlice and ArraySlice -- ported from Moscow ML, 2005-11-25 *)

functor TableSlice (type 'a table
		    val maxLen : int
		    type 'a vector_slice
		    val vector_slice_base: 'a vector_slice -> 'a vector * int * int) =
struct
  type 'a vector = 'a vector
  type 'a array = 'a array

  fun sub0 (a : 'a table, i : int) : 'a = prim ("word_sub0", (a, i))

  fun sub_vector0 (a : 'a vector, i : int) : 'a = prim ("word_sub0", (a, i))

  fun update0 (a : 'a table, i : int, x : 'a) : unit = prim ("word_update0", (a, i, x))

  fun update_vector0 (a : 'a vector, i : int, x : 'a) : unit = prim ("word_update0", (a, i, x))

  fun table0 (n : int) : 'a table = prim ("word_table0", n)

  fun vector0 (n : int) : 'a vector = prim ("word_table0", n)

  fun length0 (t : 'a table) : int = prim ("table_size", t)

  fun length_vector (t : 'a vector) : int = prim ("table_size", t)

  fun array0 (n : int, x:'a) : 'a array = prim ("word_table_init", (n, x))

  type 'a slice = 'a table * int * int
  type 'a vector_slice = 'a vector_slice

  fun length (a, i, n) = n

  fun sub((a', i', n'), i) =
    if i<0 orelse i >= n' then raise Subscript
    else sub0(a',i'+i)

  fun update ((a', i', n'), i, v)  =
      if i<0 orelse i>=n' then raise Subscript
      else update0 (a',i'+i,v)

  fun slice (a, i, len) =
    let val alen = length0 a
    in
	case len of
	    NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
		      else raise Subscript
	  | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
		      else raise Subscript
    end

  fun full a = (a, 0, length0 a)

  fun subslice ((a, i, n), i', NONE) =
    if 0<=i' andalso i'<=n then (a, i+i', n-i')
    else raise Subscript
    | subslice ((a, i, n), i', SOME n') =
    if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
    else raise Subscript

  fun base sli = sli

  fun vector (a : 'a table, i, n) =
    let val newvec = vector0 n : 'a vector
	fun copy j =
	    if j<n then
		(update_vector0(newvec, j, sub0(a,i+j)); copy (j+1))
	    else
		()
    in copy 0; newvec
    end

  fun copy {src=(a1,i1,n) : 'a slice, dst=a2: 'a table, di=i2} =
    if i2<0 orelse i2+n > length0 a2 then raise Subscript
    else if i1 < i2 then            (* copy from high to low *)
	   let fun hi2lo j =
	         if j >= 0 then
		     (update0(a2,i2+j,sub0(a1,i1+j)); hi2lo (j-1))
		 else ()
	   in hi2lo (n-1)
	   end
	 else                       (* i1 >= i2, copy from low to high *)
	   let fun lo2hi j =
	         if j < n then
		     (update0(a2,i2+j,sub0(a1,i1+j)); lo2hi (j+1))
		 else ()
	   in lo2hi 0
	   end

  fun copyVec {src : 'a vector_slice, dst=a2: 'a table, di=i2} =
    let val (a1, i1, n) = vector_slice_base src
    in
	if i2<0 orelse i2+n > length0 a2 then raise Subscript
	else
	    let fun lo2hi j = if j < n then
		  (update0(a2,i2+j,sub_vector0(a1,i1+j)); lo2hi (j+1))
			      else ()
	    in lo2hi 0
	    end
    end

  fun isEmpty (_, _, n) = n=0

  fun concat slis =
    let fun acc [] len                 = len
          | acc ((_, _, len1)::vr) len = acc vr (len1 + len)
        val len = acc slis 0
        val newvec = if len > maxLen then raise Size
		     else table0 len : 'a table
        fun copyall to []                   = () (* Now: to = len *)
          | copyall to ((v1, i1, n1)::slir) =
	    let fun copyv1 j =
		if j<n1 then
		    (update0(newvec,to+j, sub0(v1,i1+j)); copyv1 (j+1))
		else
		    ()
	    in
		(copyv1 0; copyall (to+n1) slir)
	    end
    in copyall 0 slis; newvec
    end

  fun getItem (a, i, 0) = NONE
    | getItem (a, i, n) = SOME(sub0(a, i), (a, i+1, n-1));

  fun find (p : 'a -> bool) ((a,i,n) : 'a slice) : 'a option =
    let val stop = i+n
	fun lr j =
	    if j < stop then
		if p (sub0(a,j)) then SOME (sub0(a,j)) else lr (j+1)
	    else NONE
    in lr i
    end

  fun exists (p : 'a -> bool) ((a,i,n) : 'a slice) : bool =
    let val stop = i+n
	fun lr j = j < stop andalso (p (sub0(a,j)) orelse lr (j+1))
    in lr i
    end

  fun all (p : 'a -> bool) ((a,i,n) : 'a slice) : bool =
    let val stop = i+n
	fun lr j = j >= stop orelse (p (sub0(a,j)) andalso lr (j+1))
    in lr i
    end

  fun app f (a, i, n) =
    let val stop = i+n
	fun lr j = if j < stop then (f(sub0(a,j)); lr (j+1))
		   else ()
    in lr i
    end

  fun map (f : 'a -> 'b) (a : 'a table, i, n) =
    let val newvec = table0 n : 'b table
	val stop = i+n
	fun lr j =
	    if j < stop then
		(update0(newvec,j-i,f(sub0(a,j))); lr (j+1))
	    else
		()
    in lr i; newvec
    end

  fun foldl f e (a, i, n) =
    let val stop = i+n
	fun lr j res = if j < stop then lr (j+1) (f(sub0(a,j), res))
		       else res
    in lr i e
    end

  fun foldr f e (a, i, n) =
    let fun rl j res = if j >= i then rl (j-1) (f(sub0(a,j), res))
		       else res
    in rl (i+n-1) e
    end

  fun modify f (a, i, n) =
    let val stop = i+n
	fun lr j = if j < stop then (update0(a,j,f(sub0(a,j))); lr (j+1))
		   else ()
    in lr i
    end

  fun findi (p : int * 'a -> bool) ((a,i,n) : 'a slice) : (int * 'a) option =
    let val stop = i+n
	fun lr j =
	    if j < stop then
		if p (j-i, sub0(a,j)) then SOME (j-i, sub0(a,j)) else lr (j+1)
	    else
		NONE
    in lr i
    end

  fun appi f (a, i, n) =
    let val stop = i+n
	fun lr j =
	    if j < stop then (f(j-i, sub0(a,j)); lr (j+1))
	    else ()
    in lr i
    end

  fun mapi (f : int * 'a -> 'b) (a : 'a table, i, n) =
    let val newvec = table0 n : 'b table
	val stop = i+n
	fun lr j =
	    if j < stop then
		(update0(newvec,j-i,f(j-i, sub0(a,j))); lr (j+1))
	    else
		()
    in lr i; newvec
    end

  fun foldli f e (a, i, n) =
    let val stop = i+n
	fun lr j res =
	    if j < stop then lr (j+1) (f(j-i, sub0(a,j), res))
	    else res
    in lr i e
    end

  fun foldri f e (a, i, n) =
    let fun rl j res =
	    if j >= i then rl (j-1) (f(j-i, sub0(a,j), res))
	    else res
    in rl (i+n-1) e
    end

  fun modifyi f (a, i, n) =
    let val stop = i+n
	fun lr j =
	    if j < stop then (update0(a,j,f(j-i, sub0(a,j))); lr (j+1))
	    else ()
    in lr i
    end

  fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
    let val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point a1[i1..i1+j-1] = a2[i2..i2+j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub0(a1,i1+j), sub0(a2,i2+j)) of
		    EQUAL => h (j+1)
		  | res   => res
    in h 0
    end
end
