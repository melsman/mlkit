(* CharVectorSlice/Word8VectorSlice/CharArraySlice/Word8ArraySlice
 * - code partly from Moscow ML ; mael 2005-11-27 *)
functor ByteSlice (type table
		   type elem
		   type vector
		   type array
		   val maxLen : int) =
struct

    type vector = vector
    type elem = elem
    type array = array
    fun sub_unsafe (t:table,i:int) : elem = prim ("__bytetable_sub", (t,i))
    fun sub_vector_unsafe (v:vector,i:int) : elem = prim ("__bytetable_sub", (v,i))
    fun alloc_table_unsafe (i:int) : table = prim("allocStringML", i)
    fun alloc_vector_unsafe (i:int) : vector = prim("allocStringML", i)
    fun update_unsafe(t:table,i:int,c:elem) : unit = prim("__bytetable_update", (t, i, c))
    fun update_vector_unsafe(v:vector,i:int,c:elem) : unit = prim("__bytetable_update", (v, i, c))
    fun length0 (t:table): int = prim ("__bytetable_size", t)
    fun length_vector (v:vector): int = prim ("__bytetable_size", v)
    fun null() : elem = prim("id",0:int)

    type slice = table * int * int
    type vector_slice = vector * int * int

    fun length (a, i, n) = n

    fun sub((a', i', n'), i) =
      if i<0 orelse i >= n' then raise Subscript
      else sub_unsafe(a',i'+i)

    fun update ((a', i', n'), i, v) =
      if i<0 orelse i>=n' then raise Subscript
      else update_unsafe (a',i'+i,v);

    fun slice (a, i, len) =
      let val alen = length0 a
      in
	case len of
	    NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
		      else raise Subscript
	  | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
		      else raise Subscript
      end

    fun full a = (a, 0, length0 a);

    fun subslice ((a, i, n), i', NONE) =
      if 0<=i' andalso i'<=n then (a, i+i', n-i')
      else raise Subscript
      | subslice ((a, i, n), i', SOME n') =
      if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
      else raise Subscript

    fun base sli = sli

    fun vector (a : table, i, n) : vector =
      let val newvec = alloc_vector_unsafe n : vector
	  fun copy j =
	    if j<n then
		(update_vector_unsafe(newvec,j,sub_unsafe(a,i+j)); copy (j+1))
	    else
		update_vector_unsafe(newvec,j,null())
      in copy 0; newvec
      end

    fun copy {src=(a1,i1,n) : slice, dst=a2: table, di=i2} =
	if i2<0 orelse i2+n > length0 a2 then raise Subscript
	else if i1 < i2 then		(* copy from high to low *)
	         let fun hi2lo j =
		     if j >= 0 then
			 (update_unsafe(a2,i2+j,sub_unsafe(a1,i1+j)); hi2lo (j-1))
		     else ()
		 in hi2lo (n-1)
		 end
	     else                       (* i1 >= i2, copy from low to high *)
		 let fun lo2hi j =
		     if j < n then
			 (update_unsafe(a2,i2+j,sub_unsafe(a1,i1+j)); lo2hi (j+1))
		     else ()
		 in lo2hi 0
		 end

    fun copyVec {src : vector_slice, dst=a2: table, di=i2} =
      let val (a1, i1, n) = src
      in
	if i2<0 orelse i2+n > length0 a2 then raise Subscript
	else
	    let fun lo2hi j = if j < n then
		(update_unsafe(a2,i2+j,sub_vector_unsafe(a1,i1+j)); lo2hi (j+1))
			      else ()
	    in lo2hi 0
	    end
      end

    fun isEmpty (_, _, n) = n=0

    fun concat slis : vector =
      let fun acc [] len                 = len
	    | acc ((_, _, len1)::vr) len = acc vr (len1 + len)
	  val len = acc slis 0
	  val newvec = if len > maxLen then raise Size
		       else alloc_vector_unsafe len : vector
	  fun copyall to []                   = () (* Now: to = len *)
	    | copyall to ((v1, i1, n1)::slir) =
	    let fun copyv1 j =
		if j<n1 then
		    (update_vector_unsafe(newvec,to+j,sub_unsafe(v1,i1+j)); copyv1 (j+1))
		else
		    ()
	    in
		(copyv1 0; copyall (to+n1) slir)
	    end
      in copyall 0 slis;
	  update_vector_unsafe(newvec,len,null());
	  newvec
      end

    fun getItem (a, i, 0) = NONE
      | getItem (a, i, n) = SOME(sub_unsafe(a,i), (a, i+1, n-1));

    fun find (p : elem -> bool) ((a,i,n) : slice) : elem option =
      let val stop = i+n
	  fun lr j =
	    if j < stop then
		if p (sub_unsafe(a,j)) then SOME (sub_unsafe(a,j)) else lr (j+1)
	    else NONE
      in lr i
      end

    fun exists (p : elem -> bool) ((a,i,n) : slice) : bool =
      let val stop = i+n
	  fun lr j = j < stop andalso (p (sub_unsafe(a,j)) orelse lr (j+1))
      in lr i
      end

    fun all (p : elem -> bool) ((a,i,n) : slice) : bool =
      let val stop = i+n
	  fun lr j = j >= stop orelse (p (sub_unsafe(a,j)) andalso lr (j+1))
      in lr i
      end

    fun app f (a, i, n) =
      let val stop = i+n
	  fun lr j = if j < stop then (f(sub_unsafe(a,j)): unit; lr (j+1))
		     else ()
      in lr i
      end

    fun map (f : elem -> elem) (a : table, i, n) : table =
      let val newvec = alloc_table_unsafe n : table
	  val stop = i+n
	  fun lr j =
	    if j < stop then
		(update_unsafe(newvec,j-i,f(sub_unsafe(a,j))); lr (j+1))
	    else
		update_unsafe(newvec,j-i,null())
      in lr i; newvec
      end

    fun foldl f e (a, i, n) =
      let val stop = i+n
	  fun lr j res = if j < stop then lr (j+1) (f(sub_unsafe(a,j), res))
			 else res
      in lr i e
      end

    fun foldr f e (a, i, n) =
      let fun rl j res = if j >= i then rl (j-1) (f(sub_unsafe(a,j), res))
			 else res
      in rl (i+n-1) e
      end

    fun modify f (a, i, n) =
	let val stop = i+n
	    fun lr j = if j < stop then (update_unsafe(a,j,f(sub_unsafe(a,j))); lr (j+1))
		       else ()
	in lr i
	end

    fun findi (p : int * elem -> bool) ((a,i,n) : slice) : (int * elem) option =
      let val stop = i+n
	  fun lr j =
	    if j < stop then
		if p (j-i, sub_unsafe(a,j)) then SOME (j-i, sub_unsafe(a,j)) else lr (j+1)
	    else
		NONE
      in lr i
      end

    fun appi f (a, i, n) =
      let val stop = i+n
	  fun lr j =
	      if j < stop then (f(j-i, sub_unsafe(a,j)):unit; lr (j+1))
	      else ()
      in lr i
      end

    fun mapi (f : int * elem -> elem) (a : table, i, n) : table =
      let val newvec = alloc_table_unsafe n : table
	  val stop = i+n
	  fun lr j =
	      if j < stop then
		  (update_unsafe(newvec,j-i,f(j-i, sub_unsafe(a,j))); lr (j+1))
	      else
		  update_unsafe(newvec,j-i,null())
      in lr i; newvec
      end

    fun foldli f e (a, i, n) =
      let val stop = i+n
	  fun lr j res =
	      if j < stop then lr (j+1) (f(j-i, sub_unsafe(a,j), res))
	      else res
      in lr i e
      end

    fun foldri f e (a, i, n) =
      let fun rl j res =
	      if j >= i then rl (j-1) (f(j-i, sub_unsafe(a,j), res))
	      else res
      in rl (i+n-1) e
      end

    fun modifyi f (a, i, n) =
      let val stop = i+n
	  fun lr j =
	    if j < stop then (update_unsafe(a,j,f(j-i, sub_unsafe(a,j))); lr (j+1))
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
		  case cmp(sub_unsafe(a1,i1+j), sub_unsafe(a2,i2+j)) of
		      EQUAL => h (j+1)
		    | res   => res
      in h 0
      end
end

(** SigDoc *)
structure CharVectorSlice : MONO_VECTOR_SLICE =
    ByteSlice(type table  = CharVector.vector
	      type array  = CharArray.array
	      type vector = CharVector.vector
	      type elem   = char
	      val maxLen  = CharVector.maxLen)

(** SigDoc *)
structure Word8VectorSlice : MONO_VECTOR_SLICE =
    ByteSlice(type table  = Word8Vector.vector
	      type array  = Word8Array.array
	      type vector = Word8Vector.vector
	      type elem   = word8
	      val maxLen  = Word8Vector.maxLen)

(** SigDoc *)
structure CharArraySlice : MONO_ARRAY_SLICE =
    ByteSlice(type table  = CharArray.array
	      type array  = CharArray.array
	      type vector = CharVector.vector
	      type elem   = char
	      val maxLen  = CharArray.maxLen)

(** SigDoc *)
structure Word8ArraySlice : MONO_ARRAY_SLICE =
    ByteSlice(type table  = Word8Array.array
	      type array  = Word8Array.array
	      type vector = Word8Vector.vector
	      type elem   = word8
	      val maxLen  = Word8Array.maxLen)
