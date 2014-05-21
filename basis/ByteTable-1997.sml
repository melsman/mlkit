
functor ByteTable(eqtype table
		  eqtype elem) =  

  (* The ByteTable functor should be applied four times:
   *
   *              table       elem
   * ------------------------------
   * CharVector   string      char
   * Word8Vector  string      word8
   * CharArray    chararray   char
   * Word8Array   chararray   word8
   *
   * Then, by using opaque signature constraints it is possible to
   * make CharVector.vector distinguishable from 
   * Word8Vector.vector. Notice that equality works different on
   * arrays and vectors; for vectors, equality means structural
   * equality, whereas for arrays, equality means pointer
   * equality. Things work out right because string supports 
   * structural equality (chars in the strings are compared),
   * whereas chararray supports pointer equality. *)

  struct 

    type vector = string
    type array = chararray
    type elem = elem

    fun sub_unsafe (t:table,i:int) : elem = prim ("__bytetable_sub", (t,i))
    fun sub_vector_unsafe (v:vector,i:int) : elem = prim ("__bytetable_sub", (v,i))
    fun alloc_table_unsafe (i:int) : table = prim("allocStringML", i)
    fun alloc_vector_unsafe (i:int) : vector = prim("allocStringML", i)
    fun update_unsafe(t:table,i:int,c:elem) : unit = prim("__bytetable_update", (t, i, c))
    fun update_vector_unsafe(v:vector,i:int,c:elem) : unit = prim("__bytetable_update", (v, i, c))
    fun fromList (es : elem list) : table = prim ("implodeCharsML", es)
    fun concat (vs : table list) : table = prim ("implodeStringML", vs)
    fun length (t:table): int = prim ("__bytetable_size", t)
    fun length_vector (v:vector): int = prim ("__bytetable_size", v)
    fun null() : elem = prim("id",0:int)

    (* Body *)
    fun explode (t:table) : elem list  =
      let fun h (j, res) = if j < 0 then res
			   else h (j-1, sub_unsafe (t, j) :: res)
      in h (length t - 1, nil)
      end

    val maxLen = 16777211;  		(* as MosML (= (2^22-1)*4-1, with 32 bit) *)

    fun sub(t : table, i) : elem = 
      if i < 0 orelse i >= length t then raise Subscript
      else sub_unsafe(t, i)

    fun update(t : table, i, e : elem) : unit = 
      if i < 0 orelse i >= length t then raise Subscript
      else update_unsafe(t, i, e)

    fun tabulate (i: int, f : int -> elem) : table = 
      if i < 0 orelse i > maxLen then raise Size
      else 
	let val t = alloc_table_unsafe i
	    fun loop j = if j < i then (update_unsafe(t,j,f j); loop (j+1))
			 else update_unsafe(t, j, null())
	in loop 0; t
	end

    fun array (n, e:elem) : table =
      if n > maxLen then raise Size
      else 
	let val t = alloc_table_unsafe n
	    fun loop j = if j < n then (update_unsafe(t,j,e); loop (j+1))
			 else update_unsafe(t, j, null())
	in loop 0; t
	end

    fun extract (tab, i, slicelen) =
      let val n = case slicelen of NONE => length tab - i | SOME n => n
	  val newvec = 
	    if i < 0 orelse n < 0 orelse n > maxLen 
	      orelse i > length tab orelse i+n > length tab then raise Subscript
	    else alloc_vector_unsafe n
	  fun blit (i1, i2) =
	    if i2 >= n then update_vector_unsafe(newvec, i2, null())
	    else let val e = sub_unsafe(tab, i1)
		 in update_vector_unsafe(newvec, i2, e);
		    blit (i1+1, i2+1)
		 end
      in blit(i,0); newvec 
      end

    fun foldl f e a =
      let val stop = length a
	  fun lr (j, res) = if j < stop then lr (j+1, f(sub_unsafe(a,j), res))
			    else res
      in lr (0, e)
      end

    fun foldr f e a =
      let fun rl (j, res) = if j >= 0 then rl (j-1, f(sub_unsafe(a,j), res))
			    else res
      in rl (length a - 1, e) 
      end

    fun app f a =
      let val stop = length a
	  fun lr j = if j < stop then (f(sub_unsafe(a,j)); lr (j+1))
		     else ()
      in lr 0 
      end

    fun map (f : elem -> elem) (a : table) : table =
      let val stop = length a
	  val newtab = alloc_table_unsafe stop
	  fun lr j = if j < stop then (update_unsafe(newtab, j, f(sub_unsafe(a,j)));
				       lr (j+1))
		     else update_unsafe(newtab, j, null())
      in lr 0; newtab
      end

    fun sliceend (a, i, NONE) = if i < 0 orelse i > length a then raise Subscript
				else length a
      | sliceend (a, i, SOME n) = if i < 0 orelse n < 0 orelse i+n > length a then raise Subscript
				  else i+n

    fun foldli f e (slice as (a, i, _)) =
      let fun loop stop =
	    let fun lr(j, res) = if j < stop then lr (j+1, f(j, sub_unsafe(a,j), res))
				 else res
	    in lr (i, e) 
	    end
      in loop (sliceend slice) 
      end

    fun foldri f e (slice as (a, i, _)) =
      let fun loop start =
	    let fun rl(j, res) =
		    if j >= i then rl (j-1, f(j, sub_unsafe(a,j), res))
		    else res
	    in rl (start, e) 
	    end
      in loop (sliceend slice - 1) 
      end

    fun appi f (slice as (a, i, _)) =
      let fun loop stop =
	    let	fun lr j =
		    if j < stop then (f(j, sub_unsafe(a,j)); lr (j+1))
		    else ()
	    in lr i 
	    end
      in loop (sliceend slice) 
      end
    
    fun mapi (f : int * elem -> elem) (slice as (a : table, i, _)) : table =
      let val stop = sliceend slice
	  val newtab = alloc_table_unsafe (stop - i)
	  fun loop stop =
	    let	fun lr j =
		    if j < stop then
			(update_unsafe (newtab, j-i, f(j, sub_unsafe(a,j)));
			 lr (j+1))
		    else update_unsafe (newtab, j-i, null())
	    in lr i 
	    end
      in loop stop; newtab
      end

    fun modifyi f (slice as (a, i, _)) =
      let val stop = sliceend slice
          fun lr j =
	    if j < stop then (update_unsafe (a, j, f (j, sub_unsafe (a, j))); lr (j+1))
	    else ()
      in lr i
      end

    fun modify f a =
      let val n = length a 
          fun lr j = 
	    if j < n then (update_unsafe (a, j, f (sub_unsafe (a, j))); lr (j+1))
	    else ()
      in lr 0
      end

    (* The source and destination array may be the same. We *)
    (* copy from high index to low index if si < di and     *)
    (* from low index to high index if di < si.             *)     
    fun copy {src, si, len, dst, di} =
      let 
	val n_dst = length dst
	val n_src = length src
	val n = (case len of NONE => length src - si | SOME n => n)
      in
	if n < 0 orelse si < 0 orelse si+n > n_src
	  orelse di < 0 orelse di+n > n_dst then 
	  raise Subscript
	else 
	  if si < di then	(* copy from high to low *)
	    let
	      fun hdilo j =
		if j >= 0 then 
		  (update_unsafe (dst, di+j, sub_unsafe (src, si+j)); hdilo (j-1))
		else ()
	    in 
	      hdilo (n-1)
	    end
	  else (* si >= di, copy from low to high *)
	    let 
	      fun lo2hi j =
		if j < n then
		  (update_unsafe (dst, di+j, sub_unsafe (src, si+j)); lo2hi (j+1))
		else ()
	    in 
	      lo2hi 0
	    end
      end

    (* src and dst are never the same because src is an vector 
     * and dst is an array *)
    fun copyVec {src : vector, si, len, dst : table, di} : unit =
      let 
	val n_dst = length dst
	val n_src = length_vector src
	val n = case len of NONE => n_src - si | SOME k => k
      in
	if n < 0 orelse si < 0 orelse si+n > n_src
	  orelse di < 0 orelse di+n > n_dst then 
	  raise Subscript
	else
	  let 
	    fun lo2hi j =
	      if j < n then
		(update_unsafe (dst, di+j, sub_vector_unsafe (src, si+j)); lo2hi (j+1))
	      else ()
	  in 
	    lo2hi 0
	  end
      end
  end

structure CharVector : MONO_VECTOR =   
  ByteTable(type elem = char  type table = string) 
  
structure Word8Vector : MONO_VECTOR =  
  ByteTable(type elem = word8 type table = string)

structure CharArray =    
  ByteTable(type elem = char  type table = chararray)

structure Word8Array =   
  ByteTable(type elem = word8 type table = chararray)

structure CharArray : MONO_ARRAY =
  struct
    structure Vector = CharVector
    open CharArray
  end

structure Word8Array : MONO_ARRAY =
  struct
    structure Vector = Word8Vector
    open Word8Array
  end
