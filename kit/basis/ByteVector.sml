
functor ByteVector(eqtype elem) : MONO_VECTOR =  

  (* The ByteVector functor should be applied twice - once
   * with type elem=char and once with type elem=word8. Then,
   * by using opaque signature constraints it is possible to
   * make CharVector.vector distinguishable from 
   * Word8Vector.vector. *)

  struct 

    (* Depends on List and on the primitives: *)

    type vector = string
    type elem = elem

    fun sub_unsafe (v:vector,i:int) : elem = prim ("subStringML", "subStringML", (v,i))
    fun alloc_vector_unsafe (i:int) : vector = prim("allocStringML", "allocStringProfilingML", i)
    fun update_unsafe(v:vector,i:int,c:elem) : unit = prim("updateStringML", "updateStringML", (v, i, c))
    fun fromList (es : elem list) : vector = prim ("implodeCharsML", "implodeCharsProfilingML", es)
    fun concat (vs : vector list) : vector = prim ("implodeStringML", "implodeStringProfilingML", vs)
    fun length (v:vector): int = prim ("sizeStringML", "sizeStringML", v)
    fun null() : elem = prim("id","id",0:int)

    (* Body *)
    fun explode s =
      let fun h (j, res) = if j < 0 then res
			   else h (j-1, sub_unsafe (s, j) :: res)
      in h (size s - 1, nil)
      end

    val maxLen = 16777211;  		(* as MosML (= (2^22-1)*4-1, with 32 bit) *)

    fun sub(s, i) = if i<0 orelse i >= size s then raise Subscript
		    else sub_unsafe(s, i)

    fun tabulate (i, f) : vector = 
      if i > maxLen then raise Size
      else 
	let val v = alloc_vector_unsafe i
	    fun loop j = if j < i then (update_unsafe(v,j,f j); loop (j+1))
			 else update_unsafe(v, j, null())
	in loop 0; v
	end

    fun extract (vec, i, slicelen) =
      let val n = case slicelen of NONE => length vec - i | SOME n => n
	  val newvec = if i<0 orelse n<0 orelse n>maxLen orelse i>length vec orelse i+n > length vec then raise Subscript
		       else alloc_vector_unsafe n
	  fun blit (i1, i2) =
	    if i2 >= n then update_unsafe(newvec, i2, null())
	    else let val ch = sub_unsafe(vec, i1)
		 in update_unsafe(newvec, i2, ch);
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

    fun map (f : elem -> elem) (a : vector) : vector =
      let val stop = length a
	  val newvec = alloc_vector_unsafe stop
	  fun lr j = if j < stop then (update_unsafe(newvec, j, f(sub_unsafe(a,j)));
				       lr (j+1))
		     else update_unsafe(newvec, j, null())
      in lr 0; newvec 
      end

    fun sliceend (a, i, NONE) = if i<0 orelse i>length a then raise Subscript
				else length a
      | sliceend (a, i, SOME n) = if i<0 orelse n<0 orelse i+n>length a then raise Subscript
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
  
    fun appi f a = 
      let val stop = length a 
	  fun lr j = 
	    if j < stop then (f(j, sub_unsafe(a,j)); lr (j+1)) 
	    else ()
      in lr 0 
      end

    fun mapi (f : int * elem -> elem) a : vector = 
      let val stop = length a
	  val newvec = alloc_vector_unsafe stop
	  fun lr j = 
	    if j < stop then 
		(update_unsafe(newvec,j,f(j, sub_unsafe(a,j))); 
		 lr (j+1)) 
	    else ()
      in lr 0; newvec 
      end

    fun find (p : elem -> bool) (a : vector) : elem option = 
      let val stop = length a
	fun lr j = 
	    if j < stop then 
		if p (sub_unsafe(a,j)) then SOME (sub_unsafe(a,j)) else lr (j+1)
	    else NONE
      in lr 0 
      end

    fun exists (p : elem -> bool) (a : vector) : bool = 
      let val stop = length a
	  fun lr j = j < stop andalso (p (sub_unsafe(a,j)) orelse lr (j+1))
      in lr 0 
      end

    fun all (p : elem -> bool) (a : vector) : bool = 
      let val stop = length a
	  fun lr j = j >= stop orelse (p (sub_unsafe(a,j)) andalso lr (j+1))
      in lr 0 
      end

    fun findi (p : int * elem -> bool) (a : vector) : (int * elem) option = 
      let val stop = length a
	  fun lr j = 
	    if j < stop then 
	      if p (j, sub_unsafe(a,j)) then SOME (j, sub_unsafe(a,j)) else lr (j+1)
	    else NONE
      in lr 0 
      end

  end

structure CharVector = ByteVector(type elem = char) 
structure Word8Vector = ByteVector(type elem = word8)


