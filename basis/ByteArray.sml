
functor ByteArray(structure Vector : MONO_VECTOR) 
  : MONO_ARRAY where type elem = Vector.elem
               where type Vector.vector = Vector.vector =

  (* The ByteArray functor should be applied twice - once
   * with structure Vector = CharVector and once with structure
   * Vector = Word8Vector. Then, by using opaque signature 
   * constraints it is possible to make CharArray.array 
   * distinguishable from Word8Array.array. *)


  struct

    structure Vector = Vector
    type elem = Vector.elem
    type vector = Vector.vector
    type array = vector ref

    (* Depends on the primitives: *)

    (* Notice, that characters and bytes are untagged inside a string *)
    (* (i.e., vector or array) but otherwise tagged.                  *)
    fun sub_unsafe (s:vector,i:int) : elem = prim ("subStringML", "subStringML", (s,i))
    fun alloc_vector_unsafe (i:int) : vector = prim("allocStringML", "allocStringProfilingML", i)
    fun update_unsafe(s:vector,i:int,c:elem) : unit = prim("updateStringML", "updateStringML", (s, i, c))
    fun size (s:vector): int = prim ("sizeStringML", "sizeStringML", s)

    (* Body *)

    val maxLen = Vector.maxLen

    fun array(n, e: elem) = 
      if n < 0 orelse n > maxLen then raise Size
      else let val v = alloc_vector_unsafe n
               fun loop i = if i < n then (update_unsafe(v,i,e); loop (i+1))
			    else ()
	   in loop 0; ref v
	   end

    fun fromList a = ref (Vector.fromList a)

    fun tabulate a = ref (Vector.tabulate a)

    fun length (ref v) = size v

    fun sub (ref v, i) = Vector.sub (v,i)

    fun update (ref v,i,e) = 
      if i < 0 orelse i >= size v then raise Subscript
      else update_unsafe(v,i,e)

    fun extract(ref v, i, iopt) = Vector.extract(v, i, iopt)

    fun copy {src=ref src0, si, len, dst=ref dst0, di} : unit = 
        let val n_dst = size dst0 
	    val n_src = size src0
	    val n = (case len of NONE => n_src - si | SOME n => n)
	in
	  if n<0 orelse si<0 orelse si+n > n_src
	         orelse di<0 orelse di+n > n_dst then raise Subscript
	  else if si < di then		(* copy from high to low *)
	         let fun hdilo j =
		           if j >= 0 then
			     (update_unsafe (dst0, di+j, sub_unsafe (src0, si+j)); hdilo (j-1))
			   else ()
		 in hdilo (n-1)
		 end
	       else                       (* si >= di, copy from low to high *)
		 let fun lo2hi j =
		           if j < n then
			     (update_unsafe (dst0, di+j, sub_unsafe (src0, si+j)); lo2hi (j+1))
			   else ()
		 in lo2hi 0
		 end
	end

    fun copyVec {src : vector, si, len, dst=ref dst0, di} : unit =    (* copyVec need not deal with the case *)
	let val n_dst = size dst0                                     (* where src and dst overlap. *)
	    val n_src = size src
	    val n = case len of NONE => n_src - si | SOME k => k
	in
	  if n<0 orelse si<0 orelse si+n > n_src
	         orelse di<0 orelse di+n > n_dst then raise Subscript
	  else
	    let fun lo2hi j =
	              if j < n then
			(update_unsafe (dst0, di+j, sub_unsafe (src, si+j)); lo2hi (j+1))
		      else ()
	    in lo2hi 0
	    end
	end

    fun appi f (ref v, i, iopt) = Vector.appi f (v, i, iopt)

    fun app f (ref v) = Vector.app f v

    fun foldli f a (ref v, i, iopt) = Vector.foldli f a (v, i, iopt)

    fun foldri f a (ref v, i, iopt) = Vector.foldri f a (v, i, iopt)

    fun foldl f a (ref v) = Vector.foldl f a v

    fun foldr f a (ref v) = Vector.foldr f a v
      
    fun sliceend (a, i, NONE) = 
          let val sz = length a
	  in if i<0 orelse i>sz then raise Subscript else sz
	  end
      | sliceend (a, i, SOME n) = 
          let val k = i+n
	  in if i<0 orelse n<0 orelse k>length a then raise Subscript else k
	  end

    fun modifyi f (slice as (ref v, i, _)) =
        let val stop = sliceend slice
	    fun lr j = if j < stop then (update_unsafe(v, j, f (j, sub_unsafe(v, j))); lr (j+1))
		       else ()
	in lr i
	end

    fun modify f (ref v) =
        let val n = size v
	    fun lr j = if j < n then (update_unsafe(v, j, f (sub_unsafe(v, j)));
				      lr (j+1))
		       else ()
	in lr 0
	end

  end

structure CharArray = ByteArray(structure Vector = CharVector)
structure Word8Array = ByteArray(structure Vector = Word8Vector)
