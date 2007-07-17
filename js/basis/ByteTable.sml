
functor ByteTable(eqtype table
		  eqtype elem
                  val array_to_table : chararray -> table
                  val sub_unsafe : table * int -> elem
                  val fromList : elem list -> table
                  val concat : table list -> table
                  val length : table -> int
                 ) =  

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
    type elem = elem

    (* Arrays, implemented as JavaScript Arrays with number elements *)
    type array = chararray
    fun sub_array_unsafe(a:array,i:int) : elem = prim ("word_sub0", (a,i))
    fun update_array_unsafe(a:array,i:int,c:elem) : unit = prim("word_update0", (a, i, c))
    fun alloc_array_unsafe (i:int) : array = prim("word_table0", i)
    fun length_array(a:array) : int = prim("table_size",a)

    fun sub_vector_unsafe (v:vector,i:int) : elem = prim ("__bytetable_sub", (v,i))

(*
    fun alloc_table_unsafe (i:int) : table = prim("allocStringML", i)
    fun alloc_vector_unsafe (i:int) : vector = prim("allocStringML", i)
    fun update_unsafe(t:table,i:int,c:elem) : unit = prim("__bytetable_update", (t, i, c))
    fun update_vector_unsafe(v:vector,i:int,c:elem) : unit = prim("__bytetable_update", (v, i, c))
*)

    val fromList = fromList
    val concat = concat
    val length = length                
    fun length_vector (v:vector): int = prim ("__bytetable_size", v)

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
(*
    fun update(t : table, i, e : elem) : unit = 
      if i < 0 orelse i >= length t then raise Subscript
      else update_unsafe(t, i, e)
*)
    fun tabulate (i: int, f : int -> elem) : table = 
      if i < 0 orelse i > maxLen then raise Size
      else
	let val t = alloc_array_unsafe i
	    fun loop j = if j < i then (update_array_unsafe(t,j,f j); loop (j+1))
			 else ()
	in loop 0; array_to_table t
	end

    fun array (n, e:elem) : array =
      if n > maxLen then raise Size
      else 
	let val t = alloc_array_unsafe n
	    fun loop j = if j < n then (update_array_unsafe(t,j,e); loop (j+1))
			 else ()
	in loop 0; t
	end

    fun update(t : array, i, e : elem) : unit = 
      if i < 0 orelse i >= length_array t then raise Subscript
      else update_array_unsafe(t, i, e)


    fun updatev(t : table, i, e : elem) : table = 
      if i < 0 orelse i >= length t then raise Subscript
      else tabulate (length t,fn j => if i=j then e else sub_unsafe(t,j))

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
        tabulate (length a, fn i => f(sub_unsafe(a,i)))

    fun sliceend (a, i, NONE) = if i < 0 orelse i > length a then raise Subscript
				else length a
      | sliceend (a, i, SOME n) = if i < 0 orelse n < 0 orelse i+n > length a then raise Subscript
				  else i+n

    fun foldli f e a =
      let val i = 0
	  fun loop stop =
	    let fun lr(j, res) = if j < stop then lr (j+1, f(j, sub_unsafe(a,j), res))
				 else res
	    in lr (i, e) 
	    end
      in loop (length a) 
      end

    fun foldri f e a =
      let val i = 0
	  fun loop start =
	    let fun rl(j, res) =
		    if j >= i then rl (j-1, f(j, sub_unsafe(a,j), res))
		    else res
	    in rl (start, e) 
	    end
      in loop (length a - 1) 
      end

    fun modifyi f a = 
      let val stop = length_array a
	  fun lr j = 
	    if j < stop then (update_array_unsafe(a,j,f(j, sub_array_unsafe(a,j))); lr (j+1))
	    else ()
      in lr 0 
      end

    fun modify f a =
      let val n = length_array a 
          fun lr j = 
	    if j < n then (update_array_unsafe (a, j, f (sub_array_unsafe (a, j))); lr (j+1))
	    else ()
      in lr 0
      end

    fun vector (a:array) : vector = prim("chararray_to_string", a)

    (* The source and destination array may be the same. We *)
    (* copy from high index to low index if si < di and     *)
    (* from low index to high index if di < si.             *)     
    fun copy {src:array, si, len, dst:array, di} =
      let 
	val n_dst = length_array dst
	val n_src = length_array src
	val n = (case len of NONE => length_array src - si | SOME n => n)
      in
	if n < 0 orelse si < 0 orelse si+n > n_src
	  orelse di < 0 orelse di+n > n_dst then 
	  raise Subscript
	else 
	  if si < di then	(* copy from high to low *)
	    let
	      fun hdilo j =
		if j >= 0 then 
		  (update_array_unsafe (dst, di+j, sub_array_unsafe (src, si+j)); hdilo (j-1))
		else ()
	    in 
	      hdilo (n-1)
	    end
	  else (* si >= di, copy from low to high *)
	    let 
	      fun lo2hi j =
		if j < n then
		  (update_array_unsafe (dst, di+j, sub_array_unsafe (src, si+j)); lo2hi (j+1))
		else ()
	    in 
	      lo2hi 0
	    end
      end
    val copy = fn {src,dst,di} => copy{src=src,dst=dst,di=di,si=0,len=NONE}

    (* src and dst are never the same because src is an vector 
     * and dst is an array *)
    fun copyVec {src:vector, si, len, dst:array, di} : unit =
      let 
	val n_dst = length_array dst
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
		(update_array_unsafe (dst, di+j, sub_vector_unsafe (src, si+j)); lo2hi (j+1))
	      else ()
	  in 
	    lo2hi 0
	  end
      end
    val copyVec = fn {src,dst,di} => copyVec{src=src,dst=dst,di=di,si=0,len=NONE}

    fun appi f a = 
      let val stop = length a 
	  fun lr j = 
	    if j < stop then (f(j, sub_unsafe(a,j)); lr (j+1)) 
	    else ()
      in lr 0 
      end

    fun mapi (f : int * elem -> elem) a : table = 
        tabulate(length a, fn j => f(j,sub_unsafe(a,j)))

    fun find (p : elem -> bool) (a : table) : elem option = 
      let val stop = length a
	fun lr j = 
	    if j < stop then 
		if p (sub_unsafe(a,j)) then SOME (sub_unsafe(a,j)) else lr (j+1)
	    else NONE
      in lr 0 
      end

    fun exists (p : elem -> bool) (a : table) : bool = 
      let val stop = length a
	  fun lr j = j < stop andalso (p (sub_unsafe(a,j)) orelse lr (j+1))
      in lr 0 
      end

    fun all (p : elem -> bool) (a : table) : bool = 
      let val stop = length a
	  fun lr j = j >= stop orelse (p (sub_unsafe(a,j)) andalso lr (j+1))
      in lr 0 
      end

    fun findi (p : int * elem -> bool) (a : table) : (int * elem) option = 
      let val stop = length a
	  fun lr j = 
	    if j < stop then 
	      if p (j, sub_unsafe(a,j)) then SOME (j, sub_unsafe(a,j)) else lr (j+1)
	    else NONE
      in lr 0 
      end

    fun collate cmp (a1, a2) =
      let val n1 = length a1 
	  and n2 = length a2
	  val stop = if n1 < n2 then n1 else n2
	  fun h j = (* At this point a1[0..j-1] = a2[0..j-1] *)
	      if j = stop then if      n1 < n2 then LESS
			       else if n1 > n2 then GREATER
 			       else                 EQUAL
	      else
		case cmp(sub_unsafe(a1,j), sub_unsafe(a2,j)) of
		    EQUAL => h (j+1)
		  | res   => res
      in h 0 
      end
  end

functor VectorArg(type elem) =
  struct
    type elem = elem
    type table = string
    fun array_to_table (a: chararray) : table = prim("chararray_to_string", a)
    fun sub_unsafe (t: table,i: int) : elem = prim("__bytetable_sub",(t,i))
    fun fromList (es : elem list) : table = prim ("implodeCharsML", es)
    fun concat (vs : table list) : table = prim ("implodeStringML", vs)
    fun length (t:table): int = prim ("__bytetable_size", t)
  end

structure CharVector 
    :> MONO_VECTOR where type elem = char 
		     and type vector = string =
    let structure V = ByteTable(VectorArg(type elem = char))
    in struct open V
	      val update = updatev
       end
    end

functor ArrayArg(type elem) = 
  struct
    type elem = elem
    type table = chararray
    fun array_to_table (a: chararray) : table = a
    fun sub_unsafe (t: table,i: int) : elem = prim("word_sub0",(t,i))
    fun fromList (es : elem list) : table = prim ("charsToCharArray", es)
    fun concat (vs : table list) : table = prim ("charArraysConcat", vs)
    fun length (t:table): int = prim ("table_size", t)
  end

structure CharArray :> MONO_ARRAY
  where type vector = CharVector.vector
  where type elem = char 
  where type array = chararray
  = ByteTable(ArrayArg(type elem = char))

local
    structure Word8Vector =
	let structure V = ByteTable(VectorArg(type elem = word8))
	in struct open V
	          val update = updatev
	   end
	end

    structure Word8Array = 
	ByteTable(ArrayArg(type elem = word8))

    structure Tmp =
	struct
	    structure Word8Array = Word8Array
	    structure Word8Vector = Word8Vector
	end
    : sig
	   structure Word8Vector : MONO_VECTOR where type elem = word8 
	   structure Word8Array : MONO_ARRAY where type elem = word8
	   sharing type Word8Vector.vector = Word8Array.vector
       end
in
    open Tmp
end

