(*tables.sml: implementation of Vector and Array *)

(* table is the basic table type with functions
     sub0: extract element from table
     update0: update element in table
     table0: construct new table
   The table type is defined in Runtime/Table.h.
*)

functor WordTable (eqtype 'a table) =
struct
  type 'a array  = 'a array
  type 'a vector = 'a vector

  fun sub0 (a : 'a table, i : int) : 'a = prim ("word_sub0", (a, i))

  fun sub_vector0 (a : 'a vector, i : int) : 'a = prim ("word_sub0", (a, i))

  fun update0 (a : 'a table, i : int, x : 'a) : unit = prim ("word_update0", (a, i, x))

  fun update_vector0 (a : 'a vector, i : int, x : 'a) : unit = prim ("word_update0", (a, i, x))

  fun table0 (n : int) : 'a table = prim ("word_table0", n)

  fun vector0 (n : int) : 'a vector = prim ("word_table0", n)

  fun length (t : 'a table) : int = prim ("table_size", t)

  fun length_vector (t : 'a vector) : int = prim ("table_size", t)

  fun array0 (n : int, x:'a) : 'a array = prim ("word_table_init", (n, x))    

  (* 26 bits are reserved for the length in the tag field; maxLen = 2^26 *)
  val maxLen = 123456789 (* arbitrary chosen. *)

  fun check_index (n, i) = 
    if 0 <= i andalso i < n then () 
    else raise Subscript

  fun check_size n = 
    if 0 <= n andalso n <= maxLen then () 
    else raise Size

  fun sub (t : 'a table, i:int) : 'a = 
    (check_index(length t, i); sub0(t,i))

  fun update (t, i, x) = 
    (check_index(length t, i); update0(t,i,x))

  (* table n returns an uninitialised table with n elements.
   * Raise Size if n > maxLen. *)
  fun table n = (check_size n; table0 n)

  (* vector n returns an uninitialised vector with n elements.
   * Raise Size if n > maxLen. *)
  fun vector n = (check_size n; vector0 n)

  (* array n gives an initialised array with n elements. *)
  (* Raise Size if n > maxLen.                           *)
  fun array (n, x) = (check_size n; array0 (n, x))
    
  (* fromList creates a table from a list of elements *)
  fun fromList (xs : 'a list) =
    let 
      fun init (t, [], i) = t
	| init (t, x::xs, i) = (update0 (t, i, x); init (t, xs, i+1))
      val n = List.length xs
    in 
      init (table n, xs, 0)
    end

  (* tabulate creates a table of n elements *)
  fun tabulate (n, f : int -> 'a) =
    let fun init (t, f, i) = if i >= n then t
			     else (update0 (t, i, f i); init (t, f, i+1))
    in init (table n, f, 0) 
    end

  (* is a table slice valid? *)
  fun check_slice slice = 
    let fun valid (a, i, SOME n) = 0 <= i andalso i <= i+n andalso i+n <= length a
	  | valid (a, i, NONE)   = 0 <= i andalso i <=                    length a
    in
      if valid slice then () else raise Subscript
    end

  (* extract always returns an vector *)
  fun extract (t : 'a table, i, n_opt) : 'a vector =
    let 
      val l = length t
      val n = (case n_opt of NONE => l - i | SOME n => n)
      val v : 'a vector = 
	if i < 0 orelse n < 0 orelse i+n > l then raise Subscript
	else vector0 n
      fun copy j =
	if j < n then (update_vector0 (v, j, sub0 (t, (i+j))); copy (j+1))
	else v
    in
      copy 0
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
		(update0 (dst, di+j, sub0 (src, si+j)); hdilo (j-1))
	      else ()
	  in 
	    hdilo (n-1)
	  end
	else (* si >= di, copy from low to high *)
	  let 
	    fun lo2hi j =
	      if j < n then
		(update0 (dst, di+j, sub0 (src, si+j)); lo2hi (j+1))
	      else ()
	  in 
	    lo2hi 0
	  end
    end

  (* src and dst are never the same because src is an vector 
   * and dst is an array *)
  fun copyVec {src : 'a vector, si, len, dst : 'a table, di} : unit =
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
	      (update0 (dst, di+j, sub_vector0 (src, si+j)); lo2hi (j+1))
	    else ()
	in 
	  lo2hi 0
	end
    end

  (* apply f on the elements from left to right. *)
  fun app f a =
    let val n = length a
        fun lr j = 
	  if j < n then (f (sub0 (a, j)); lr (j+1)) 
	  else ()
    in lr 0
    end

  (* sliceend returns the (index+1) of the last element in the slice *)
  fun sliceend (a, i, NONE) =
        if i < 0 orelse i > length a then raise Subscript else length a
    | sliceend (a, i, SOME n) =
	if i < 0 orelse n < 0 orelse i+n > length a then raise Subscript else i+n;

  fun appi f (slice as (a, i, _)) =
    let val stop = sliceend slice
        fun lr j = if j < stop then (f (j, sub0 (a, j)); lr (j+1)) 
		   else ()
    in lr i
    end

  fun foldli f e (slice as (a, i, _)) =
    let val stop = sliceend slice
        fun lr (j, res) =
	  if j < stop then lr (j+1, f (j, sub0 (a, j), res)) 
	  else res
    in lr (i, e)
    end

  fun foldri f e (slice as (a, i, _)) =
    let 
      fun rl (j, res) = 
	if j >= i then rl (j-1, f (j, sub0 (a, j), res))
	else res
    in rl (sliceend slice - 1, e)
    end

  fun foldl f e a =
    let val n = length a
	fun lr (j, res) = 
	  if j < n then lr (j+1, f (sub0 (a, j), res))
	  else res
    in lr (0, e)
    end

  fun foldr f e a =
    let val n = length a
        fun rl (j, res) = 
	  if j >= 0 then rl (j-1, f (sub0 (a, j), res))
	  else res
    in rl (n-1, e)
    end

  fun modifyi f (slice as (a, i, _)) =
    let val stop = sliceend slice
        fun lr j =
	  if j < stop then (update0 (a, j, f (j, sub0 (a, j))); lr (j+1))
	  else ()
    in lr i
    end

  fun modify f a =
    let val n = length a 
        fun lr j = 
	  if j < n then (update0 (a, j, f (sub0 (a, j))); lr (j+1))
	  else ()
    in lr 0
    end

  (* The following are only for the Vector structure: *)
  fun map (f : 'a -> 'b) (a : 'a table) : 'b table =
    let val n = length a
        val b : 'b table = table n
	fun lr j = 
	  if j < n then (update0 (b, j, f (sub0 (a, j))); lr (j+1))
	  else b
    in lr 0 
    end

  fun mapi (f : int * 'a -> 'b) (slice as (a : 'a table, i, _)) : 'b table =
    let val stop = sliceend slice
        val b : 'b table = table (stop - i)
	fun lr j =
	  if j < stop then (update0 (b, j-i, f (j, sub0 (a, j))); lr (j+1))
	  else b
    in lr i
    end

  fun concat (vecs : 'a table list) =
    let 
      fun total_length ([], n) = n
	| total_length (v::vs, n) = total_length (vs, length v + n);
      val n = total_length (vecs, 0)
      val v : 'a table = table n
      fun copyall (to, []) = v
	| copyall (to, x :: xs) =
	let val x_n = length x
	    fun copy j =
	      if j < x_n then (update0 (v, to+j, sub0 (x, j)); copy (j+1))
	      else ()
	in copy 0; copyall (to+x_n, xs)
	end
    in copyall (0, vecs)
    end

end; (*functor table*)
