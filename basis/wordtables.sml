(*tables.sml: implementation of Vector and Array *)

(* table is the basic table type with functions
     sub0: extract element from table
     update0: update element in table
     table0: construct new table
   The table type is defined in Runtime/Table.h.

   Parts of the implementation is from Peter Sestoft's implementation
   in Moscow ML.
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

  (* quite a few bits are available for the length in the tag field! *)
  val maxLen = Initial.wordtable_maxlen

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
  fun vectorv n = (check_size n; vector0 n)

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
      let fun init f (t, i) = if i >= n then ()
			      else (update0 (t, i, f i); init f (t, i+1))
          val t = table n
      in init f (t, 0)
       ; t
      end

  fun tabulatev (n, f : int -> 'a) =
    let fun init (t, f, i) = if i >= n then t
			     else (update_vector0 (t, i, f i); init (t, f, i+1))
    in init (vector0 n, f, 0)
    end

  fun vector a = tabulatev (length a, fn i => sub0(a,i))

  fun updatev (t, i, x) =
      (check_index(length t, i);
       tabulate (length t,fn j => if i=j then x else sub0(t,j)))

  fun copy {src=a1: 'a table, dst=a2: 'a table, di=i2} =
    let val n = length a1
    in
	if i2<0 orelse i2+n > length a2 then
	    raise Subscript
	else		(* copy from high to low *)
	    let fun hi2lo j =
		    if j >= 0 then
			(update0(a2, i2+j,sub0(a1,j)); hi2lo (j-1))
		    else a1
	    in hi2lo (n-1); ()
	    end
    end

  fun copyVec {src=v: 'a vector, dst=a: 'a table, di=i2} =
    let val n = length_vector v
    in
	if i2<0 orelse i2+n > length a then
	    raise Subscript
	else
	    let fun lo2hi j =
		    if j < n then
			(update0(a,i2+j,sub_vector0(v,j)); lo2hi (j+1))
		    else v
	    in lo2hi 0; ()
	    end
    end

  (* apply f on the elements from left to right. *)
  fun app f a =
    let val n = length a
        fun lr j =
	  if j < n then (f (sub0 (a, j)) : unit; lr (j+1))
	  else a
    in lr 0 ; ()
    end

  fun foldli f e a =
    let val stop = length a
	fun lr (j, res, a) =
	    if j < stop then lr (j+1, f(j, sub0(a,j), res), a)
	    else res
    in lr (0, e, a)
    end

  fun foldri f e a =
    let fun rl (j, res, a) =
	    if j >= 0 then rl (j-1, f(j, sub0(a,j), res), a)
	    else res
    in rl (length a - 1, e, a)
    end

  fun appi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (f(j, sub0(a,j)):unit; lr (j+1))
	    else a
    in lr 0; ()
    end

  fun mapi (f : int * 'a -> 'b) (a : 'a table) : 'b table =
    let val stop = length a
	val newvec = table0 stop
	fun lr j =
	    if j < stop then
		(update0(newvec, j, f(j, sub0(a,j)));
		 lr (j+1))
	    else a
    in lr 0; newvec
    end

  fun foldl f e a =
    let val n = length a
	fun lr (j, res, a) =
	  if j < n then lr (j+1, f (sub0 (a, j), res), a)
	  else res
    in lr (0, e, a)
    end

  fun foldr f e a =
    let val n = length a
        fun rl (j, res, a) =
	  if j >= 0 then rl (j-1, f (sub0 (a, j), res), a)
	  else res
    in rl (n-1, e, a)
    end

  fun modifyi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (update0(a,j,f(j, sub0(a,j))); lr (j+1))
	    else a
    in lr 0; ()
    end

  fun modify f a =
    let val n = length a
        fun lr j =
	  if j < n then (update0 (a, j, f (sub0 (a, j))); lr (j+1))
	  else a
    in lr 0; ()
    end

  (* The following are only for the Vector structure: *)
  fun map (f : 'a -> 'b) (a : 'a table) : 'b table =
    let val n = length a
        val b : 'b table = table n
	fun lr j =
	  if j < n then (update0 (b, j, f (sub0 (a, j))); lr (j+1))
	  else (a,b)
    in lr 0; b
    end

  fun mapi (f : int * 'a -> 'b) (a : 'a table) : 'b table =
    let val stop = length a
	val newvec = table0 stop
	fun lr j =
	    if j < stop then
		(update0(newvec, j, f(j, sub0(a,j)));
		 lr (j+1))
	    else (a,newvec)
    in lr 0; newvec
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
	      else v
	in copy 0; copyall (to+x_n, xs)
	end
    in copyall (0, vecs)
    end

  fun findi (p : int * 'a -> bool) (a : 'a table) : (int * 'a) option =
    let val stop = length a
	fun lr j =
	    if j < stop then
		if p (j, sub0(a,j)) then SOME (j, sub0(a,j)) else lr (j+1)
	    else NONE
    in lr 0
    end

  fun find (p : 'a -> bool) (a : 'a table) : 'a option =
    let val stop = length a
	fun lr j =
	    if j < stop then
		if p (sub0(a,j)) then SOME (sub0 (a,j))
		else lr (j+1)
	    else NONE
    in lr 0
    end

  fun exists (p : 'a -> bool) (a : 'a table) : bool =
    let val stop = length a
	fun lr (j,a) = j < stop andalso (p (sub0(a,j)) orelse lr (j+1,a))
    in lr (0,a)
    end

  fun all (p : 'a -> bool) (a : 'a table) : bool =
    let val stop = length a
	fun lr (j,a) = j >= stop orelse (p (sub0(a,j)) andalso lr (j+1,a))
    in lr (0,a)
    end

  fun 'a collate cmp (v1: 'a table, v2: 'a table) =
    let val n1 = length v1
	and n2 = length v2
	val stop = if n1 < n2 then n1 else n2
	fun h (j,v: 'a table) = (* At this point v1[0..j-1] = v2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub0(v1,j), sub0(v2,j)) of
		    EQUAL => h (j+1,v)
		  | res   => res
    in h (0,v1)
    end
end
