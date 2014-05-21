(* Modified for Moscow ML from SML/NJ Library version 0.2
 *    -- modified for the ML Kit, 2001-06-07
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 * See file doc/license/copyright.att for details.
 *
 * Original author: John Reppy, AT&T Bell Laboratories, Murray Hill, NJ 07974
 *)

structure Polyhash : POLYHASH =           (* Requires Word, Array *)
  struct
    type ('key, 'data) bucket_t = (int * 'key * 'data) list

    type ('key, 'data) hash_table = 
	      {hashVal   : 'key -> int,
	       sameKey   : 'key * 'key -> bool,
	       not_found : exn,
	       table     : ('key, 'data) bucket_t Array.array ref,
	       n_items   : int ref}

    fun index (i, sz) = Word.toInt(Word.andb(Word.fromInt i, Word.fromInt (sz-1)))

    (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = 
      let val nw = Word.fromInt n
	fun f (i : word) = if (i >= nw) then i 
			   else f (Word.<< (i, 0w1))
      in Word.toInt(f 0w32)
      end

    (* Create a new table; the int is a size hint and the exception
     * is to be raised by find.
     *)
    fun mkTable (hashVal, sameKey) (sizeHint, notFound) = {
		hashVal=hashVal,
		sameKey=sameKey,
		not_found = notFound,
		table = ref (Array.array(roundUp sizeHint, nil)),
		n_items = ref 0
	      };

    fun forall (a,b) f =
        let fun loop i =
                if i > b then ()
                else (f i; loop(i+1))
        in loop a
        end

    fun clear ({table, n_items, ...} : ('key, 'data) hash_table) =
        let val a = !table 
        in forall (0, Array.length a - 1) (fn i => Array.update(a,i,nil));
           n_items := 0
        end

    (* conditionally grow a table *)
    fun growTable ({table, n_items, ...} : ('key, 'data) hash_table) = let
		val arr = !table
		val sz = Array.length arr
		in
		  if (!n_items >= sz)
		    then let
		      val newSz = sz+sz
		      val newArr = Array.array (newSz, nil)
		      fun copy nil = ()
			| copy ((h, key, v)::rest) = let
			    val indx = index (h, newSz)
			    in
			      Array.update (newArr, indx,
				(h, key, v) :: (Array.sub(newArr, indx)));
			      copy rest
			    end
		      fun bucket n = (copy (Array.sub(arr, n)); bucket (n+1))
		      in
			(bucket 0) handle _ => ();
			table := newArr
		      end
		    else ()
		end (* growTable *);

      (* Insert an item.  If the key already has an item associated with it,
       * then the old item is discarded.
       *)
	fun insert (tbl as {hashVal, sameKey, table, n_items, ...} : ('key, 'data) hash_table) (key, item) =
	    let
	      val arr = !table
	      val sz = Array.length arr
	      val hash = hashVal key
	      val indx = index (hash, sz)
	      fun look nil = (
		    Array.update(arr, indx, (hash, key, item) :: (Array.sub(arr, indx)));
		    n_items := !n_items + 1;
		    growTable tbl;
		    nil)
		| look ((h, k, v)::r) = if ((hash = h) andalso sameKey(key, k))
		    then (hash, key, item):: r
		    else (case (look r)
		       of nil => nil
			| rest => (h, k, v)::rest
		      (* end case *))
	      in
		case (look (Array.sub (arr, indx)))
		 of nil => ()
		  | b => Array.update(arr, indx, b)
	      end;

      (* Insert an item if not there already; if it is there already, 
	 then return the old data value and leave the table unmodified..
       *)
	fun peekInsert (tbl as {hashVal, sameKey, table, n_items, ...} : ('key, 'data) hash_table) (key, item) =
	    let val arr = !table
		val sz = Array.length arr
		val hash = hashVal key
		val indx = index (hash, sz)
		fun look nil = 
		    (Array.update(arr, indx, (hash, key, item) :: 
					       Array.sub(arr, indx));
		     n_items := !n_items + 1;
		     growTable tbl;
		     NONE)
		  | look ((h, k, v) :: r) = 
		    if hash = h andalso sameKey(key, k) then SOME v
		    else look r
	    in
		look (Array.sub (arr, indx))
	    end;

      (* find an item, the table's exception is raised if the item doesn't exist *)
	fun find ({hashVal, sameKey, table, not_found, ...} : ('key, 'data) hash_table) key = let
	      val arr = !table
	      val sz = Array.length arr
	      val hash = hashVal key
	      val indx = index (hash, sz)
	      fun look nil = raise not_found
		| look ((h, k, v) :: r) = if ((hash = h) andalso sameKey(key, k))
		    then v
		    else look r
	      in
		look (Array.sub (arr, indx))
	      end;

      (* look for an item, return NONE if the item doesn't exist *)
	fun peek ({hashVal, sameKey, table, ...} : ('key, 'data) hash_table) key = let
	      val arr = !table
	      val sz = Array.length arr
	      val hash = hashVal key
	      val indx = index (hash, sz)
	      fun look nil = NONE
		| look ((h, k, v) :: r) = if ((hash = h) andalso sameKey(key, k))
		    then SOME v
		    else look r
	      in
		look (Array.sub (arr, indx))
	      end;

      (* Remove an item.  The table's exception is raised if
       * the item doesn't exist.
       *)
	fun remove ({hashVal, sameKey, not_found, table, n_items, ...} : ('key, 'data) hash_table) key = let
	      val arr = !table
	      val sz = Array.length arr
	      val hash = hashVal key
	      val indx = index (hash, sz)
	      fun look nil = raise not_found
		| look ((h, k, v) :: r) = if ((hash = h) andalso sameKey(key, k))
		    then (v, r)
		    else let val (item, r') = look r in (item, (h, k, v) :: r') end
	      val (item, bucket) = look (Array.sub (arr, indx))
	      in
		Array.update (arr, indx, bucket);
		n_items := !n_items - 1;
		item
	      end (* remove *);

      (* Return the number of items in the table *)
       fun numItems ({n_items, ...} : ('key, 'data) hash_table) = !n_items

      (* return a list of the items in the table *)
	fun listItems ({table = ref arr, n_items, ...} : ('key, 'data) hash_table) = let
	      fun f (_, l, 0) = l
		| f (~1, l, _) = l
		| f (i, l, n) = let
		    fun g (nil, l, n) = f (i-1, l, n)
		      | g ((_, k, v) :: r, l, n) = g(r, (k, v)::l, n-1)
		    in
		      g (Array.sub(arr, i), l, n)
		    end
	      in
		f ((Array.length arr) - 1, [], !n_items)
	      end (* listItems *);

      (* Apply a function to the entries of the table *)
	fun apply f ({table, ...} : ('key, 'data) hash_table) = let
	      fun appF nil = ()
		| appF ((_, key, item) :: rest) = (
		    f (key, item);
		    appF rest)
	      val arr = !table
	      val sz = Array.length arr
	      fun appToTbl i = if (i < sz)
		    then (appF (Array.sub (arr, i)); appToTbl(i+1))
		    else ()
	      in
		appToTbl 0
	      end (* apply *);

      (* Map a table to a new table that has the same keys and exception *)
	fun map f ({hashVal, sameKey, table, n_items, not_found}) = let
	      fun mapF nil = nil
		| mapF ((hash, key, item) :: rest) =
		    (hash, key, f (key, item)) :: mapF rest
	      val arr = !table
	      val sz = Array.length arr
	      val newArr = Array.array (sz, nil)
	      fun mapTbl i = if (i < sz)
		    then (
		      Array.update(newArr, i, mapF (Array.sub(arr, i)));
		      mapTbl (i+1))
		    else ()
	      in
		mapTbl 0;
		{hashVal=hashVal,
		   sameKey=sameKey,
		   table = ref newArr, 
		   n_items = ref(!n_items), 
		   not_found = not_found}
	      end (* transform *);

      (* remove any hash table items that do not satisfy the given
       * predicate.
       *)
	fun filter pred ({table, n_items, not_found, ...} : ('key, 'data) hash_table) = let
	      fun filterP nil = nil
		| filterP ((hash, key, item) :: rest) = if (pred(key, item))
		    then (hash, key, item) :: filterP rest
		    else filterP rest
	      val arr = !table
	      val sz = Array.length arr
	      fun filterTbl i = if (i < sz)
		    then (
		      Array.update (arr, i, filterP (Array.sub (arr, i)));
		      filterTbl (i+1))
		    else ()
	      in
		filterTbl 0
	      end (* filter *);

      (* Map a table to a new table that has the same keys, exception,
	 hash function, and equality function *)

	fun transform f ({hashVal, sameKey, table, n_items, not_found}) = let
	      fun mapF nil = nil
		| mapF ((hash, key, item) :: rest) = (hash, key, f item) :: mapF rest
	      val arr = !table
	      val sz = Array.length arr
	      val newArr = Array.array (sz, nil)
	      fun mapTbl i = if (i < sz)
		    then (
		      Array.update(newArr, i, mapF (Array.sub(arr, i)));
		      mapTbl (i+1))
		    else ()
	      in
		mapTbl 0;
		{hashVal=hashVal, 
		   sameKey=sameKey, 
		   table = ref newArr, 
		   n_items = ref(!n_items), 
		   not_found = not_found}
	      end (* transform *);

      (* Create a copy of a hash table *)
	fun copy ({hashVal, sameKey, table, n_items, not_found}) = let
	      val arr = !table
	      val sz = Array.length arr
	      val newArr = Array.array (sz, nil)
	      fun mapTbl i = (
		    Array.update (newArr, i, Array.sub(arr, i));
		    mapTbl (i+1))
	      in
		(mapTbl 0) handle _ => ();
		{hashVal=hashVal, 
		   sameKey=sameKey,
		   table = ref newArr, 
		   n_items = ref(!n_items), 
		   not_found = not_found}
	      end (* copy *);

      (* returns a list of the sizes of the various buckets.  This is to
       * allow users to gauge the quality of their hashing function.
       *)
	fun bucketSizes ({table = ref arr, ...} : ('key, 'data) hash_table) = let
	      fun len (nil, n) = n
		| len (_ :: r, n) = len(r, n+1)
	      fun f (~1, l) = l
		| f (i, l) = f (i-1, len (Array.sub (arr, i), 0) :: l)
	      in
		f ((Array.length arr)-1, [])
	      end

	(* Look for an item and return the number of values with the same hash *)
	fun peekSameHash ({hashVal, sameKey, table, ...} : ('key, 'data) hash_table) key = let
	      val arr = !table
	      val sz = Array.length arr
	      val hash = hashVal key
	      val indx = index (hash, sz)
	      fun count (nil,acc) = acc
		| count ((h,_,_)::r,acc) = if hash = h then count(r,acc+1)
					   else count(r,acc)
	      in
		  (count (Array.sub (arr, indx), 0), hash)
	      end

(*
    prim_val hash_param : int -> int -> 'a -> int = 3 "hash_univ_param";

    fun hash x = hash_param 50 500 x;

    fun mkPolyTable (sizeHint, notFound) = 
	 mkTable (hash, op=) (sizeHint, notFound);
*)
  end
