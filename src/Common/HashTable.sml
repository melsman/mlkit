(* File "HashTable.sml"                    *)
(* This module uses NJ-arrays.             *)
(* 09/10/1996, Niels Hallenberg            *)
(* Domain is int.                          *)

(*$HASH_TABLE: *)
signature HASH_TABLE =
  sig
    type '_a hash_table

    val mk_empty : int -> '_a hash_table
    val update : '_a hash_table * int * '_a -> unit
    val remove : '_a hash_table * int -> unit
    val lookup : '_a hash_table * int -> '_a option
    val size : '_a hash_table -> int
    val range : '_a hash_table -> '_a list
    val list : '_a hash_table -> (int * '_a) list
    val apply : ('_a -> unit) -> '_a hash_table -> unit
    val Apply : ((int * '_a) -> unit) -> '_a hash_table -> unit
    val fold : ('_a * 'b -> 'b) -> 'b -> '_a hash_table -> 'b
    val Fold : ((int * '_a) * 'b -> 'b) -> 'b -> '_a hash_table -> 'b

    type StringTree
    val layout_map : {start: string, eq: string, sep: string, finish: string} ->
                     (int -> StringTree) -> ('_a -> StringTree) -> '_a hash_table -> StringTree

    val pp_table_usage : '_a hash_table -> string
  end

(*$HashTable: HASH_TABLE PRETTYPRINT*)
functor HashTable(structure PP : PRETTYPRINT) : HASH_TABLE =
  struct

    structure List = Edlib.List
    structure Int = Edlib.Int

    type dom = int
    type 'a hash_table = int * ((dom*'a)list Array.array)
    type StringTree = PP.StringTree
    (*	  val table_size = 127*) (* bin: 11111111 *)
    (*	  val table_size = 63*)  (* bin: 111111 *)
    fun hash (key,size) = Word.toInt (Word.andb (Word.fromInt key,Word.fromInt size))
    fun mk_empty size = (size, Array.array(size+1, []))
    fun update ((size,table), key, value) = 
      let
	val hash_key = hash (key,size)
	fun insert [] = [(key,value)]
	  | insert (bucket as [(key',value')]) =
	  if key = key' then
	    [(key,value)]
	  else
	    (key,value) :: bucket
	  | insert ((key',value')::rest) =
	    if key = key' then
	      (key,value)::rest
	    else
	      (key',value')::(insert rest)
      in
	Array.update (table, hash_key, insert (Array.sub (table, hash_key)))
      end
    fun remove ((size,table), key) =
      let
	val hash_key = hash(key,size)
	fun remove' [] = []
	  | remove' (bucket as [(key',value')]) =
	  if key = key' then
	    []
	  else
	    bucket
	  | remove' ((key',value')::rest) =
	    if key = key' then
	      rest (* We use, that the same key is at max. one time in the bucket. *)
	    else
	      (key',value')::(remove' rest)
      in
	Array.update (table, hash_key, remove'(Array.sub (table, hash_key)))
      end
    fun lookup ((size,table), key) =
      let 
	fun find_first_key [] = NONE
	  | find_first_key ((key',value)::rest) = 
	  if key = key' then 
	    SOME value 
	  else 
	    find_first_key rest
      in 
	find_first_key(Array.sub(table,(hash (key,size))))
      end
    
    (* Return number of associations. *)
    fun size (table_size, table) =
      let
	fun size' (0,s) = s + (List.size(Array.sub(table,0)))
	  | size' (n,s) = size' ((n-1), s + (List.size(Array.sub(table,n))))
      in
	size' (table_size,0)
      end
    
    (* Range of hash table. Not sorted!*)
    fun range (table_size, table) =
      let
	fun range' (0,acc) = Array.sub(table,0) @ acc
	  | range' (n,acc) = range' ((n-1),(Array.sub(table,n) @ acc))
      in
	List.map (fn (key,value) => value) (range' (table_size,[]))
      end
    
    (* Domain and range of hash table. Not sorted!*)
    fun list (table_size, table) =
      let
	fun list' (0,acc) = Array.sub(table,0) @ acc
	  | list' (n,acc) = list' ((n-1),(Array.sub(table,n) @ acc))
      in
	list' (table_size,[])
      end
    
    fun apply (f:'_a -> unit) ((table_size,table) : '_a hash_table) : unit =
      let
	fun apply_bucket [] = ()
	  | apply_bucket [(key,value)] = f(value)
	  | apply_bucket ((key,value)::buckets) = (f value; apply_bucket buckets)
	fun apply' 0 = apply_bucket (Array.sub(table,0))
	  | apply' n = (apply_bucket(Array.sub(table,n));apply' (n-1))
      in
	apply' table_size
      end
    
    fun Apply (f:(dom * 'a) -> unit) ((table_size,table) : 'a hash_table) : unit =
      let
	fun apply_bucket [] = ()
	  | apply_bucket [b] = f(b)
	  | apply_bucket (b::buckets) = (f b; apply_bucket buckets)
	fun apply' 0 = apply_bucket (Array.sub(table,0))
	  | apply' n = (apply_bucket(Array.sub(table,n));apply' (n-1))
      in
	apply' table_size
      end
    
    fun fold (f:'_a * 'b -> 'b) (empty : 'b) ((table_size, table) : '_a hash_table) : 'b =
      let
	fun fold_bucket ([],acc) = acc
	  | fold_bucket ([(key,value)],acc) = f(value,acc)
	  | fold_bucket ((key,value)::buckets,acc) = fold_bucket (buckets,f(value,acc))
	fun fold' (0,acc) = fold_bucket (Array.sub(table,0), acc)
	  | fold' (n,acc) = fold' (n-1,fold_bucket(Array.sub(table,n), acc))
      in
	fold' (table_size,empty)
      end
    
    fun Fold (f:(dom * 'a) * 'b -> 'b) (empty : 'b) ((table_size, table) : 'a hash_table) : 'b =
      let
	fun Fold_bucket ([],acc) = acc
	  | Fold_bucket ([b],acc) = f(b,acc)
	  | Fold_bucket (b::buckets,acc) = Fold_bucket (buckets,f(b,acc))
	fun Fold' (0,acc) = Fold_bucket (Array.sub(table,0), acc)
	  | Fold' (n,acc) = Fold' (n-1,Fold_bucket(Array.sub(table,n), acc))
      in
	Fold' (table_size,empty)
      end
    
    fun layout_map {start, eq=equal, sep, finish} 
      layoutDom layoutRan table =
      PP.NODE {start=start,
	       finish=finish,
	       children=List.map (fn (d,r) => 
				  PP.NODE {start="",
					   finish="",
					   children=[layoutDom d, 
						     layoutRan r],
					   indent=3,
					   childsep=PP.RIGHT equal})
	       (list table),
	       indent=3,
	       childsep=PP.RIGHT sep}
      
    fun pp_table_usage (table_size, table) =
      let
	fun bucket ([],acc) = acc
	  | bucket (buckets,(no_of_associations,no_of_used_table_entries,longest_bucket,total_bucket_length)) =
	  let
	    val size_bucket = List.size buckets
	  in
	    (no_of_associations+size_bucket,
	     no_of_used_table_entries+1,
	     (if longest_bucket>size_bucket then
		longest_bucket
	      else
		size_bucket),
		total_bucket_length+size_bucket)
	  end
	fun table_usage' (0,acc) = bucket (Array.sub(table,0),acc)
	  | table_usage' (n,acc) = table_usage' (n-1,bucket (Array.sub(table,n),acc))
	val (no_of_associations,no_of_used_table_entries,longest_bucket,total_bucket_length) =
	  table_usage' (table_size,(0,0,0,0))
      in
	("Table size......................: " ^ (Int.string table_size) ^ "\n" ^
	 "No. of associations.............: " ^ (Int.string no_of_associations) ^ "\n" ^
	 "No. of used table entries.......: " ^ (Int.string no_of_used_table_entries) ^ "\n" ^
	 "No. of unused table entries.....: " ^ (Int.string (table_size+1-no_of_used_table_entries)) ^ "\n" ^
	 "Longest bucket..................: " ^ (Int.string (longest_bucket)) ^ "\n" ^
	 "Mean bucket length..............: " ^ (if no_of_used_table_entries <> 0 then
						   Real.toString (real(total_bucket_length)/real(no_of_used_table_entries))
						 else
						   "------") ^ "\n")
      end
    
  end (* functor HashTable *)


